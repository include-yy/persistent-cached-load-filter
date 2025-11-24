;;; persistent-cached-load-filter.el --- load path cache -*- lexical-binding: t; -*-

;; Copyright (C) 2025 include-yy

;; Author: include-yy <yy@egh0bww1.com>
;; Version: 0.3
;; Package-Requires: ((emacs "31"))
;; Keywords: speedup, tools
;; URL: https://github.com/include-yy/persistent-cached-load-filter

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a persistent cache mechanism for `load-path'
;; lookups to speed up Emacs startup and library loading.
;;
;; It leverages the `load-path-filter-function' variable introduced in
;; Emacs 31.  By caching the directory locations of loaded libraries to
;; a file, it reduces the number of system calls (stat/access) required
;; when `require' or `load' is called, particularly beneficial for
;; systems with slow I/O or large `load-path' lists.
;;
;; To use this package, add the following to your init file:
;;
;;   (require 'persistent-cached-load-filter)
;;   (persistent-cached-load-filter-easy-setup)
;;
;; Which is equivalent to:
;;
;;   (require 'persistent-cached-load-filter)
;;   (when (boundp 'load-path-filter-function)
;;     (setq load-path-filter-function #'persistent-cached-load-filter)
;;     (add-hook 'kill-emacs-hook #'persistent-cached-load-filter-write-cache))
;;
;; To cache as many path lookups as possible, it is recommended to
;; place the configuration code as early as possible in your init file.
;; You can use the following form to prevent errors if the package is
;; not yet installed:
;;
;;   (when (require 'persistent-cached-load-filter nil t)
;;     (persistent-cached-load-filter-easy-setup))
;;
;; The cache is automatically saved to `load-path-cache.eld' in your
;; `user-emacs-directory' when Emacs exits.
;;
;; To write cache to file manually, use `persistent-cached-load-filter-write-cache'.
;; To clear cache, use `persistent-cached-load-filter-clear-cache'.
;;
;; NOTE ON BENCHMARKING:
;;
;; You can change `persistent-cached-load-filter-assoc-type' to
;; benchmark different data structures.  Note that if you change the
;; type, you MUST clear the existing cache file before restarting Emacs,
;; as the disk format changes depending on the chosen type.
;;
;; TODO:
;;
;; * The current implementation shadows packages with the same name
;;   appearing later in the load-path (similar to standard Emacs
;;   behavior).  Future versions might explore improvements to this
;;   logic.  Use `list-load-path-shadows' to check shadowed packages.

;;; Code:
(require 'pcase)
(require 'seq)
(require 'radix-tree)
(require 'map)

(defconst t-filename "load-path-cache.eld"
  "Name of the cache file.")

(defconst t-cache-path (file-name-concat user-emacs-directory t-filename)
  "The absolute path to the cache file.")

(defun t--read-cache ()
  "Read and return the contents of the load-path cache file.
Return the Lisp object read from file.  Return nil if the file
does not exist or if an error occurs during reading.

WARNING: If `persistent-cached-load-filter-assoc-type' has changed,
the object returned here may not match the expected type, causing
subsequent errors.  Clear the cache file if you change types."
  (when (file-exists-p t-cache-path)
    (ignore-errors
      (thread-first
        (with-temp-buffer
          (insert-file-contents t-cache-path)
          (buffer-string))
        read))))

(defvar t-assoc-type 'radix
  "The data structure used for the in-memory cache.

This variable must be set BEFORE loading the package, as the
internal accessor functions are statically defined via `defalias'
at load time to ensure zero-overhead access.

Valid values fall into three categories:

1. Radix Tree
   `radix'       : Use `radix-tree'.  Optimized for string keys
                   sharing common prefixes.

2. Hash Tables
   `hash+equal'  : Hash table with string keys.  O(1) lookup.
   `hash+eq'     : Hash table with interned symbol keys.

3. Linear Lists
   `alist+equal' : Association list with string keys.  O(N) lookup.
   `plist+equal' : Property list with string keys.
   `alist+eq'    : Association list with interned symbol keys.
   `plist+eq'    : Property list with interned symbol keys.

Note on `+eq' variants:
These options intern file name strings into symbols to use `eq' for
fast pointer comparison.")

(defun t--error-type ()
  (error "Unknown associative array type: %s" t-assoc-type))

(defalias 't--ini
  (pcase t-assoc-type
    ((or 'alist+eq 'alist+equal 'plist+eq 'plist+equal) (lambda () nil))
    ('hash+eq (lambda () (make-hash-table)))
    ('hash+equal (lambda () (make-hash-table :test #'equal)))
    ('radix (lambda () radix-tree-empty))
    (_ (t--error-type)))
  "Initialize an empty cache container for the current type.")

(defalias 't--elt
  (pcase t-assoc-type
    ('alist+eq (lambda (alist key) (alist-get (intern key) alist)))
    ('alist+equal (lambda (alist key) (alist-get key alist nil nil #'equal)))
    ('plist+eq (lambda (plist key) (plist-get plist (intern key))))
    ('plist+equal (lambda (plist key) (plist-get plist key #'equal)))
    ('hash+eq (lambda (ht key) (gethash (intern key) ht)))
    ('hash+equal (lambda (ht key) (gethash key ht)))
    ('radix (lambda (rt key) (radix-tree-lookup rt key)))
    (_ (t--error-type)))
  "Return the value associated with KEY in CONTAINER.
KEY is always a string.

\(fn CONTAINER KEY)")

(defalias 't--del
  (pcase t-assoc-type
    ('alist+eq
     (lambda (alist key)
       (seq-remove (lambda (x) (eq (car x) (intern key))) alist)))
    ('alist+equal
     (lambda (alist key)
       (seq-remove (lambda (x) (equal (car x) key)) alist)))
    ('plist+eq
     (lambda (plist key) (plist-put plist (intern key) nil)))
    ('plist+equal
     (lambda (plist key) (plist-put plist key nil #'equal)))
    ('hash+eq (lambda (ht key) (remhash (intern key) ht) ht))
    ('hash+equal (lambda (ht key) (remhash key ht) ht))
    ('radix (lambda (rt key) (radix-tree-insert rt key nil)))
    (_ (t--error-type)))
  "Remove KEY from CONTAINER and return the updated container.
KEY is always a string.

\(fn CONTAINER KEY)")

(defalias 't--put
  (pcase t-assoc-type
    ('alist+eq (lambda (alist key value) (push (cons (intern key) value) alist)))
    ('alist+equal (lambda (alist key value) (push (cons key value) alist)))
    ('plist+eq (lambda (plist key value) (plist-put plist (intern key) value)))
    ('plist+equal (lambda (plist key value) (plist-put plist key value #'equal)))
    ('hash+eq (lambda (ht key value) (puthash (intern key) value ht) ht))
    ('hash+equal (lambda (ht key value) (puthash key value ht) ht))
    ('radix (lambda (rt key value) (radix-tree-insert rt key value)))
    (_ (t--error-type)))
  "Associate KEY with VALUE in CONTAINER and return the updated container.
KEY is always a string.

\(fn CONTAINER KEY VALUE)")

(defalias 't--map
  (pcase t-assoc-type
    ((or 'alist+eq 'plist+eq 'hash+eq)
     (lambda (function eq-map)
       (map-do (lambda (k v) (funcall function (symbol-name k) v)) eq-map)))
    ((or 'alist+equal 'plist+equal 'hash+equal)
     (lambda (function equal-map) (map-do function equal-map)))
    ('radix (lambda (function rt) (radix-tree-iter-mappings rt function)))
    (_ (t--error-type)))
  "Iterate FUNCTION over CONTAINER.
FUNCTION is called with (KEY VALUE), where KEY is a string.

\(fn FUNCTION CONTAINER)")

(defalias 't--from-alist
  (let ((type (pcase t-assoc-type
                ((or 'alist+eq 'alist+equal) 'alist)
                ((or 'plist+eq 'plist+equal) 'plist)
                ((or 'hash+eq 'hash+equal) 'hash-table)
                ('radix 'radix)
                (_ (t--error-type)))))
    (pcase t-assoc-type
      ((or 'alist+eq 'plist+eq 'hash+eq)
       (lambda (alist)
         (map-into (mapcar (pcase-lambda (`(,k . ,v))
                             (cons (intern k) v))
                           alist)
                   (if (not (eq t-assoc-type 'hash+eq)) type
                     '(hash-table :test eq)))))
      ((or 'alist+equal 'plist+equal 'hash+equal)
       (lambda (alist) (map-into alist type)))
      ('radix (lambda (alist) (radix-tree-from-map alist)))
      (_ (t--error-type))))
  "Convert ALIST into a container of the current chosen type.")

(defvar t--cache (or (t--read-cache) (t--ini))
  "In-memory cache for `load-path' filtering.

The specific data structure depends on `persistent-cached-load-filter-assoc-type'.

The value is initialized by reading from the disk cache file when
this package is loaded.  This involves file I/O during startup.")

(defvar t--need-update nil
  "Non-nil means the cache has changed and needs to be written to disk.")

(defun persistent-cached-load-filter (path file suf)
  "Filter PATH for FILE with suffixes SUF using a persistent cache.

If FILE contains a directory component, return PATH unchanged.
Otherwise, look up FILE in cache.

If a cached value exists and contains only directories present in PATH,
return it.  If the cache is invalid or no entry is found, delegate to
the default mechanism `load-path-filter-cache-directory-files'.

If a new search is performed, add it to the cache for future use."
  (if (file-name-directory file) path
    (let ((ls-cached (t--elt t--cache file)))
      (when (not (seq-every-p (lambda (p) (member p path)) ls-cached))
        (setq t--need-update t)
        (setq t--cache (t--del t--cache file))
        (setq ls-cached nil))
      (or ls-cached
          (let ((ls (load-path-filter-cache-directory-files path file suf)))
            ;; It happens when suffixes has "", see comments of
            ;; `load-path-filter-cache-directory-files' in startup.el.
            (if (eq path ls) path
              (when ls
                (setq t--need-update t)
                (setq t--cache (t--put t--cache file ls))
                ls)))))))

(defun t--try-uniquify-cache ()
  "Remove duplicates and non-existent files from the cache.
Return the cleaned cache structure.

This function also performs string interning on directory paths to
reduce memory usage and disk footprint."
  (let* ((suffixes (get-load-suffixes))
         ;; Helper hash table for string interning
         (ht (make-hash-table :test #'equal))
         (alist nil))
    (t--map
     (lambda (name paths)
       ;; Actually we use `locate-file' here, so the shadowed path will be ignored.
       (when-let* ((file (locate-file name paths suffixes))
                   (dir (directory-file-name (file-name-directory file)))
                   ;; Intern the directory string.
                   (unique-dir (with-memoization (gethash dir ht)
                                 (puthash dir dir ht))))
         (push (cons name (list unique-dir)) alist)))
     t--cache)
    (t--from-alist alist)))

(defun t-write-cache ()
  "Write the uniquified load path cache to disk.
Filter cache to remove duplicates and entries for files that
no longer exist, then write the result to
`persistent-cached-load-filter-cache-path'.

Do nothing if `persistent-cached-load-filter--need-update' is nil."
  (interactive)
  (when t--need-update
    (let ((print-circle t))
      (with-temp-file t-cache-path
        (pp (t--try-uniquify-cache) (current-buffer))))))

(defun t-clear-cache ()
  "Clear the content of the persistent load path cache file.
This resets both the cache file on disk and the in-memory variable."
  (interactive)
  (setq t--cache (t--ini))
  (setq t--need-update nil)
  ;; Disable autosave when exiting Emacs to avoid writing additional
  ;; non-init time caching to disk.
  (remove-hook 'kill-emacs-hook #'t-write-cache)
  (with-temp-file t-cache-path
    (prin1 nil (current-buffer))))

(defun t-easy-setup ()
  "Configure the persistent load path cache.
Set `load-path-filter-function' to `persistent-cached-load-filter'
and add `persistent-cached-load-filter-write-cache' to
`kill-emacs-hook' to ensure the cache is saved on exit."
  (when (boundp 'load-path-filter-function)
    (setq load-path-filter-function #'persistent-cached-load-filter)
    (add-hook 'kill-emacs-hook #'t-write-cache)))

(provide 'persistent-cached-load-filter)
;; Local Variables:
;; read-symbol-shorthands: (("t-" . "persistent-cached-load-filter-"))
;; End:
