;;; persistent-cached-load-filter.el --- load path cache -*- lexical-binding: t; -*-

;; Copyright (C) 2025 include-yy

;; Author: include-yy <yy@egh0bww1.com>
;; Version: 0.1
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
;; TODOs:
;;
;; * The current implementation shadows packages with the same name
;;   appearing later in the load-path (similar to standard Emacs
;;   behavior).  Future versions might explore improvements to this
;;   logic.
;;
;; * The current cache implementation uses an alist.  This might become
;;   a performance bottleneck if the number of entries in load-path is
;;   very large.  Consider using other data structures such as plists,
;;   hash-tables, or avl-trees.

;;; Code:
(require 'pcase)
(require 'seq)
(require 'radix-tree)

(defconst t-filename "load-path-cache.eld"
  "Name of the cache file.")

(defconst t-cache-path (file-name-concat user-emacs-directory t-filename)
  "The absolute path to the cache file.")

(defun t--read-cache ()
  "Read and return the contents of the load-path cache file.
Return the Lisp object read from file.  Return nil if the file
does not exist or if an error occurs during reading."
  (when (file-exists-p t-cache-path)
    (ignore-errors
      (thread-first
        (with-temp-buffer
          (insert-file-contents t-cache-path)
          (buffer-string))
        read))))

(defvar t--cache (t--read-cache)
  "In-memory cache for `load-path' filtering.
This variable is an alist where each element is a cons cell of the
form (FILE . MATCHES).  FILE is the file name string being sought,
and MATCHES is a list of directories containing FILE.

The value is initialized by reading from the disk cache file when
this package is loaded.  This involves file I/O during startup.")

(defvar t--exist-path-table (make-hash-table)
  "Hash table for checking the existence of directories in `load-path'.

This hash table is used to memoize the results of directory existence
checks within the main filter function `persistent-cached-load-filter'.")

(defvar t--need-update nil
  "Non-nil means the cache has changed and needs to be written to disk.")

(defun t--path-included-p (path all-path)
  (seq-every-p
   (lambda (p)
     (or (gethash p t--exist-path-table)
         (and (member p all-path)
              (puthash p t t--exist-path-table))))
   path))

(defun persistent-cached-load-filter (path file suf)
  "Filter PATH for FILE with suffixes SUF using a persistent cache.

If FILE contains a directory component, return PATH unchanged.
Otherwise, look up FILE in cache.

If a cached value exists and contains only directories present in PATH,
return it. If the cache is invalid or no entry is found, delegate to the
default mechanism `load-path-filter-cache-directory-files'.

If a new search is performed, add it to the cache for future use."
  (if (file-name-directory file) path
    (let ((ls-cached (radix-tree-lookup t--cache file)))
      ;;(alist-get file t--cache nil nil #'equal)))
      (when (not (t--path-included-p ls-cached path))
        (setq t--need-update t)
        (setq t--cache (radix-tree-insert t--cache file nil))
        ;; (setq t--cache (seq-remove (lambda (x) (equal file (car x))) t--cache))
        (setq ls-cached nil))
      (or ls-cached
          (let ((ls (load-path-filter-cache-directory-files path file suf)))
            ;; It happens when suffixes has "", see comments of
            ;; `load-path-filter-cache-directory-files' in startup.el.
            (if (eq path ls) path
              (when ls
                (setq t--need-update t)
                (setq t--cache (radix-tree-insert t--cache file ls))
                ;; (push (cons file ls) t--cache)
                ls)))))))

(defun t--try-uniquify-cache ()
  "Remove duplicates and non-existent files from the cache.
Return the cleaned list.  Verify that the cached files actually
exist on disk using the current load suffixes."
  (let* ((suffixes (get-load-suffixes))
         (ht (make-hash-table :test #'equal))
         (alist nil))
    (radix-tree-iter-mappings
     t--cache
     (lambda (name paths)
       ;; Actually we use `locate-file' here, so the shadowed path will be ignored.
       (when-let* ((file (locate-file name paths suffixes))
                   (dir (directory-file-name (file-name-directory file)))
                   (unique-dir (with-memoization (gethash dir ht)
                                 (puthash dir dir ht))))
         (push (cons name unique-dir) alist))))
    (radix-tree-from-map alist)))
                              
(defun t-write-cache ()
  "Write the uniquified load path cache to disk.
Filter cache to remove duplicates and entries for files that
no longer exist, then write the result to
`persistent-cached-load-filter-cache-path'.

Do nothing if `persistent-cached-load-filter--need-update' is nil."
  (interactive)
  (when (and t--cache t--need-update)
    (let ((print-circle t))
      (with-temp-file t-cache-path
        (pp (t--try-uniquify-cache) (current-buffer))))))

(defun t-clear-cache ()
  "Clear the content of the persistent load path cache file.
This resets both the cache file on disk and the in-memory variable."
  (interactive)
  (setq t--cache nil)
  (setq t--need-update nil)
  (with-temp-file t-cache-path
    (insert (format "%S" radix-tree-empty))))

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
