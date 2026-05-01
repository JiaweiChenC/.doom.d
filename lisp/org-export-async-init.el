;;; lisp/org-export-async-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(defmacro map! (&rest _args)
  nil)

(unless (fboundp 'replace-citations)
  (defun replace-citations (s)
    s))

(let ((org-build-dir "/Users/jiawei/.emacs.d/.local/straight/build-31.0.50/org"))
  (when (file-directory-p org-build-dir)
    (add-to-list 'load-path org-build-dir)))

(require 'org)
(require 'ox-latex)

(load "/Users/jiawei/.doom.d/lisp/mytex.el" nil t)
