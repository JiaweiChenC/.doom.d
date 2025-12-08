;;; lisp/tramp_optim.el -*- lexical-binding: t; -*-
(setq! tramp-verbose 2)

(after! diff-hl
  (defun jc/diff-hl-disable-on-remote ()
    (when (file-remote-p default-directory)
      (diff-hl-mode -1)
      (diff-hl-flydiff-mode -1)))

  ;; In case something else turns it on, we turn it back off for remote
  (add-hook 'find-file-hook #'jc/diff-hl-disable-on-remote))

;; A buffer-local override for project root
(defvar-local jc/project-root nil
  "Per-buffer override for Doom project root (especially for TRAMP).")

;; (after! find-file-in-project
(dir-locals-set-class-variables
'gvhmr-remote
'((nil . (
        ;; Explicit project root for this remote project
        (jc/project-root . "/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/")
        (projectile-project-root . "/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/")
        (jc/dumb-jump-project-root . "/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/")))))

(dir-locals-set-directory-class
"/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/"
'gvhmr-remote)

;; (after! doom-project
(defun jc/doom-project-root-from-dir-locals (orig-fun &rest args)
"If `jc/project-root` is set in this buffer, use it instead of auto-detection."
(if (and (bound-and-true-p jc/project-root)
        (stringp jc/project-root))
jc/project-root
(apply orig-fun args)))

(advice-add #'doom-project-root :around #'jc/doom-project-root-from-dir-locals)

;; (advice-add 'projectile-project-root :before-while
;;   (lambda (&optional dir)
;;     (not (file-remote-p (or dir default-directory)))))
(with-eval-after-load 'projectile
  (defun jc/projectile-project-root-remote-orig (orig-fn &optional dir)
    "Use `jc/project-root' for remote buffers, fall back to ORIG-FN otherwise."
    (let* ((dir (or dir default-directory)))
      (if (file-remote-p dir)
          ;; Remote: try our explicit root, fall back to DIR itself.
          (or jc/project-root
              ;; last-resort: don't scan upward, just treat current dir as root
              (file-name-as-directory dir))
        ;; Local: normal Projectile behaviour
        (funcall orig-fn dir))))

  (advice-add 'projectile-project-root :around
              #'jc/projectile-project-root-remote-orig))
