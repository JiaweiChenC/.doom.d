;;; lisp/tramp_optim.el -*- lexical-binding: t; -*-
(setq! tramp-verbose 2)

;; tramp trick 
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(after! diff-hl
  (defun jc/diff-hl-disable-on-remote ()
    (when (file-remote-p default-directory)
      (diff-hl-mode -1)
      (diff-hl-flydiff-mode -1)))

;;   ;; In case something else turns it on, we turn it back off for remote
  (add-hook 'find-file-hook #'jc/diff-hl-disable-on-remote))
;;; Configurable remote project roots via dir-locals
;;; ---------------------------------------------------------------------------

(defvar-local jc/project-root nil
  "Per-buffer override for Doom / project root (especially for TRAMP).")

(defvar jc/remote-project-roots
  '("/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/")
  "List of remote project root directories.

For each entry, we set up a dir-locals class that binds
`jc/project-root` and `projectile-project-root` to that path.")

(defun jc/register-remote-project-roots ()
  "Create dir-locals classes for `jc/remote-project-roots`."
  (dolist (root jc/remote-project-roots)
    ;; One class per root, so each can have its own value.
    (let* ((class (intern (format "jc-remote-%s" (md5 root))))
           (vars  `((nil . ((jc/project-root . ,root)
                            ;; (projectile-project-root . ,root)
                            )))))
      (dir-locals-set-class-variables class vars)
      (dir-locals-set-directory-class root class))))

;; Run once at startup; rerun if you change `jc/remote-project-roots`.
(jc/register-remote-project-roots)

;;; ---------------------------------------------------------------------------
;;; Doom project integration (unchanged logic)
;;; ---------------------------------------------------------------------------

(defun jc/doom-project-root-from-dir-locals (orig-fun &rest args)
  "If `jc/project-root` is set in this buffer, use it instead of auto-detection."
  (if (and (bound-and-true-p jc/project-root)
           (stringp jc/project-root))
      jc/project-root
    (apply orig-fun args)))

(advice-add #'doom-project-root :around #'jc/doom-project-root-from-dir-locals)

;;; ---------------------------------------------------------------------------
;;; Projectile integration (unchanged logic)
;;; ---------------------------------------------------------------------------

;; (with-eval-after-load 'projectile
(defun jc/projectile-project-root-remote-orig (orig-fn &optional dir)
    "Use `jc/project-root` for remote buffers, fall back to ORIG-FN otherwise."
    (let* ((dir (or dir default-directory)))
      (if (file-remote-p dir)
          ;; Remote: try our explicit root, fall back to DIR itself.
          (or jc/project-root
              ;; Last-resort: don't scan upward, just treat current dir as root.
              (file-name-as-directory dir))
        ;; Local: normal Projectile behaviour.
        (funcall orig-fn dir))))

  (advice-add 'projectile-project-root :around
              #'jc/projectile-project-root-remote-orig)
;; )

;;; ---------------------------------------------------------------------------
;;; dumb-jump integration (unchanged logic)
;;; ---------------------------------------------------------------------------

(after! dumb-jump
  (defun jc/dumb-jump-project-root-remote (orig-fn &rest args)
    "Avoid `locate-dominating-file` on TRAMP by not calling ORIG-FN there.
On remote files, use `jc/project-root` if set, otherwise `default-directory`."
    (if (file-remote-p default-directory)
        ;; REMOTE: do NOT call orig-fn (which uses locate-dominating-file).
        (or (and (boundp 'jc/project-root) jc/project-root)
            (file-name-as-directory
             (directory-file-name default-directory)))
      ;; LOCAL: normal behavior.
      (apply orig-fn args)))

  (advice-add 'dumb-jump-get-project-root
              :around #'jc/dumb-jump-project-root-remote))

(defun jc/mini-echo-project-root ()
  "Project root for mini-echo, consistent with jc/project-root / Doom."
  (or
   ;; Explicit override for remote etc.
   (and (boundp 'jc/project-root) jc/project-root)
   ;; Doom's project root (already advised to use jc/project-root on remote)
   (when (fboundp 'doom-project-root)
     (doom-project-root))
   ;; Last fallback
   (file-name-as-directory
    (directory-file-name default-directory))))

(setq mini-echo-project-detection #'jc/mini-echo-project-root)


(setq jc/remote-project-roots
      '("/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/"
        "/scpx:100.105.242.51:/home/jiawei/projects/SKEL/"
        "/scpx:100.105.242.51:/home/jiawei/projects/HSMR/"))

(jc/register-remote-project-roots)  ;; rerun after changing the list

