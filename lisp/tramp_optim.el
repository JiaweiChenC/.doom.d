;;; tramp_optim.el --- TRAMP / project root optimizations -*- lexical-binding: t; -*-

;;; Commentary:
;; - Make TRAMP quieter/noisier via `tramp-verbose'.
;; - Avoid VC poking at TRAMP paths.
;; - Disable diff-hl on remote buffers.
;; - Provide a jc/project-root abstraction that:
;;   * Uses a host-local project root from dir-locals
;;   * Prepends the appropriate TRAMP prefix on remote buffers
;; - Integrate that root with Doom projects, Projectile, dumb-jump, and mini-echo.

;;; Code:

;;---------------------------------------------------------------------------
;; Basic TRAMP tweaks
;;---------------------------------------------------------------------------

(setq! tramp-verbose 2)

;; Don't let VC wander into TRAMP paths.
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;---------------------------------------------------------------------------
;; diff-hl: disable on remote
;;---------------------------------------------------------------------------

(after! diff-hl
  (defun jc/diff-hl-disable-on-remote ()
    "Disable diff-hl in remote TRAMP buffers."
    (when (file-remote-p default-directory)
      (diff-hl-mode -1)
      (diff-hl-flydiff-mode -1)))
  ;; In case something else turns it on, we turn it back off for remote.
  (add-hook 'find-file-hook #'jc/diff-hl-disable-on-remote))

;;---------------------------------------------------------------------------
;; Project root abstraction
;;---------------------------------------------------------------------------

(defvar-local jc/project-root-local nil
  "Host-local project root for this buffer, e.g. \"/home/jiawei/projects/SKEL/\".
No TRAMP prefix; that is reconstructed from `default-directory' when needed.")

(defun jc/project-root ()
  "Return full project root path for the current buffer.

If `jc/project-root-local' is set, combine it with the TRAMP prefix when
the buffer is remote; otherwise return it as-is.  Return nil if unset."
  (when (and (boundp 'jc/project-root-local)
             (stringp jc/project-root-local))
    (let ((prefix (file-remote-p default-directory)))
      (if prefix
          (concat prefix jc/project-root-local)
        jc/project-root-local))))

;;---------------------------------------------------------------------------
;; Multiple remote servers: dir-locals registration
;;---------------------------------------------------------------------------

;; (defvar jc/remote-project-specs
;;   '(("/scpx:100.105.242.51:"
;;      ;; "/home/jiawei/projects/GVHMR/"
;;      ;; "/home/jiawei/projects/SKEL/"
;;      ;; "/home/jiawei/projects/HSMR/"
;;      ))
;;   "List of remote project specs.

;; Each element is a list: (TRAMP-PREFIX ROOT1 ROOT2 ...), where ROOT*
;; are host-local directories on that server (no TRAMP prefix).")

;; (defun jc/register-remote-project-roots ()
;;   "Create dir-locals classes for all `jc/remote-project-specs'.

;; Each root is host-local; we build the full TRAMP directory using the
;; given prefix, but `jc/project-root-local' stays host-local."
;;   (dolist (spec jc/remote-project-specs)
;;     (let ((prefix (car spec))
;;           (roots  (cdr spec)))
;;       (dolist (root roots)
;;         (let* ((remote-root (concat prefix root))
;;                (class (intern (format "jc-remote-%s" (md5 remote-root))))
;;                (vars  `((nil . ((jc/project-root-local . ,root))))))
;;           (dir-locals-set-class-variables class vars)
;;           (dir-locals-set-directory-class remote-root class))))))

;; ;; Run once at startup; rerun if you change `jc/remote-project-specs`.
;; (jc/register-remote-project-roots)

;; Example to add another server later:
;;
;; (setq jc/remote-project-specs
;;       '(("/scpx:100.105.242.51:"
;;          "/home/jiawei/projects/GVHMR/"
;;          "/home/jiawei/projects/SKEL/"
;;          "/home/jiawei/projects/HSMR/")
;;         ("/scpx:other.host:"
;;          "/home/jiawei/projects/OTHER/")))
;; (jc/register-remote-project-roots)

;;---------------------------------------------------------------------------
;; Doom project integration
;;---------------------------------------------------------------------------

(defun jc/doom-project-root-from-dir-locals (orig-fun &optional dir &rest _)
  "Use `jc/project-root' for remote DIR when applicable; fall back to ORIG-FUN."
  (let* ((dir  (or dir default-directory))
         (root (jc/project-root)))
    (if (and root
             (file-remote-p dir)
             ;; Only override when DIR is really under our remote root.
             (string-prefix-p (file-name-as-directory root)
                              (file-name-as-directory dir)))
        root
      (funcall orig-fun dir))))

(advice-add #'doom-project-root :around #'jc/doom-project-root-from-dir-locals)

;;---------------------------------------------------------------------------
;; Projectile integration
;;---------------------------------------------------------------------------

(defun jc/projectile-project-root-remote (orig-fn &optional dir)
  "Use `jc/project-root' for remote buffers, fall back to ORIG-FN otherwise."
  (let* ((dir  (or dir default-directory))
         (root (jc/project-root)))
    (if (file-remote-p dir)
        ;; Remote: try our explicit root, fall back to DIR itself.
        (or root
            ;; Last-resort: treat current dir as root, no upward scan.
            (file-name-as-directory dir))
      ;; Local: normal Projectile behaviour.
      (funcall orig-fn dir))))

(advice-add #'projectile-project-root :around #'jc/projectile-project-root-remote)

;;---------------------------------------------------------------------------
;; dumb-jump integration
;;---------------------------------------------------------------------------

(after! dumb-jump
  (defun jc/dumb-jump-project-root-remote (orig-fn &rest args)
    "Avoid heavy project root detection on TRAMP.

On remote files, use `jc/project-root' if set, otherwise `default-directory'.
On local files, defer to ORIG-FN."
    (if (file-remote-p default-directory)
        ;; REMOTE: do NOT call ORIG-FN (which uses locate-dominating-file).
        (or (jc/project-root)
            (file-name-as-directory
             (directory-file-name default-directory)))
      ;; LOCAL: normal behavior.
      (apply orig-fn args)))
  (advice-add #'dumb-jump-get-project-root
              :around #'jc/dumb-jump-project-root-remote))

;;---------------------------------------------------------------------------
;; mini-echo integration
;;---------------------------------------------------------------------------

;; (defun jc/mini-echo-project-root ()
;;   "Project root for mini-echo, consistent with `jc/project-root' / Doom."
;;   (or
;;    ;; Explicit override for remote/local via jc/project-root-local.
;;    (jc/project-root)
;;    ;; Doom's project root (already advised to use jc/project-root on remote).
;;    (when (fboundp 'doom-project-root)
;;      (doom-project-root))
;;    ;; Last fallback: just use the current directory.
;;    (file-name-as-directory
;;     (directory-file-name default-directory))))

;; (setq mini-echo-project-detection #'jc/mini-echo-project-root)

;; add scpx login in to vterm tramp shells 
(after! vterm
  (add-to-list 'vterm-tramp-shells '("scpx" login-shell) t))
