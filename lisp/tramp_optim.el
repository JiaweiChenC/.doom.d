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

(setopt tramp-verbose 2)

;; (defvar jc/vc-ignore-dir-regexp-base vc-ignore-dir-regexp
;;   "Baseline `vc-ignore-dir-regexp' without TRAMP override.")

;; (defun jc/vc-ignore-tramp-enable ()
;;   "Add TRAMP paths to `vc-ignore-dir-regexp'."
;;   (interactive)
;;   (setq vc-ignore-dir-regexp
;;         (format "\\(%s\\)\\|\\(%s\\)"
;;                 jc/vc-ignore-dir-regexp-base
;;                 tramp-file-name-regexp)))

;; (defun jc/vc-ignore-tramp-disable ()
;;   "Restore baseline `vc-ignore-dir-regexp' without TRAMP override."
;;   (interactive)
;;   (setq vc-ignore-dir-regexp jc/vc-ignore-dir-regexp-base))

;; (jc/vc-ignore-tramp-enable)

;; (after! projectile
;;   (defun jc/projectile-find-file-remote-vterm-a (orig-fn &rest args)
;;     "Use the preserved remote project root in vterm buffers."
;;     (if (and (derived-mode-p 'vterm-mode)
;;              (file-remote-p default-directory)
;;              (jc/project-root))
;;         (let ((default-directory (jc/project-root)))
;;           (projectile--find-file (car args)))
;;       (apply orig-fn args)))
;;   (advice-add #'projectile-find-file :around #'jc/projectile-find-file-remote-vterm-a))

(after! vterm
  (dolist (method '("ssh" "scp" "scpx" "rpc"))
    (setf (alist-get method vterm-tramp-shells nil nil #'equal)
          '("/usr/bin/zsh"))))

(after! vterm
  (defun jc/vterm-preserve-remote-prefix-a (orig path)
    "Keep the originating TRAMP prefix for remote vterm buffers."
    (or
     (when (and (boundp 'jc/vterm-origin-prefix)
                (stringp jc/vterm-origin-prefix)
                path
               (string-match "^[^@]+@[^:]+:\\(.*\\)$" path))
       (let ((dir (match-string 1 path)))
         (when (and dir (file-directory-p (concat jc/vterm-origin-prefix dir)))
           (file-name-as-directory (concat jc/vterm-origin-prefix dir)))))
     (funcall orig path)))
  (advice-add #'vterm--get-directory :around #'jc/vterm-preserve-remote-prefix-a))

(after! tramp
  ;; Avoid login-shell PATH probing on remote connections. It is noisy and
  ;; unnecessary for this setup because the common executable directories are
  ;; already listed explicitly.
  (setq tramp-remote-path
        (delete 'tramp-own-remote-path (copy-sequence tramp-remote-path))))

(after! tramp-rpc
  ;; The rpc backend has its own login-shell PATH probe. Disable it too so
  ;; remote vterm buffers do not trigger benign but repeated path warnings.
  (setq tramp-rpc-remote-path
        (delete 'tramp-rpc-own-remote-path (copy-sequence tramp-rpc-remote-path)))

  (defun jc/tramp-rpc-disable-undo-a (_vec _process buffer)
    "Disable undo in TRAMP RPC connection buffers."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-disable-undo))))

  (advice-add #'tramp-rpc--set-connection :after #'jc/tramp-rpc-disable-undo-a))

(after! diff-hl
  ;; Keep file-level vc-gutter enabled on remote buffers; we disable only the
  ;; Dired integration separately because that is the reproducible leak path.
  (setq diff-hl-disable-on-remote nil))

;; ;; enable delete via dired on remote files
(defun my/dired-remote-trash-only ()
  (when (and (derived-mode-p 'dired-mode)
             (file-remote-p default-directory))
    (setq-local delete-by-moving-to-trash 'nil)))
(add-hook 'dired-mode-hook #'my/dired-remote-trash-only)


;; (after! vterm
;;   (defun my/vterm-disable-evil ()
;;     "Use raw terminal input in `vterm' buffers."
;;     (setq-local evil-normal-state-cursor nil)
;;     (setq-local evil-insert-state-cursor nil)
;;     (setq-local evil-visual-state-cursor nil)
;;     (setq-local evil-motion-state-cursor nil)
;;     (setq-local evil-emacs-state-cursor nil)
;;     (when (bound-and-true-p evil-local-mode)
;;       (evil-emacs-state))
;;     (setq-local cursor-type nil))

;;   (defun my/vterm-fix-evil-cursor-jump ()
;;     (setq-local evil-move-cursor-back nil)
;;     (remove-hook 'window-configuration-change-hook #'evil-refresh-cursor t))

;;   (evil-set-initial-state 'vterm-mode 'emacs)
;;   (add-hook 'vterm-mode-hook #'my/vterm-disable-evil)
;;   )
