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

;; Don't let VC wander into TRAMP paths.
(defvar jc/vc-ignore-dir-regexp-base vc-ignore-dir-regexp
  "Baseline `vc-ignore-dir-regexp' without TRAMP override.")

(defun jc/vc-ignore-tramp-enable ()
  "Add TRAMP paths to `vc-ignore-dir-regexp'."
  (interactive)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                jc/vc-ignore-dir-regexp-base
                tramp-file-name-regexp)))

(defun jc/vc-ignore-tramp-disable ()
  "Restore baseline `vc-ignore-dir-regexp' without TRAMP override."
  (interactive)
  (setq vc-ignore-dir-regexp jc/vc-ignore-dir-regexp-base))

(jc/vc-ignore-tramp-enable)

;;---------------------------------------------------------------------------
;; Project root abstraction
;;---------------------------------------------------------------------------

(defvar-local jc/project-root-local nil
  "Host-local project root for this buffer, without any TRAMP prefix.")

(defun jc/project-root ()
  "Return the current buffer's explicit project root, if any."
  (when (stringp jc/project-root-local)
    (if-let ((prefix (or (and (boundp 'jc/vterm-origin-prefix)
                              (stringp jc/vterm-origin-prefix)
                              jc/vterm-origin-prefix)
                         (file-remote-p default-directory))))
        (concat prefix jc/project-root-local)
      jc/project-root-local)))

(defun jc/project-root-for-dir (&optional dir)
  "Return the explicit project root for DIR, including non-file buffers."
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (current-dir (and (stringp default-directory)
                           (file-name-as-directory default-directory))))
    (or (and current-dir
             (string-equal dir current-dir)
             (jc/project-root))
        (with-temp-buffer
          (setq default-directory dir)
          (ignore-errors
            (hack-dir-local-variables-non-file-buffer))
          (jc/project-root)))))

;;---------------------------------------------------------------------------
;; Doom project integration
;;---------------------------------------------------------------------------

(defun jc/doom-project-root-from-dir-locals (orig-fun &optional dir &rest _)
  "Use `jc/project-root' for remote DIR when applicable; fall back to ORIG-FUN."
  (let* ((dir (or dir default-directory))
         (root (jc/project-root-for-dir dir)))
    (if (and root
             (file-remote-p dir)
             (string-prefix-p (file-name-as-directory root)
                              (file-name-as-directory dir)))
        root
      (funcall orig-fun dir))))

(defun jc/project-root-integration-enable ()
  "Enable remote project root overrides for Doom and Projectile."
  (interactive)
  (unless (advice-member-p #'jc/doom-project-root-from-dir-locals
                           #'doom-project-root)
    (advice-add #'doom-project-root :around #'jc/doom-project-root-from-dir-locals))
  (unless (advice-member-p #'jc/projectile-project-root-remote
                           #'projectile-project-root)
    (advice-add #'projectile-project-root :around #'jc/projectile-project-root-remote)))

;; ;;---------------------------------------------------------------------------
;; ;; Projectile integration
;; ;;---------------------------------------------------------------------------

(defun jc/projectile-project-root-remote (orig-fn &optional dir)
  "Use `jc/project-root' for remote buffers; otherwise defer to ORIG-FN."
  (let* ((dir (or dir default-directory))
         (root (jc/project-root-for-dir dir)))
    (if (file-remote-p dir)
        (or root (funcall orig-fn dir))
      (funcall orig-fn dir))))

(jc/project-root-integration-enable)

(after! projectile
  (defun jc/projectile-find-file-remote-vterm-a (orig-fn &rest args)
    "Use the preserved remote project root in vterm buffers."
    (if (and (derived-mode-p 'vterm-mode)
             (file-remote-p default-directory)
             (jc/project-root))
        (let ((default-directory (jc/project-root)))
          (projectile--find-file (car args)))
      (apply orig-fn args)))
  (advice-add #'projectile-find-file :around #'jc/projectile-find-file-remote-vterm-a))

;;---------------------------------------------------------------------------
;; dumb-jump integration
;;---------------------------------------------------------------------------

(after! dumb-jump
  (defun jc/dumb-jump-project-root-remote (orig-fn &rest args)
    "Use `jc/project-root' on TRAMP when set.

On remote files, use `jc/project-root' if set, otherwise defer to ORIG-FN.
On local files, defer to ORIG-FN."
    (if (file-remote-p default-directory)
        ;; REMOTE: no current-directory fallback.
        (or (jc/project-root)
            (apply orig-fn args))
      ;; LOCAL: normal behavior.
      (apply orig-fn args)))
  (advice-add #'dumb-jump-get-project-root
              :around #'jc/dumb-jump-project-root-remote))

;; Avoid vterm's login-shell autodetection for custom TRAMP methods.
;; That code runs `getent passwd $LOGNAME` over TRAMP to discover the shell,
;; which can trigger `tramp-own-remote-path` lookups and fail noisily before
;; the connection is fully ready. Use the remote shell we already configured
;; for TRAMP instead.
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
        (delete 'tramp-rpc-own-remote-path (copy-sequence tramp-rpc-remote-path))))

;; ;; enable delete via dired on remote files
(defun my/dired-remote-trash-only ()
  (when (and (derived-mode-p 'dired-mode)
             (file-remote-p default-directory))
    (setq-local delete-by-moving-to-trash 'nil)))
(add-hook 'dired-mode-hook #'my/dired-remote-trash-only)


(after! vterm
  (defun my/vterm-normalize-point-for-command (pos)
    "Map POS onto a real command-buffer position for vterm operations."
    (let* ((prompt (or (ignore-errors (vterm--get-prompt-point)) (point-min)))
           (eol (or (ignore-errors (vterm--get-end-of-line pos)) pos))
           (pos (max prompt (min pos eol))))
      (if (get-text-property pos 'vterm-line-wrap)
          (min (1+ pos) eol)
        pos)))

  (defun my/vterm-wrap-between-p (a b)
    "Return non-nil if a wrapped fake newline lies between A and B."
    (let ((start (min a b))
          (end (max a b))
          found)
      (while (and (not found) (<= start end))
        (setq found (get-text-property start 'vterm-line-wrap)
              start (1+ start)))
      found))

  (defun my/vterm-remote-goto-char-safe (pos)
    "Move to POS one terminal step at a time for remote vterm buffers."
    (when (and vterm--term
               (file-remote-p default-directory)
               (vterm-cursor-in-command-buffer-p)
               (vterm-cursor-in-command-buffer-p pos))
      (let ((inhibit-redisplay t))
        (vterm-reset-cursor-point)
        (let ((steps-left (+ 8 (abs (- pos (point))))))
          (while (and (> steps-left 0)
                      (/= (point) pos))
            (vterm-send-key (if (< pos (point)) "<left>" "<right>") nil nil nil t)
            (setq steps-left (1- steps-left))))
        (= (point) pos))))

  (defun my/vterm-remote-insert ()
    "Enter insert state in remote vterm without visible cursor desync."
    (interactive)
    (let* ((target (my/vterm-normalize-point-for-command (point)))
           (cursor (or (ignore-errors (vterm--get-cursor-point)) target))
           (wrap-sensitive (or (get-text-property (max (point-min) (1- target)) 'vterm-line-wrap)
                               (get-text-property target 'vterm-line-wrap)
                               (my/vterm-wrap-between-p cursor target))))
      (let ((inhibit-redisplay t))
        (unless (= target cursor)
          (or (and wrap-sensitive
                   (my/vterm-remote-goto-char-safe target))
              (vterm-goto-char target)))
        (evil-insert-state))
      (goto-char (or (ignore-errors (vterm--get-cursor-point)) target))))

  (defun my/vterm-remote-evil-fixes ()
    "Apply targeted Evil fixes for remote vterm buffers."
    (when (and (file-remote-p default-directory)
               (bound-and-true-p evil-local-mode))
      (evil-local-set-key 'normal (kbd "i") #'my/vterm-remote-insert)))

  (defun my/vterm--remote-goto-char (pos)
    "Move remote vterm cursor to POS, waiting for PTY updates."
    (when (and vterm--term
               (file-remote-p default-directory)
               (vterm-cursor-in-command-buffer-p)
               (vterm-cursor-in-command-buffer-p pos))
      (let ((inhibit-redisplay t))
        (vterm-reset-cursor-point)
        (let ((proc (get-buffer-process (current-buffer)))
              (steps-left (+ 8 (abs (- pos (point))))))
          (while (and (> steps-left 0)
                      (/= (point) pos))
            (let* ((distance (abs (- pos (point))))
                   (key (if (< pos (point)) "<left>" "<right>"))
                   (burst (min 16 (max 1 distance))))
              (dotimes (_ burst)
                (vterm-send-key key))
              (when proc
                (accept-process-output proc 0.03)))
            (setq steps-left (1- steps-left)))
          (= (point) pos)))))

  (defun my/vterm-goto-char-around (orig pos)
    "Suppress display updates during remote vterm cursor sync to avoid jumping."
    (if (and vterm--term (file-remote-p default-directory))
        (let ((inhibit-redisplay t))
          (or (funcall orig pos)
              (my/vterm--remote-goto-char pos)))
      (funcall orig pos)))

  (defun my/vterm-delete-region-around (orig start end)
    "Batch remote deletes to reduce round trips in vterm."
    (if (and vterm--term
             (file-remote-p default-directory))
        (save-excursion
          (when (get-text-property start 'vterm-line-wrap)
            (setq start (1+ start)))
          (let ((count (length (filter-buffer-substring start end))))
            (if (vterm-goto-char start)
                (let ((proc (get-buffer-process (current-buffer))))
                  (while (> count 0)
                    (let ((burst (min 16 count)))
                      (dotimes (_ burst)
                        (vterm-send-key "<delete>"))
                      (when proc
                        (accept-process-output proc 0.03))
                      (setq count (- count burst)))))
              (let ((inhibit-read-only nil))
                (vterm--delete-region start end)))))
      (funcall orig start end)))

  (defun my/vterm-fix-evil-cursor-jump ()
    (setq-local evil-move-cursor-back nil)
    (remove-hook 'window-configuration-change-hook #'evil-refresh-cursor t))

  (add-hook 'vterm-mode-hook #'my/vterm-remote-evil-fixes)
  (advice-add 'vterm-goto-char :around #'my/vterm-goto-char-around)
  (advice-add 'vterm-delete-region :around #'my/vterm-delete-region-around)
  (add-hook 'vterm-mode-hook #'my/vterm-fix-evil-cursor-jump))
