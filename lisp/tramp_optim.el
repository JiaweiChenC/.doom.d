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
  (add-to-list 'vterm-tramp-shells '("scpx" login-shell) t)
  (add-to-list 'vterm-tramp-shells '("rpc" login-shell) t)
  )

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;---------------------------------------------------------------------------
;; TRAMP connection guard — fail fast on unreachable hosts
;;---------------------------------------------------------------------------
;; TRAMP is fully synchronous: any file operation on a remote path
;; blocks the entire Emacs event loop during SSH connection setup.
;; This includes Vertico's minibuffer completion, which triggers
;; file-name-all-completions → TRAMP connect → 60+ second freeze.
;;
;; This advice wraps tramp-maybe-open-connection (the actual function
;; that initiates SSH).  Before letting TRAMP start its slow connection
;; process, we run a quick SSH probe with a 3-second timeout:
;;   - Host reachable  → mark verified, let TRAMP proceed
;;   - Host unreachable → signal error immediately (no 60s freeze)
;;   - Already connected or verified → skip check entirely

(defvar jc/tramp--verified-hosts (make-hash-table :test 'equal)
  "Hosts verified reachable this session.  Keyed by \"user@host\".")

(defun jc/tramp--host-key (vec)
  "Return a cache key for VEC's user@host."
  (let ((user (tramp-file-name-user vec))
        (host (tramp-file-name-host vec)))
    (if user (format "%s@%s" user host) host)))

(defun jc/tramp--ssh-method-p (method)
  "Return non-nil if METHOD is an SSH-based TRAMP method."
  (member method '("ssh" "scp" "scpx" "sshx" "rsync")))

(defun jc/tramp--cm-socket-alive-p (vec)
  "Return non-nil if an SSH ControlMaster socket is active for VEC."
  (let* ((user (or (tramp-file-name-user vec) (user-login-name)))
         (host (tramp-file-name-host vec))
         (socket (expand-file-name
                  (format "~/.ssh/sockets/%s@%s:22" user host))))
    (file-exists-p socket)))

(defadvice! jc/tramp-connection-guard-a (fn vec)
  "Pre-check SSH reachability before TRAMP tries to connect.
If the host is unreachable, fail immediately instead of freezing
Emacs for 60+ seconds."
  :around #'tramp-maybe-open-connection
  (let ((method (tramp-file-name-method vec))
        (key (jc/tramp--host-key vec)))
    (cond
     ;; Already connected — proceed immediately
     ((process-live-p (tramp-get-connection-process vec))
      (funcall fn vec))
     ;; Non-SSH method or already verified — proceed
     ((or (not (jc/tramp--ssh-method-p method))
          (gethash key jc/tramp--verified-hosts))
      (funcall fn vec))
     ;; ControlMaster socket exists — host is reachable, skip check
     ((jc/tramp--cm-socket-alive-p vec)
      (puthash key t jc/tramp--verified-hosts)
      (funcall fn vec))
     ;; Unknown host — quick SSH probe (3s max)
     (t
      (message "Checking SSH to %s..." key)
      (let ((exit (call-process "ssh" nil nil nil
                                "-o" "BatchMode=yes"
                                "-o" "ConnectTimeout=3"
                                key "echo" "ok")))
        (if (= exit 0)
            (progn
              (puthash key t jc/tramp--verified-hosts)
              (funcall fn vec))
          (user-error "Cannot reach %s (SSH exit %d)" key exit)))))))
