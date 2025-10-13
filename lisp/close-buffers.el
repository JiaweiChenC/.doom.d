;;; lisp/close-buffers.el -*- lexical-binding: t; -*-

;; (defvar my/target-directory nil
;;   "The directory whose buffers should be closed when switching away.")

;; (defun my/close-directory-buffers ()
;;   "Close all buffers from `my/target-directory` when switching away."
;;   (unless (or (null my/target-directory)
;;               (eq (buffer-local-value 'default-directory (current-buffer))
;;                   my/target-directory))
;;     (dolist (buffer (buffer-list))
;;       (with-current-buffer buffer
;;         (when (string-prefix-p my/target-directory default-directory)
;;           (kill-buffer buffer))))))

;; (add-hook 'buffer-list-update-hook 'my/close-directory-buffers)
