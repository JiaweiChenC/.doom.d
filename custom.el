(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/jiawei/opt/anaconda3/")
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((eval set
      (make-local-variable 'compile-command)
      (format "/Users/jiawei/opt/anaconda3/envs/aihw5/bin/python %s" buffer-file-name))
     (eval progn
      (defun file-compilation-buffer-name
          (compilation-mode)
        (concat "*"
                (downcase compilation-mode)
                "*"
                (if
                    (buffer-file-name)
                    (concat "<"
                            (file-name-nondirectory
                             (buffer-file-name))
                            ">")
                  "")))
      (setq compilation-buffer-name-function 'file-compilation-buffer-name)))))
(put 'customize-face 'disabled nil)
(put 'customize-group 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
