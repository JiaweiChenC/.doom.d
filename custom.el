(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.5)
 '(custom-safe-themes
   '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "554ca5fb1eecd56f1af30610864ff50bf4b9058060af79cf4585c10b06092c0b" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "0d0936adf23bba16569c73876991168d0aed969d1e095c3f68d61f87dd7bab9a" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "bfccfb247a960bf15b95cd3dccc41ff87caaba93f064fcc724b646867f2f4766" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "4fdbed4aa8bcb199d7f6a643886bac51178d1705b9b354ef3dd82d4ec48072d2" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files '("/Users/jiawei/org/journal/20240415"))
 '(package-selected-packages '(svg-lib))
 '(safe-local-variable-values
   '((eval progn
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
      (setq compilation-buffer-name-function 'file-compilation-buffer-name))
     (eval progn
      (map! :map org-mode-map :localleader "e T" #'my/org-to-tex-body-only))
     (eval defun my-org-to-tex-body-only nil
      (interactive)
      (org-latex-export-to-latex nil nil nil nil
                                 '(:body-only t)))
     (eval set
      (make-local-variable 'compile-command)
      (format "python %s" buffer-file-name))
     (eval set
      (make-local-variable 'compile-command)
      (format "/Users/jiawei/opt/anaconda3/envs/aihw5/bin/python %s" buffer-file-name))))
 '(warning-suppress-log-types '(((org-element org-element-parser)) (emacs)))
 '(warning-suppress-types '((emacs) (defvaralias) (org-element-cache))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'magit-clean 'disabled nil)
