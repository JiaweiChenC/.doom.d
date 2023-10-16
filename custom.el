(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bfccfb247a960bf15b95cd3dccc41ff87caaba93f064fcc724b646867f2f4766" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "4fdbed4aa8bcb199d7f6a643886bac51178d1705b9b354ef3dd82d4ec48072d2" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/Users/jiawei/Projects/modern_control_projects/final.org" "/Users/jiawei/Projects/modern_control_projects/papers.org" "/Users/jiawei/Projects/modern_control_projects/project_proposal.org" "/Users/jiawei/Projects/modern_control_projects/proposal.org" "/Users/jiawei/Projects/TruST/TruST_Manual.org" "/Users/jiawei/Projects/TruST/TruST_Manual_Evaluation.org" "/Users/jiawei/Projects/TruST/changelog.org" "/Users/jiawei/Projects/TruST/todo.org" "/Users/jiawei/Projects/TruST/training_log.org" "/Users/jiawei/Projects/TruST/vicon.org" "/Users/jiawei/org/journal/2023-09-25.org" "/Users/jiawei/org/journal/2023-10-02.org" "/Users/jiawei/org/journal/20230911.org" "/Users/jiawei/org/journal/20230918.org" "/Users/jiawei/org/journal/2023-10-16"))
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
