(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-scrollbar nil)
 '(conda-anaconda-home "/Users/jiawei/opt/anaconda3/")
 '(custom-safe-themes
   '("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "ac18cc10455c6c26a98354ba8f9d338842d7ecc9ae3d28c205ed154ef20d74ce" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "bfccfb247a960bf15b95cd3dccc41ff87caaba93f064fcc724b646867f2f4766" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/Users/jiawei/Projects/modern_control_projects/final.org" "/Users/jiawei/Projects/modern_control_projects/papers.org" "/Users/jiawei/Projects/modern_control_projects/project_proposal.org" "/Users/jiawei/Projects/modern_control_projects/proposal.org" "/Users/jiawei/Projects/TruST/Dynamic_evaluation.org" "/Users/jiawei/Projects/TruST/TruST_Manual.org" "/Users/jiawei/Projects/TruST/TruST_Manual_Evaluation.org" "/Users/jiawei/Projects/TruST/training_log.org" "/Users/jiawei/Projects/TruST/vicon.org" "/Users/jiawei/org/journal/2023-09-25.org" "/Users/jiawei/org/journal/2023-10-02.org" "/Users/jiawei/org/journal/20230911.org" "/Users/jiawei/org/journal/20230918.org" "/Users/jiawei/org/journal/2023-10-02"))
 '(org-babel-python-command "python" t)
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow eglot ox-reveal latex-preview-pane rg cdlatex nov srcery-theme jupyter))
 '(python-shell-interpreter "python")
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
     (eval set
      (make-local-variable 'compile-command)
      (format "/Users/jiawei/opt/anaconda3/envs/aihw5/bin/python %s" buffer-file-name))
     (eval set
      (make-local-variable 'compile-command)
      (format "python3 %s" buffer-file-name))))
 '(screenshot-line-numbers-p nil))
(put 'customize-face 'disabled nil)
(put 'customize-group 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
