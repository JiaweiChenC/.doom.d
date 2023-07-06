(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/jiawei/opt/anaconda3/")
 '(custom-safe-themes
   '("bfccfb247a960bf15b95cd3dccc41ff87caaba93f064fcc724b646867f2f4766" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(org-babel-python-command "python" t)
 '(package-selected-packages
   '(eglot ox-reveal latex-preview-pane rg cdlatex nov srcery-theme jupyter))
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
;; (put 'customize-face 'disabled nil)
;; (put 'customize-group 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
