(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-top-caption-list '("table"))
 '(codeium/metadata/api_key "1678bb01-6663-4c05-a2c9-27ed39c052f0")
 '(custom-safe-themes
   '("9e296dbc86374778cca0f22cfd7cd44d35e7c2e678085417be97251ce7c75dcc"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "b0cbcb2fa0c69ab36f4505fec9967969b73327f1b8564f9afface8afd216bc93"
     "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032"
     "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "3d3eef7dd80e89aee44be1ff51d8285045615ea3239358a51818f4deaa5bd558"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a"
     "bfccfb247a960bf15b95cd3dccc41ff87caaba93f064fcc724b646867f2f4766"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files '("/Users/jiawei/org/journal/20240801"))
 '(package-selected-packages '(image+ kanagawa-theme zmq))
 '(safe-local-variable-values
   '((TeX-master . "./main.tex") (org-export-expand-links)
     (eval progn
      (defun file-compilation-buffer-name (compilation-mode)
        (concat "*" (downcase compilation-mode) "*"
                (if (buffer-file-name)
                    (concat "<" (file-name-nondirectory (buffer-file-name)) ">")
                  "")))
      (setq compilation-buffer-name-function 'file-compilation-buffer-name))
     (org-attach-directory . "./images/"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'magit-clean 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
