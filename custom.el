(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/Users/jiawei/Projects/modern_control_projects/final.org" "/Users/jiawei/Projects/modern_control_projects/papers.org" "/Users/jiawei/Projects/modern_control_projects/project_proposal.org" "/Users/jiawei/Projects/modern_control_projects/proposal.org" "/Users/jiawei/Projects/TruST/Dynamic_evaluation.org" "/Users/jiawei/Projects/TruST/TruST_Manual.org" "/Users/jiawei/Projects/TruST/TruST_Manual_Evaluation.org" "/Users/jiawei/Projects/TruST/training_log.org" "/Users/jiawei/Projects/TruST/vicon.org" "/Users/jiawei/org/journal/2023-09-25.org" "/Users/jiawei/org/journal/2023-10-02.org" "/Users/jiawei/org/journal/20230911.org" "/Users/jiawei/org/journal/20230918.org" "/Users/jiawei/org/journal/2023-10-02"))
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
