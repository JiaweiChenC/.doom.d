;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d56a9fa4b46c249961ce6891f3f86d7936cb283088e6a83ffe848d295b037163"
     "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4" default))
 '(org-agenda-files '("/Users/jiawei/org/journal/20251001"))
 '(package-selected-packages '(simple-httpd))
 '(safe-local-variable-directories
   '("/Users/jiawei/.emacs.d/.local/straight/repos/citar/"
     "/Users/jiawei/Documents/roam/" "/Users/jiawei/Projects/Bio_model/"
     "/Users/jiawei/Projects/courses/"
     "/Users/jiawei/Projects/RobUST_stepping_task/"
     "/Users/jiawei/Projects/figure_of_eight_PD/"
     "/Users/jiawei/Projects/virtual_maze_multi_session/"
     "/Users/jiawei/.doom.d/" "~/.emacs.d/"))
 '(safe-local-variable-values
   '((eval progn (setq-local TeX-master "./latex/main.tex")
      (setq-local TeX-master-file nil)
      (setq-local org-cite-export-processors '((latex bibtex)))
      (setq-local my/org-export-outfile
       (concat "./latex/" (file-name-base buffer-file-name))))
     (my/project-todo-file
      . "/Users/jiawei/Projects/virtual_maze_multi_session/.notes/TODO.org")
     (org-roam-db-location
      . "/Users/jiawei/Projects/virtual_maze_multi_session/.notes/org-roam.db")
     (org-roam-directory
      . "/Users/jiawei/Projects/virtual_maze_multi_session/.notes")))
 '(zoom-size '(0.8 . 0.8)))
;; (custom-set-faces
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'magit-clean 'disabled nil)
;; disable org block highlight
;; (after! org
;;   (set-face-attribute 'org-block nil :foreground nil :background nil)
;;   (set-face-attribute 'org-block-begin-line nil :foreground nil :background nil)
;;   (set-face-attribute 'org-block-end-line nil :foreground nil :background nil)
;;   )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mini-echo-yellow ((t (:foreground "gold3")))))
;; (put 'scroll-left 'disabled nil)
