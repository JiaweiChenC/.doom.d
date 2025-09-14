;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/Users/jiawei/org/journal/20250901"))
 '(package-selected-packages '(simple-httpd))
 '(safe-local-variable-directories
   '("/Users/jiawei/Projects/courses/"
     "/Users/jiawei/Projects/RobUST_stepping_task/"
     "/Users/jiawei/Projects/figure_of_eight_PD/"
     "/Users/jiawei/Projects/virtual_maze_multi_session/"
     "/Users/jiawei/.doom.d/" "~/.emacs.d/"))
 '(safe-local-variable-values
   '((my/project-todo-file
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
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :underline nil))))
 '(mini-echo-yellow ((t (:foreground "gold3")))))
;; (put 'scroll-left 'disabled nil)
