;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "e8183add41107592ee785f9f9b4b08d21bd6c43206b85bded889cea1ee231337" default))
 '(org-agenda-files
   '("/Users/jiawei/org/journal/20250404" "/Users/jiawei/org/journal/20250401"))
 '(package-selected-packages
   '(avy bibtex-completion citeproc eglot htmlize hydra ox-pandoc request))
 '(safe-local-variable-directories
   '("/Users/jiawei/Projects/Bio_model/"
     "/Users/jiawei/Projects/Bio_model/utils_for_bio_models/LearningHumans/"
     "/Users/jiawei/Projects/Bio_model/doc/" "/Users/jiawei/.doom.d/"
     "~/.emacs.d/"))
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
 '(mini-echo-yellow ((t (:foreground "dark magenta")))))
