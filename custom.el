;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9665818204df71cbe983e85dbb8cd024ae36abbd2c8be715b2a5702ea142be29"
     "a34d21b95ca98576120c4231b1a43b13f575c014866dbb76da406e0ce708e5f2"
     "9fd5bb26aa0a993af675c19372ac72b6e1299503cb32050178d21edabe9a1845"
     "a6224a67a66b64efcdc4967ea70f7ec135a0c6a66e704e29fe8a3e401614deae"
     "8717434774f34f325aca6fedb24b572026a0e61dca6e3fe5c03f8c3af8f412f6"
     "7e969364b4921a142b017eb4135005786650f76e40564abdce7ea6d0e0d46ead"
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
 '(warning-suppress-log-types '((files missing-lexbind-cookie)))
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
