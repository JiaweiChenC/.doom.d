;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3"
     "a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031"
     "c5975101a4597094704ee78f89fb9ad872f965a84fb52d3e01b9102168e8dc40"
     "0adcffc4894e2dd21283672da7c3d1025b5586bcef770fdc3e2616bdb2a771cd"
     "6bf350570e023cd6e5b4337a6571c0325cec3f575963ac7de6832803df4d210a"
     "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     "35a828eb273e55dc0041c11e7b78cd7b3c005a566e73ee4e99f253447b768bb7"
     "95ddb63be6417ed3d661a9858cdde3b8e363f783ff2b8181840ee7a5153bda49"
     "1a115a5125aca3244fc09a946e52386d93d7bb2ca5c65338b05aabdc39d80f3b"
     "55d823394508383752cafe3a914d1831586ea72b56628693521fea04c0fefbcc"
     "f36a52da134eafa60274e852320daf066acb824c6b81a46dc7accf538ea573a6"
     "df768cd7947c2f04116090d3df3b6dddfa3633249f477bd5e44a72d83b059961"
     "fd6f696ce0bd8bfdb72c573631e0b0cdc33e6301852d656596617a7bbaf55729"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
     "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "2aecbd599720b2ae10a33ada23e800a4a445fa1f758cd2c8139f71708f53bfcc"
     "c4a411af520148fb09c310dbe00d1b0b74ee7a3df61dc9c8dd832bd3a7205f6c"
     "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     "9665818204df71cbe983e85dbb8cd024ae36abbd2c8be715b2a5702ea142be29"
     "a34d21b95ca98576120c4231b1a43b13f575c014866dbb76da406e0ce708e5f2"
     "9fd5bb26aa0a993af675c19372ac72b6e1299503cb32050178d21edabe9a1845"
     "a6224a67a66b64efcdc4967ea70f7ec135a0c6a66e704e29fe8a3e401614deae"
     "8717434774f34f325aca6fedb24b572026a0e61dca6e3fe5c03f8c3af8f412f6"
     "7e969364b4921a142b017eb4135005786650f76e40564abdce7ea6d0e0d46ead"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "e8183add41107592ee785f9f9b4b08d21bd6c43206b85bded889cea1ee231337" default))
 '(org-agenda-files '("/Users/jiawei/org/journal/20250501"))
 '(package-selected-packages
   '(avy bibtex-completion citeproc eglot htmlize hydra ox-pandoc request))
 '(safe-local-variable-directories
   '("/Users/jiawei/Projects/virtual_maze_PD/paper/movement_disorder/"
     "/Users/jiawei/Projects/Bio_model/"
     "/Users/jiawei/Projects/Bio_model/utils_for_bio_models/LearningHumans/"
     "/Users/jiawei/Projects/Bio_model/doc/" "/Users/jiawei/.doom.d/"
     "~/.emacs.d/"))
 '(warning-suppress-log-types '((org-element org-element-parser)))
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
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow))))
 '(mini-echo-blue ((t (:foreground "dark cyan"))))
 '(mini-echo-yellow ((t (:foreground "dark magenta")))))
