;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fb232a8ae1311f1b8acecb1f766880d12d7a01b7d8d547e7c09325a074a31237"
     "3799f9b2e997c7cf7d1a5d9846095c8976bce96852eda40d8bf9248157c2615f"
     "f9575ecd8da2a02c114e427f2ebb49ffee94285c5963f33a0abf357c5a33a1d9"
     "a653d827813de11c7416eaebe9b5cf5700ec272296a89b67dd37650dca8b7dfc"
     "1f26d58b9261b912777eb139c13cb183e62ce1d7362591b2db1e8a79c5c18418"
     "feffd678f5f23be51697e8a5c20a96ef4e339d426bbe3759a67409f951e00477"
     "6f71d969fca920c7fd0778f2fa8acabdfb1de2da6001fbbe5da4466d477a98ce"
     "c1f0e0e78217e59a4d30253f216a2d6cd0a7dfc31d830d392b60e5a485b0318b"
     "d2ed4d5e92393529904a64b842964d0d733f2c1e8e11efc28ed88746990cf3ba"
     "6fe7d8db7ec5f0e84ef3161ce03574b59abbb320074b21fd5ae8aaae1f7317ca"
     "4551d8bc9b3c67424a85164e680b6e0dc10da65682bd9948db355663ec5452e6"
     "4537882cfb108606dc87b5cd149c1b7d1c68b0c386844067c96a0a79a4097f18"
     "a658367642a1660044944bc69f6e37109a0fabf4e4a3f02e617fd61abbf4eb89"
     "d55bb68460a9cb867675cefd2c7db3092caa8a8077dab0a844b0dc76d688911e"
     "055ae3b5f1ae78773fe4e3040ae06f6bf4c4e5b51a813045b7ea4e1b8b984629"
     "49e0d43842d8087b496519c7d3894b4beebcd76d88a3f737f8e6b917380fdcf0"
     "f7a508ce5ad38c46b7fa01c7d6dffc8055a3c4c997d65e2cca2c4a1cab86e888"
     "23a995d88ee772ca8827328a074847a74ee664dccdbe2e54a2e4d1d61bd1ffaf"
     "0be354994ca8e891ceeded72e15b4a8dcc3026d087ca839def9ba1b147bb9fb4"
     "38fc94cabfa2ed7f22880b77194170c879ecfe7c12b957bfc52782643f18aec1"
     "14a6b3b8b9c645d76724219fc2b863d286cd44f8f342806aaee1a6cc0b7481eb"
     "579cfff1f6b4eb5faca5dff95504e7e31f12649e4bcc320edbc67c7bc89c80d6"
     "1d75139247065d19a0db3df008d1bcfb69183f6ab8e5767946601bac46c72bac"
     "69a25b1d88aad37bb3a7c311a4a648772fa794a8ab4c1fa69b9455112e536449"
     "a8347c492511ecc002df26d2b268f1342d84af50c43901e0d88dd678134729d8"
     "cfc1e49eec0d57e2f70ef3a6db3b543bbb2edcf16a9f164231afcc4d1263f9fb" default))
 '(org-agenda-files '("/Users/jiawei/org/journal/20251201") t)
 '(package-selected-packages
   '(cond-let llama magit-section tomlparse uuidgen with-editor))
 '(safe-local-variable-directories
   '("/ssh:100.105.242.51:/home/jiawei/projects/GVHMR/"
     "/rsync:100.105.242.51:/home/jiawei/projects/GVHMR/"
     "/scpx:100.105.242.51:/home/jiawei/projects/GVHMR/"
     "/ssh:100.105.242.51:/home/jiawei/projects/HSMR/"
     "/Users/jiawei/Projects/virtual_maze_PD/paper/"
     "/Users/jiawei/Projects/Bio_model/"
     "/Users/jiawei/Projects/figure_of_eight_PD/"
     "/ssh:35.221.44.134:/home/jiawei/projects/hpe/"
     "/Users/jiawei/Projects/virtual_maze_multi_session/"
     "/Users/jiawei/Documents/roam/" "/Users/jiawei/Projects/courses/"
     "/Users/jiawei/.doom.d/" "~/.emacs.d/"))
 '(warning-suppress-log-types
   '((uuid network-interface-info) (org latex-preview hyperref)))
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
 '(mini-echo-yellow ((t (:foreground "DarkGoldenrod3")))))
;; (put 'scroll-left 'disabled nil)
