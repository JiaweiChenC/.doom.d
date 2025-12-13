;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
   (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
   :files
   (:defaults "etc")
   :build t :pre-build
   (with-temp-file "org-version.el"
     (require 'lisp-mnt)
     (let
         ((version
           (with-temp-buffer
             (insert-file-contents "lisp/org.el")
             (lm-header "version")))
          (git-version
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
              (buffer-string)))))
       (insert
        (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
        (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
        "(provide 'org-version)\n"))))
  :pin nil)

(unpin! auctex evil-easymotion dirvish eglot quickrun)
;; (unpin! persp-mode)
 ;;         citar-org-roam citar citar-embark ess persp-mode yasnippet
 ;;         nerd-icons doom-snippets transient)
(unpin! org)
(package! htmlize)
(package! impatient-showdown)
(package! impatient-mode)
(package! ox-hugo)
(package! super-save)
(package! matlab-mode)
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! flycheck-popup-tip :disable t)
;; (package! evil-nerd-commenter :disable t)
(package! code-cells)
(package! org-download)
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear" :branch "org-9.7-fixes"))
(package! modus-themes)
;; (package! activities)
;; (package! tabspaces)
;; (package! bookmark-in-project)
(package! mini-echo)
(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent"))
;; (package! iscroll)
(package! breadcrumb
  :recipe (:host github :repo "joaotavora/breadcrumb"))
(package! zoom)
(package! pet)
(package! gruvbox-theme)
;; (package! ef-themes
;;   :recipe (:host github :repo "JiaweiChenC/ef-themes"))
;; (package! ef-themes)
;; (package! window-stool :recipe (:host github :repo "jaszhe/window-stool" :files ("*.el")))
(package! exec-path-from-shell)
(package! phscroll
  :recipe (:host github :repo "misohena/phscroll"))
(package! grip-mode)
(package! ivy)
(package! nano-theme)
;; (disable-packages! evil-nerd-commenter)
(unpin! evil-nerd-commenter)
(package! evil-nerd-commenter
  :recipe (:host github :repo "JiaweiChenC/evil-nerd-commenter"))
(package! uv
  :recipe (:host github :repo "johannes-mueller/uv.el"))
;; (package! lsp-treemacs)
;; (package! flash.emacs
;;   :recipe (:host github :repo "JiaweiChenC/flash.emacs"))
;; disable evil snipe
(package! evil-snipe :disable t)
;; (package! evil-surround :disable t)
;; (package! claude-code-ide
;;   :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
;;   :recipe (:host github :repo "stevemolitor/claude-code.el"))
(package! eat)
(package! sideline)
(package! sideline-flycheck)
(package! sideline-eglot
  :recipe (:host github :repo "emacs-sideline/sideline-eglot")
  )
;; (package! org-inline-pdf)
;; (package! outline-indent)
(package! atomic-chrome)
;;(package! reader
;;  :recipe (:host codeberg
;;           :repo "divyaranjan/emacs-reader"))
(package! catppuccin-theme)
(package! ox-pandoc)
;; (package! evil-visual-mark-mode
  ;; :recipe (:host github :repo "JiaweiChenC/evil-visual-mark-mode"))
;; (package! zotxt)
;; (package! minuet)
;; (package! find-file-in-project)
;; (package! mason)
;; (package! tramp-hlo
  ;; :recipe (:host github :repo "jsadusk/tramp-hlo"))
;; (package! zlua
;;   :recipe (:host github
;;            :repo "Kinneyzhang/zlua"
;;            :files ("*.el" "z.lua")))
