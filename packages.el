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
(unpin! org-journal hide-mode-line dirvish org
         citar-org-roam citar citar-embark flycheck
        yasnippet nerd-icons doom-snippets
        )
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
(package! code-cells)
(package! org-download)
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear" :branch "org-9.7-fixes"))
(package! modus-themes)
;; (package! activities)
;; (package! spacious-padding)
(package! bookmark-in-project)
;; (package! nano-modeline)
;; pin to a version
(package! mini-echo)
;; (package! doom-nano-modeline
;;   :recipe (:host github
;;   :repo "ronisbr/doom-nano-modeline"))
;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))
(package! sideline)
(package! sideline-flycheck)
(package! color-theme-sanityinc-tomorrow)
(package! sideline-eglot
  :recipe (:host github :repo "emacs-sideline/sideline-eglot")
  )
;; (package! org-preview-html)
(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent"))
;; (package! iscroll)
;; (package! nova
;;   :recipe (:host github :repo "thisisran/nova"))
(package! float-narrow-indirect
  :recipe (:host github
           :repo "yibie/float-narrow-indirect"))
;; (package! holo-layer :recipe (:host github :repo "manateelazycat/holo-layer"))
(package! rainbow-csv
  :recipe (:host github :repo "emacs-vs/rainbow-csv"))

(package! breadcrumb
  :recipe (:host github :repo "joaotavora/breadcrumb"))
;; (package! cell-mode
  ;; :recipe (:host gitlab :repo "dto/cell-mode"))

(package! solarized-theme
  :recipe (:host github :repo "bbatsov/solarized-emacs"))
(package! zoom)
(package! pet)
(package! gruvbox-theme)
(package! ef-themes)
;; (package! window-stool :recipe (:host github :repo "jaszhe/window-stool" :files ("*.el")))
;; (package! org-src-context
;;   :recipe (:host github :repo "karthink/org-src-context"))

;; (package! exec-path-from-shell)
;; (package! savefold
;;   :recipe (:host github :repo "jcfk/savefold.el"))
;; (package! org-supertag
  ;; :recipe (:host github :repo "yibie/org-supertag"))
(package! phscroll
  :recipe (:host github :repo "misohena/phscroll"))
;; (package! olivetti)
(package! grip-mode)
;; (package! xeft)
(package! ivy)
(package! nano-theme)
;; (package! catppuccin-theme
  ;; :recipe (:host github :repo "catppuccin/emacs" :files ("*.el" "data")))
(package! eglot
:recipe (:host github :repo "joaotavora/eglot")
:pin "355a167c625b58a0ff2c1b1bbcc8c18bf64b3b08")
(package! kaolin-themes)
