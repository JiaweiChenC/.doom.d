;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jiawei Chen"
      user-mail-address "jc5667@columbia.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'nil)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-log-into-drawer t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'nil)

(setq auto-save-default t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(setq latex-preview-pane-use-frame t)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(after! cdlatex
(add-to-list 'cdlatex-math-modify-alist '( ?s   "\\boldsymbol"               nil        t   t   nil ))
(add-to-list 'cdlatex-math-modify-alist '( ?n   "\\mathbb"               nil        t   t   nil ))
)

(setq org-latex-listings t)

(setq org-roam-directory "~/Documents/roam")

(setq vterm-tramp-shells '(("ssh" "/usr/bin/bash")))

(setq org-agenda-files '("~/Documents/roam/daily/"))
(add-to-list 'org-agenda-files "/Users/jiawei/Projects/TruST/training_log.org")
(add-to-list 'org-agenda-files "/Users/jiawei/Documents/roam/research/meetings.org")

(setq org-fontify-done-headline t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq org-log-done t)

(setq org-preview-latex-default-process 'dvisvgm)
(after! org (plist-put org-format-latex-options :scale 2.5))
(setq fancy-splash-image (concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(setq org-startup-folded 'show2levels)

(setq doom-modeline-major-mode-icon t)

(define-key evil-normal-state-map (kbd "gj") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "gk") 'evil-previous-visual-line)

(map! (:after evil-org
       :map evil-org-mode-map
       )
      :o "o" #'evil-inner-symbol

      :leader
      "h L" #'global-keycast-mode
      (:prefix "f"
       "t" #'find-in-dotfiles
       "T" #'browse-dotfiles)
      (:prefix "n"
       "b" #'org-roam-buffer-toggle
       "d" #'org-roam-dailies-goto-today
       "D" #'org-roam-dailies-goto-date
       "e" (cmd! (find-file (doom-path org-directory "ledger.gpg")))
       "i" #'org-roam-node-insert
       "r" #'org-roam-node-find
       "R" #'org-roam-capture))

(use-package! mathpix.el
  :custom ((mathpix-app-id "chenjw12580_gmail_com_2ad82a_fb84ed")
           (mathpix-app-key "a52385924df4b5a6c0ada7b0f127e5d721147387d2b7be5494919f957ae11565"))
  :bind
  ("C-x m" . mathpix-screenshot))
(setq mathpix-screenshot-method "screencapture -i %s")

(use-package! websocket
    :after org-roam)

(use-package super-save
  :config
  (super-save-mode +1))

(setq auto-save-default nil)
(setq super-save-remote-files nil)

;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)
(add-to-list 'super-save-triggers '+vterm/toggle)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize 'grow-only)
  )

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; paste image
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 400)
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! lsp-clangd
  (set-lsp-priority! 'clangd 2)
  lsp-clients-clangd-args '("-j=7"
                            "--background-index"
                            "--clang-tidy"
                            "--completion-style=detailed"
                            "--suggest-missing-includes"
                            "--header-insertion=never")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection (cons "clangd" lsp-clients-clangd-args))
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(after! lsp-pyright
  (setq lsp-log-io t)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-diagnostic-mode "workspace")
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (lambda ()
                                            (cons "pyright-langserver"
                                                  lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyright-remote
    :multi-root t
    :priority 3
    :initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
                                                 (lsp-configuration-section "python")))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (ht-merge (lsp-configuration-section "pyright")
                                   (lsp-configuration-section "python")))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                   ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
  )


(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("b" "booknotes" plain
           ,(format "#+title: ${title}\n%%[%s/template/booknotes.org]" org-roam-directory)
           :target (file "booknotes/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "coding" plain
           ,(format "#+title: ${title}\n%%[%s/template/coding.org]" org-roam-directory)
           :target (file "coding/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("l" "lectures" plain
           ,(format "#+title: ${title}\n%%[%s/template/lectures.org]" org-roam-directory)
           :target (file "lectures/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("r" "research" plain
           ,(format "#+title: ${title}\n%%[%s/template/research.org]" org-roam-directory)
           :target (file "research/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("a" "paper" plain
           ,(format "#+title: ${title}\n%%[%s/template/paper.org]" org-roam-directory)
           :target (file "paper/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("m" "math" plain
           ,(format "#+title: ${title}\n%%[%s/template/math.org]" org-roam-directory)
           :target (file "math/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))))

(setq org-noter-notes-search-path '("/Users/jiawei/Documents/roam/booknotes"))
;; ;; (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
;; (add-to-list 'TeX-command-list '("pdflateX" "%`pdflatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))

;; use posframe
(company-posframe-mode 1)
 (setq company-posframe-show-indicator nil
       company-posframe-show-metadata nil)
(setq company-posframe-show-indicator nil)

;; alias open file
(defalias 'e 'find-file)
(defalias 'eo 'find-file-other-window)

;; citar configuration
;; paths
(setq org-cite-csl-styles-dir "~/Zotero/styles")
(setq! citar-bibliography '("~/Documents/roam/biblibrary/references.bib"))
(setq! bibtex-completion-library-path '("~/Documents/roam/biblibrary/")
       bibtex-completion-notes-path "~/Documents/roam/")
(setq! citar-library-paths '("~/Documents/roam/biblibrary/")
       citar-notes-paths '("~/Documents/roam/"))
;; citar symbols
(setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
(setq citar-symbol-separator "  ")

