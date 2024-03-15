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
                                        ;
                                        ;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi
      doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))
;; (setq doom-theme 'modus-operandi
;;       doom-font (font-spec :family "JetBrains Mono" :size 15)
;;       doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 16))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-log-into-drawer t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'nil)

(setq auto-save-default t)

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
  (add-to-list 'cdlatex-math-modify-alist '( ?s "\\boldsymbol"  nil  t t nil ))
  (add-to-list 'cdlatex-math-modify-alist '( ?n "\\mathbb"      nil  t t nil ))
  )

(setq org-latex-src-block-backend "listings")

(setq org-roam-directory "~/Documents/roam/")

(setq vterm-tramp-shells '(("ssh" "/usr/bin/bash")))

(setq org-agenda-files '("~/org/journal/"))
;; (add-to-list 'org-agenda-files "/Users/jiawei/Projects/TruST/")
;; (add-to-list 'org-agenda-files "/Users/jiawei/Projects/modern_control_projects/")
(setq org-journal-enable-agenda-integration t)
;; add all the todo.org file to the agenda

;; do not highlight the current line
(setq org-fontify-done-headline t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq org-log-done t)

(setq org-preview-latex-default-process 'dvisvgm)

(setq fancy-splash-image (concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)


(setq doom-modeline-major-mode-icon t)

(map! :n "gj" 'evil-next-visual-line)
(map! :n "gk" 'evil-previous-visual-line)
(map! (:after evil-org
       :map evil-org-mode-map
       )
      :o "o" #'evil-inner-symbol
      :leader
      (:prefix "f"
               "t" #'find-in-dotfiles
               "T" #'browse-dotfiles)
      (:prefix "n"
               "b" #'org-roam-buffer-toggle
               "j" #'org-journal-open-current-journal-file
               "d" #'org-journal-new-entry
               "D" #'org-journal-new-date-entry
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



(after! org
  (setq org-startup-folded 'content)
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  ;; (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 400)
  (org-link-set-parameters "zotero"
                           :follow (lambda (url arg) (browse-url (format "zotero:%s" url) arg)))
  (map! :map org-mode-map
        "C-M-y" #'zz/org-download-paste-clipboard)
  ;; map space l i to citar insert citation
(map! :leader :desc "citar insert citation" "l i" #'citar-insert-citation)
  )

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; set org-image-actual-width to 700 after entering doom big font mode
;; and set it back to 400 after exiting doom big font mode
(add-hook 'doom-big-font-mode-hook
          (lambda ()
            (if doom-big-font-mode
                (setq org-image-actual-width 700)
              (setq org-image-actual-width 400))))

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

(setq! org-noter-notes-search-path '("/Users/jiawei/Documents/roam/booknotes"))

;; citar configuration
(setq! org-cite-csl-styles-dir "~/Zotero/styles")
(setq! citar-bibliography '("~/Documents/roam/biblibrary/references.bib"))
(setq! bibtex-completion-library-path '("~/Documents/roam/biblibrary/")
       bibtex-completion-notes-path "~/Documents/roam/")
(setq! citar-library-paths '("~/Documents/roam/biblibrary/")
       citar-notes-paths '("~/Documents/roam/paper/"))
(setq citar-symbol-separator "  ")

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  ;; set delay to 0.5s
        (setq copilot-idle-delay 0.5)
  :bind (("M-TAB" . 'copilot-accept-completion-by-word)
         ("M-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; set space o m as mac-os-open-with
(map! :leader :desc "macos open with default programe" "o m" #'+macos/open-in-default-program)

;; set yank from kill ring to space y
(map! :leader :desc "yank from kill ring" "y y" #'yank-from-kill-ring)

;; open warp terminal in current directory
(defvar last-warp-dir nil
  "Directory where the last Warp Terminal was opened.")


(map! :n "C-;" #'scroll-other-window)
(map! :n "C-'" #'scroll-other-window-down)

;; set the default frame
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; bind quickrun to space r r
(map! :leader :desc "quickrun" "r r" #'quickrun)
;; bind quickrun kill process to space r k
(map! :leader :desc "quickrun kill process" "r k" #'quickrun--kill-running-process)


(use-package! dired
  :config
  (set-popup-rule! "^\\*image-dired" :ignore t))

;; set org journal to weekly
(setq org-journal-file-type 'weekly)


;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (projectile-find-file))

;; ;; set C-n and C-p in insert mode to next line and previous line
(map! :map evil-insert-state-map
      "C-n" #'next-line
      "C-p" #'previous-line
      )

;; initial frame size
(setq initial-frame-alist
      '((width . 150) (height . 55)))

(setq projectile-indexing-method 'native)

(setq! citar-open-entry-function #'citar-open-entry-in-zotero)

;; after python mode, start evil vimish fold mode
(add-hook 'python-mode-hook #'evil-vimish-fold-mode)

(setq! warning-suppress-types (append warning-suppress-types '((org-element-cache))))

(setq! quickrun-timeout-seconds 10000)

(defun +macos/open-link-in-default-program ()
  "Open the file under the cursor with the system's default application."
  (interactive)
  (let* ((file-path (thing-at-point 'filename))
         (clean-path (replace-regexp-in-string "^[^:]+:" "" file-path))
         (expanded-path (expand-file-name clean-path))) ; expand ~ to the user's home directory
    (if (and expanded-path (file-exists-p expanded-path))
        (shell-command (concat "open " (shell-quote-argument expanded-path)))
      (message "No file under cursor found."))))
(map! :leader :desc "macos open link with default programe" "o [" #'+macos/open-link-in-default-program)

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

;; warp terminal
(defun open-warp-terminal-in-dir ()
  "Open Warp Terminal in the current directory if not already open."
  (interactive)
  (let ((dir default-directory))
    (if (equal dir last-warp-dir)
        (shell-command "osascript -e 'tell application \"Warp\" to activate'")
      (progn
        (setq last-warp-dir dir)
        (shell-command (concat "open -a Warp " dir))))))
;; map space o w to open warp terminal in current directory
(map! :leader :desc "open warp terminal in current directory" "o w" #'open-warp-terminal-in-dir)
(defun zz/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

;; map insert a file name function to space i a
(map! :leader :desc "insert another file name" "i a" #'zz/insert-file-name)

(defun +macos/find-file-with-default-program ()
  "Select a file in Emacs and open it using the default program on your Mac."
  (interactive)
  (let ((file-name (read-file-name "Select file: ")))
    (call-process-shell-command (concat "open " file-name))))

;; map open file with mac default to space m m
(map! :leader :desc "find file with mac default" "m m" #'+macos/find-file-with-default-program)

;; use org-appear package
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t))

(defun my-export-org-to-latex-body-only ()
  "Export current Org file to a LaTeX file with body only."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil))

(use-package copilot
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda ()
                    (setq-local copilot--indent-warning-printed-p t))))

(defun org-export-latex-body-only ()
  "Export current Org file to a LaTeX file with body only, open and compile it."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil)) ;; Run the compile command
;; map to space l b
(map! :leader :desc "export latex body only" "l b" #'org-export-latex-body-only)


(defun org-compile-latex ()
  "Export current Org file to a LaTeX file with body only,
compile it, then switch back to the Org file."
  (interactive)
  (let ((original-buffer (current-buffer)) ; Store the current buffer (Org file)
        (output-file (org-latex-export-to-latex nil nil nil t nil))) ; Export and get the output file name
    (find-file output-file) ; Open the LaTeX file
    (call-interactively '+latex/compile) ; Run the compile command
    (switch-to-buffer original-buffer)
    ;; (when (file-exists-p output-file)
    ;;   (start-process "open-pdf" "*open-pdf-output*" "open" "-a" "Preview" output-file))
    ))

;; map compile latex to space l c
(map! :leader :desc "compile latex" "l c" #'org-compile-latex)

;; map to space y m
(map! :leader :desc "convert markdown links to org and paste" "y m" #'convert-markdown-links-to-org-and-paste)

