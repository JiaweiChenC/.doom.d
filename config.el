;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
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
;; + `doom-bg-font' -- used for `doom-big-font-mode'; use this for
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
(setq doom-theme 'doom-rose-pine-dawn
      doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13)
      )

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

(after! cdlatex
  (add-to-list 'cdlatex-math-modify-alist '( ?s "\\boldsymbol"  nil  t t nil ))
  (add-to-list 'cdlatex-math-modify-alist '( ?n "\\mathbb"      nil  t t nil )))

(after! texmathp
  (add-to-list 'texmathp-tex-commands '("tikzpicture" env-on)))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq org-latex-src-block-backend "listings")

(setq org-roam-directory "~/Documents/roam/note/")

(setq vterm-tramp-shells '(("ssh" "/usr/bin/zsh")))

(setq org-agenda-files '("~/org/journal/"))
(setq org-journal-enable-agenda-integration t)
;; add all the todo.org file to the agenda

;; (global-hide-mode-line-mode)

;; do not highlight the current line
(setq org-fontify-done-headline t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq org-log-done t)


(setq fancy-splash-image (concat doom-user-dir "splash.png"))
;; Hide the menu for minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(setq doom-modeline-major-mode-icon t)

;; map 0 in org mode to evil beginning-of-line
(map! (:after evil-org
       :map evil-org-mode-map
       :n "0" #'evil-beginning-of-line
       :n "gk" (cmds! (org-on-heading-p)
                      #'org-backward-element
                      #'evil-previous-visual-line)
       :n "gj" (cmds! (org-on-heading-p)
                      #'org-forward-element
                      #'evil-next-visual-line))
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

(when (file-exists-p "~/.emacs.d/.secret.el")
  (load "~/.emacs.d/.secret.el"))

(use-package! mathpix.el
  :custom ((mathpix-app-id mathpix-app-id)
           (mathpix-app-key mathpix-app-key))
  :bind
  ("C-x m" . mathpix-screenshot))

(setq mathpix-screenshot-method "screencapture -i %s")

(use-package! super-save
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


(after! org
  ;; (setq! org-src-context-mode 1)
  (setq! org-pretty-entities nil)
  ;; start hl-todo-mode
  ;; disable org indent mode
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-heading-lvl nil)
  ;; (setq! org-download-method 'directory)
  (setq org-download-image-dir "images")
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (defadvice! recover-paragraph-seperate ()
    "Recover org paragraph mark position."
    :after 'org-setup-filling
    (setq-local paragraph-start "[\f\\|[ \t]*$]")
    (setq-local paragraph-separate "[ \t\f]*$"))
  (setq org-startup-folded 'content)
  ;; (setq org-image-actual-width '(400))
  (org-link-set-parameters "zotero"
                           :follow (lambda (url arg) (browse-url (format "zotero:%s" url) arg)))
  (org-link-set-parameters "skim"
                           :follow (lambda (url arg) (browse-url (format "skim:%s" url) arg)))
  (map! :map org-mode-map
        "C-M-y" #'zz/org-download-paste-clipboard)
  )

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq! org-noter-notes-search-path '("/Users/jiawei/Documents/roam/booknotes"))
;; (setq citar-org-roam-capture-template-key "z")


;; citar configuration
(setq! org-cite-csl-styles-dir "~/Zotero/styles")
(setq! citar-bibliography '("~/Documents/roam/biblibrary/references.bib"))
;; (setq! citar-library-paths '("/Users/jiawei/Documents/roam/paper/"))
;; (setq! citar-notes-paths '("/Users/jiawei/Documents/roam/paper/"))
(setq citar-symbol-separator "  ")
;; (setq! citar-org-roam-subdir "paper/")

(setq org-roam-capture-templates
      '(("n" "note" plain
         "* ${title}\n\n%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+created: %<%Y-%m-%d>")
         :unnarrowed t
         :empty-lines 1)))

(use-package! copilot
 :hook (prog-mode . copilot-mode)
 :bind (("<backtab>" . 'copilot-accept-completion-by-word)
        :map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion))
 :config
 (add-to-list 'copilot-indentation-alist '(prog-mode 2))
 (add-to-list 'copilot-indentation-alist '(org-mode 2))
 (add-to-list 'copilot-indentation-alist '(text-mode 2))
 (add-to-list 'copilot-indentation-alist '(closure-mode 2))
 (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
 (setq copilot-idle-delay 0.5)
 )

;; open warp terminal in current directory
(defvar last-warp-dir nil
  "Directory where the last Warp Terminal was opened.")


;; set the default frame
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; bind quickrun to space r r
(map! :leader :desc "quickrun" "r r" #'quickrun)

;; bind quickrun kill process to space r k
(map! :leader :desc "quickrun kill process" "r k" #'quickrun--kill-running-process)

(use-package! dired
  :config
  (set-popup-rule! "^\\*image-dired" :ignore t))

(setq org-journal-file-type 'monthly)

;; ;; set C-n and C-p in insert mode to next line and previous line
(map! :map evil-insert-state-map
      "C-n" #'next-line
      "C-p" #'previous-line
      )

;; initial frame size
(setq initial-frame-alist
      '((width . 150) (height . 50)))
;; Set the default frame width and height
(add-to-list 'default-frame-alist '(width . 170))  ; width set to 100 columns
(add-to-list 'default-frame-alist '(height . 60))  ; height set to 50 lines

(setq projectile-indexing-method 'native)

;; after python mode, start evil vimish fold mode
(add-hook 'python-mode-hook #'evil-vimish-fold-mode)

(setq! quickrun-timeout-seconds 1000000)

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


(defun +macos/find-file-with-default-program ()
  "Select a file in Emacs and open it using the default program on your Mac."
  (interactive)
  (let ((file-name (read-file-name "Select file: ")))
    (call-process-shell-command (concat "open " file-name))))

;; map open file with mac default to space m m
(map! :leader :desc "find file with mac default" "e f" #'+macos/find-file-with-default-program)

(defun my/export-org-to-latex-body-only ()
  "Export current Org file to a LaTeX file with body only."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil))

;; map compile latex to space l c
(map! :leader :desc "compile latex" "l c" #'org-compile-latex)

(setq split-width-threshold nil)

(defun my/switch-to-workspace-in-new-frame ()
  "Prompt for an existing workspace, then open it in a new frame."
  (interactive)
  (let* ((workspaces (+workspace-list-names)) ; Get a list of existing workspace names
         (workspace (completing-read "Select workspace: " workspaces))) ; Prompt to select
    (let ((new-frame (make-frame))) ; Create a new frame
      (select-frame-set-input-focus new-frame) ; Focus the new frame
      (+workspace/switch-to workspace) ; Switch to the selected workspace in the new frame
      (+workspace/display))) ; Display the workspace tab bar in the new frame
  )

;; map to space tab ,
(map! :leader :desc "switch to workspace in new frame" "TAB ," #'my/switch-to-workspace-in-new-frame)

(setq org-hide-macro-markers t)

(defun my/citar-open-pdf ()
  "Open all PDF files associated with selected references
 on macOS using the default system application."
  (interactive)
  (let* ((refs (citar-select-refs))  ; User selects references
         (keys (if (listp refs) refs (list refs))))  ; Ensure keys are in a list
    (let ((files-hash (citar-get-files keys)))  ; Get files for the keys
      (maphash (lambda (key file-list)
                 (dolist (file file-list)
                   (when (string-suffix-p ".pdf" file)  ; Check if the file is a PDF
                     (if (string-prefix-p "http" file)  ; Check if it is an online PDF link
                         (browse-url file)              ; Open in a web browser
                       (start-process "open-pdf" nil "open" file)))))  ; Open locally with system default
               files-hash))))

;; map space o c to citar create note
(map! :leader :desc "citar create note" "o C" #'citar-open-files)

;; map citar open files to space o C
(map! :leader :desc "citar open files" "o c" #'my/citar-open-pdf)

(use-package! corfu
  ;; :init
  ;; (corfu-global-mode)
  :config
  (setq corfu-auto nil
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-on-exact-match 'separator
        corfu-min-width 50
        corfu-max-width 80)
  ;; (map! :i "<tab>" #'completion-at-point)
  )

(after! dabbrev
  ;; This line adds a regex to ignore buffers ending in .csv for dabbrev
  (add-to-list 'dabbrev-ignored-buffer-modes 'csv-mode))

(use-package! tab-bar
  :config
  (map! :leader :desc "tab bar mode" "t t" #'toggle-frame-tab-bar)
  (setq tab-bar-new-tab-choice t
        tab-bar-tab-name-truncated-max 20
        tab-bar-tab-hints t)
  (map! :n "]T" 'tab-bar-switch-to-next-tab)
  (map! :n "[T" 'tab-bar-switch-to-prev-tab)

  (map! :leader
        (:prefix ("t" . "tab")
         :desc "Switch to tab number"
         "1" #'(lambda () (interactive) (tab-bar-select-tab))
         "2" #'(lambda () (interactive) (tab-bar-select-tab))
         "3" #'(lambda () (interactive) (tab-bar-select-tab))
         "4" #'(lambda () (interactive) (tab-bar-select-tab))
         "5" #'(lambda () (interactive) (tab-bar-select-tab))
         "6" #'(lambda () (interactive) (tab-bar-select-tab))
         "7" #'(lambda () (interactive) (tab-bar-select-tab))
         "8" #'(lambda () (interactive) (tab-bar-select-tab))
         "9" #'(lambda () (interactive) (tab-bar-select-tab))
         )))


(setq! ess-startup-directory 'default-directory)

(defun my/move-buffer-to-new-frame ()
  "Move the current buffer to a new frame and delete it from the current frame."
  (interactive)
  (let ((buffer (current-buffer))) ; Store the current buffer
    ;; Check if there's more than one window
    (when (not (one-window-p t))
      (delete-window)) ; Delete the current window if there are multiple windows
    (display-buffer-pop-up-frame buffer nil) ; Display the buffer in a new frame
    ;; Switch to another buffer in the original window, if it still exists
    (when (not (one-window-p t))
      (switch-to-buffer (other-buffer buffer)))))

(map! :leader :desc "Move buffer to new frame" "w F" #'my/move-buffer-to-new-frame)

;; open a file using zotero
(eval-after-load 'citar-file
  '(progn
     (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))))

(setq! dired-mouse-drag-files 'move)

(add-hook 'doom-load-theme-hook
          (lambda ()
            (set-face-background 'fringe (face-attribute 'default :background))))


(load! (expand-file-name "my-quickrun.el" "~/.doom.d/lisp/"))
;; load mytex.el after org
(after! org
  (add-to-list 'org-file-apps '("\\.svg\\'" . default))
  (load! (expand-file-name "mytex.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "skim.el" "~/.doom.d/lisp/"))
  (load! ".secret.el")
  (load! (expand-file-name "babel.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "citar_function.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "lib-gptel" "~/.doom.d/lisp/"))
  (load! (expand-file-name "custom-functions" "~/.doom.d/lisp/"))
  )

(map! :n "s-;" #'skim-next-page)
(map! :n "s-'" #'skim-prev-page)
(setq org-image-actual-width nil)

;; (setq! imagemagick-types-inhibit (append imagemagick-types-inhibit '(SVG)))

(setq-hook! LaTeX-mode TeX-command-default "LaTeXMk")

(setq! org-highlight-latex-and-related '(native latex script entities))

(setq tex-fontify-script 'nil)

(setq org-latex-caption-above '(table src-block special-block math))

;; bind space b return to embard open bookmark external
(map! :leader :desc "open bookmark external" "e m" #'embark-bookmark-open-externally)

(setq! org-modern-table nil)
(setq! org-modern-block-fringe nil)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq! org-appear-autolinks t)
  )

(setq! org-latex-src-block-backend 'listings)

(setq! org-image-actual-width nil)

(setq! TeX-command-extra-options "-shell-escape")

(setq! org-export-expand-links 'nil)

(map! :leader :desc "bookmark in project" "p m" #'bookmark-in-project-toggle)
(map! :leader :desc "bookmark in project jump" "p j" #'bookmark-in-project-jump)

(setq! corfu-popupinfo-max-height 1)

(setq! vterm-timer-delay 0.01)

;; space t e to mini-echo-mode
(map! :leader :desc "toggle mini echo mode" "t e" #'mini-echo-mode)

(add-hook 'emacs-startup-hook #'global-hide-mode-line-mode)

;; (setq-default bidi-display-reordering nil)

;; (setq bidi-inhibit-bpa t
;;       long-line-threshold 1000
;;       large-hscroll-threshold 1000
;;       syntax-wholeline-max 1000)

;; (setq! org-preview-html-viewer 'xwidget)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(after! org
  (define-key org-mode-map (kbd "S-s-<mouse-1>") 'hermanhelf-org-jump-to-pdf))

(after! corfu
  (setq corfu-auto nil))

(add-hook 'prog-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-comment-face :slant 'italic)))

(use-package! rainbow-csv
  :hook (csv-mode . rainbow-csv-mode))

;; csv align mode after csv mode
(add-hook 'csv-mode-hook 'csv-align-mode)

;; disable visual line mode in csv mode
(add-hook 'csv-mode-hook
          (lambda ()
            (when (eq major-mode 'csv-mode)
              (visual-line-mode -1))))

;; map space t b to breadcrumb-mode
(map! :leader :desc "breadcrumb mode" "t h" #'breadcrumb-mode)

;; (setq! csv-align-max-width 10000)

(setq! catppuccin-flavor 'latte)

(defun wrap-text-with-color ()
  "Wrap the selected text with [[color:red][text]]."
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[[color:red][%s]]" selection)))
    (message "No text selected!")))

;; To bind this function to a key in Doom Emacs:
;; (map! :leader
;;       :desc "Wrap Text with Color"
;;       "l r" #'wrap-text-with-color)

(defun wrap-text-with-delete ()
  "Wrap the selected text with [[delete:red][text]]."
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[[delete:][%s]]" selection)))
    (message "No text selected!")))

;; To bind this function to a key in Doom Emacs:
(map! :leader
      :desc "Wrap Text with Delete"
      "l d" #'wrap-text-with-delete)

(map! :map citar-embark-citation-map
      :n
      "<return>" nil
      "<return>" #'zot-open-pdf)

(setq! evil-kill-on-visual-paste nil)

(custom-set-variables
 '(zoom-size '(0.8 . 0.8)))

;; (setq! org-startup-truncated nil)
(use-package! phscroll
  :hook (org-mode . org-phscroll-mode))

(setq! visual-fill-column-width 100)

;;; Define a flag variable for the startup option (buffer-local by Org parsing)
(defvar org-visual-fill-startup nil
  "Non-nil if this Org buffer should enable visual-fill-column-mode on startup.")

;;; Add a custom startup keyword "visual-fill" that sets the flag to t
(with-eval-after-load 'org   ; ensure Org is loaded before modifying org-startup-options
  (add-to-list 'org-startup-options '("visual-fill" org-visual-fill-startup t)))

;;; Hook to turn on visual-fill-column-mode when the flag is set by #+STARTUP
(add-hook 'org-mode-hook
          (lambda ()
            (when org-visual-fill-startup
              (visual-fill-column-mode 1))))

(defvar hoagie-org-narrow-startup nil
  "Non-nil if this Org buffer should narrow to a :narrow: tag subtree on startup.")

;;; Add a custom startup keyword "narrow" that sets the flag to t
(with-eval-after-load 'org
  (add-to-list 'org-startup-options '("narrow" hoagie-org-narrow-startup t)))

(defun hoagie--heading-has-tag-p (tag)
  "Return non-nil if the current Org heading has TAG."
  (member tag (org-get-tags)))

(defun hoagie-org-narrow-to-narrow-tag ()
  "Narrow to the first Org heading tagged with :narrow:, if any."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp nil t)
      (while (and (not (hoagie--heading-has-tag-p "narrow"))
                  (re-search-forward org-heading-regexp nil t)))
      (when (hoagie--heading-has-tag-p "narrow")
        (org-narrow-to-subtree)))))

;;; Hook to perform narrowing when #+STARTUP: narrow is present
(add-hook 'org-mode-hook
          (lambda ()
            (when hoagie-org-narrow-startup
              (hoagie-org-narrow-to-narrow-tag))))


(setq! doom-big-font-increment 2)

(map! :n "C-;" #'send-scroll-up-to-other-window)
(map! :n "C-'" #'send-scroll-down-to-other-window)

(use-package! org-modern-indent
  :hook (org-mode . org-modern-indent-mode))

(use-package! org-modern
  :defer t
  :init
  (setq! org-modern-tag nil)
  )

(defun my/deft-set-project-doc-org-dir (orig-fn &rest args)
  "Set `deft-directory` to `[project]/doc/org/` before calling `deft`."
  (let ((project-root (projectile-project-root)))
    (setq deft-directory (expand-file-name "doc/org/" project-root)))
  (apply orig-fn args))

(advice-add 'deft :around #'my/deft-set-project-doc-org-dir)

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-aliases-from-kernelspecs)
  )

;; hack to make jupyter work with images
;; https://github.com/emacs-jupyter/jupyter/issues/558
(defun skip-undo (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS without recording undo information."
  (let ((buffer-undo-list t)) ; Temporarily disable undo recording
    (apply orig-fun args)))

(advice-add 'jupyter-generate-request :around #'skip-undo)

;; attachment
(setq org-attach-auto-tag nil)

(defun insert-attachment-from-dir ()
  "Insert a link to a file from the current heading's attachment
   directory into the :ATTACHMENTS: property."
  (interactive)
  (require 'org-attach)
  (let* ((attach-dir (org-attach-dir t)) ;; Get the attachment dir, create if needed
         (files (when (file-directory-p attach-dir)
                  (directory-files attach-dir nil "^[^.]"))) ;; List non-dotfiles
         (file (completing-read "Choose attachment file: " files nil t))
         (link (concat "attachment:" file))
         (current (org-entry-get (point) "ATTACHMENTS")))
    ;; Update the ATTACHMENTS property
    (org-entry-put (point) "ATTACHMENTS"
                   (string-join (remove "" (list current (format "[[%s][%s]]" link file)))))))

;; map to space m a i
(map! :leader :desc "insert attachment from dir" "m a i" #'insert-attachment-from-dir)

;; make copilot ignore .eld file
(setq! copilot-max-char-warning-disabled t)

(map! :after evil-org
      :map evil-org-mode-map
      :n "C-j" #'electric-newline-and-maybe-indent
      :n "C-k" #'kill-line)

;; add header-args to org default properties
(after! org
  (add-to-list 'org-default-properties "HEADER-ARGS"))

(setq! envrc-remote t)

;; (use-package! org-latex-preview
  ;; :hook (org-mode . org-latex-preview-auto-mode)
  ;; :config
  ;; ;; Increase preview width
  ;; (plist-put org-latex-preview-appearance-options
  ;;            :page-width 1.0)
  ;; (plist-put org-latex-preview-appearance-options
  ;;            :zoom 1.2)
  ;; ;; ;; Use dvisvgm to generate previews
  ;; ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)

  ;; ;; ;; Block C-n, C-p etc from opening up previews when using auto-mode
  ;; (setq org-latex-preview-auto-ignored-commands
  ;;       '(next-line previous-line))

  ;; ;; ;; Enable consistent equation numbering
  ;; (setq org-latex-preview-numbered t)

  ;; (setq org-latex-preview-live t)

  ;; ;; More immediate live-previews -- the default delay is 1 second
  ;; (setq org-latex-preview-live-debounce 0.25)

  ;; (defun org--latex-preview-region (beg end)
  ;;   "Compatibility shim for old Org LaTeX preview function.
  ;;       Calls `org-latex-preview--preview-region' with a default
  ;;       processing type."
  ;;   (let ((processing-type org-latex-preview-process-default))
  ;;     (org-latex-preview--preview-region processing-type beg end)))
  ;; )

(map! :leader :desc "macos open with default programe" "o m" #'+macos/open-in-default-program)

;; do not export when archived
(setq! org-export-with-archived-trees nil)

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

(use-package! phscroll
  :hook (org-mode . org-phscroll-mode)
  )

(defun flash-emacs-jump-after ()
  "Call `flash-emacs-jump` and move forward one char if point moved forward."
  (interactive)
  (let ((origin (point)))
    (flash-emacs-jump)
    (when (> (point) origin)
      ;; only move forward if point advanced
      (forward-char))))

(after! evil
  ;; disable evil surround global mode
  (global-evil-surround-mode -1)
  (define-key evil-normal-state-map (kbd "s") #'flash-emacs-jump)
  (define-key evil-insert-state-map (kbd "C-s") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd "C-s") #'flash-emacs-jump-after)
  (define-key evil-normal-state-map (kbd "S") #'flash-emacs-ts)
  )

(load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-remote.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-ts.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-ts-search.el")

(defun flash-emacs--set-jump-before-jump (&rest _args)
  "Set a jump point before running `flash-emacs-jump`."
  (better-jumper-set-jump))

(advice-add 'flash-emacs-jump :before #'flash-emacs--set-jump-before-jump)


(map! :n "m" #'point-to-register)
(map! :n "`" #'jump-to-register)

(setq avy-keys (append (number-sequence ?a ?z)))

(defvar-local my/project-todo-file nil
  "Path to the TODO file for the current project.")

;; map my/project-todo to space p t
(map! :leader :desc "Open project TODO" "p t" #'my/project-todo)

(defun my/project-todo ()
  "Open the TODO.org file for the current project in a popup with a unique name."
  (interactive)
  (let* ((project-name (projectile-project-name))
         (todo-file my/project-todo-file)  ;; Your project-specific TODO path
         (buf-name (format "*TODO:%s*" project-name)))
    ;; Define popup rule before displaying the buffer
    (set-popup-rule!
      (format "^\\*TODO:%s\\*$" (regexp-quote project-name))
      :side 'right
      :size 0.4
      :select t
      :quit 'current
      :ttl 0
      :autosave t
      )
    ;; Open and rename buffer before popping up
    (let ((buf (find-file-noselect todo-file)))
      (with-current-buffer buf
        (rename-buffer buf-name t))
      (pop-to-buffer buf))))

(set-popup-rule!
"^\\*jupyter-outyut\\*$"
:side 'bottom
:size 0.2
:select t
:quit t
:ttl nil)

(use-package! apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(black)))

(map! :leader
       :desc "recent dired" "r d" #'dirvish-history-jump)

(after! flycheck
  (setq flycheck-indication-mode 'right-fringe)
  ;; Apply the same bitmap to all levels using a loop
  (dolist (level '(error warning info))
    (flycheck-define-error-level level
      :overlay-category (intern (format "flycheck-%s-overlay" level))
      :fringe-bitmap 'flycheck-fringe-bitmap-double-left-arrow
      :fringe-face (intern (format "flycheck-fringe-%s" level)))))

(setq enable-remote-dir-locals t)

(use-package! mini-echo
  :config
  (mini-echo-mode 1))

;; eglot setttings
(setq eglot-send-changes-idle-time 0.1)

(use-package! eglot
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-events-buffer-config '(:size 0)
        eglot-report-progress nil
        eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:inlayHintProvider))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "pyright-langserver" "--stdio")))

(use-package! citar)

;; (use-package! uv)

(use-package! claude-code
  :defer t
  :config
  (set-popup-rule! "\\*claude"
                  :side 'right
                  :size 0.33
                   :select t))

(after! org
  (set-popup-rule! "\\*Org Babel Results\\*"
                  :size 0.33
                  :select t))

(use-package! gptel
  :defer t
  :config
  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule! "^\\*ChatGPT\\*$"
        :side 'right :size 0.3 :select t :quit t :ttl nil))

(use-package! eat
  :defer t
  :config
  (setq! eat-term-name "xterm-256color")
  )

;; map j-k to evil-escape
(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

(after! vterm
  (add-hook 'vterm-mode-hook #'hack-dir-local-variables-non-file-buffer))

(after! embark-org
  (define-key embark-org-src-block-map (kbd "r") #'org-babel-open-src-block-result))

(setq! org-preview-latex-default-process 'dvisvgm)

(use-package! sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package! sideline
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t
        sideline-backends-right '(sideline-flycheck)
        flycheck-display-errors-function nil
        )
  :hook (
         (flycheck-mode . sideline-mode)   ; for `sideline-flycheck`
         ))            ; display the backend name

(use-package! org-inline-pdf
  :hook (org-mode . org-inline-pdf-mode))

(defun my/org-toggle-inline-image-at-point ()
  "Toggle inline image (incl. PDF via org-inline-pdf) for the link at point."
  (interactive)
  (let* ((ctx (org-element-context))
         (beg (org-element-property :begin ctx))
         (end (org-element-property :end   ctx)))
    (unless (and beg end) (user-error "No link at point"))
    ;; Ensure PDFs can render inline
    (unless (bound-and-true-p org-inline-pdf-mode)
      (org-inline-pdf-mode 1))
    (let ((has-img
           (seq-some (lambda (ov) (overlay-get ov 'org-image-overlay))
                     (overlays-in beg end))))
      (if has-img
          ;; HIDE: remove overlays in this link region
          (org-remove-inline-images beg end)
        ;; SHOW: render inline just for this link region
        (org-display-inline-images t t beg end)))))

(defun my/org-open-pdf-inline-toggle (_file &optional _link)
  (my/org-toggle-inline-image-at-point)
  t) ;; returning non-nil tells Org we handled it

(after! org
  (setq org-file-apps (assoc-delete-all "\\.pdf\\'" org-file-apps))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . my/org-open-pdf-inline-toggle)))

(setq! good-scroll-persist-vscroll-window-scroll 'nil)

(setq! org-journal-time-format ""
       org-journal-time-prefix "** TODO ")

(map! :leader
      :prefix ("n" . "notes") ;; or choose a better prefix name
      :desc "Find file in ./src/" "p"
      (lambda ()
        (interactive)
        (projectile-find-file-in-directory (expand-file-name "src/" (projectile-project-root)))))
