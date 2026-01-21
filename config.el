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
(setq doom-theme 'ef-rose-pine-dawn
      ;; (setq doom-theme 'doom-one
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

;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))

;; (setq org-latex-src-block-backend "listings")

(setq org-roam-directory "~/Documents/roam/note/")

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(setq org-agenda-files '("~/org/journal/"))
(setq org-journal-enable-agenda-integration t)

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


(setq org-image-actual-width nil)
(after! org
  ;; (setq! org-src-context-mode 1)
  (setq! org-pretty-entities nil)
  ;; start hl-todo-mode
  ;; disable org indent mode
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-heading-lvl nil)
  ;; (setq! org-download-method 'directory)
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


(setq! org-noter-notes-search-path '("/Users/jiawei/Documents/roam/booknotes"))
;; (setq citar-org-roam-capture-template-key "z")


;; citar configuration
(setq! org-cite-csl-styles-dir "~/Zotero/styles")
;; (setq! citar-bibliography '("/Users/jiawei/Documents/roam/biblibrary/references.bib"))
(setq citar-bibliography
      (list (expand-file-name "/Users/jiawei/Documents/roam/biblibrary/references.bib")))
;; (setq! citar-library-paths '("/Users/jiawei/Documents/roam/paper/"))
;; (setq! citar-notes-paths '("/Users/jiawei/Documents/roam/paper/"))
(setq citar-symbol-separator "  ")
;; (setq! citar-org-roam-subdir "paper/")
(use-package! citar
  :config
  (setq! citar-org-roam-note-title-template "${title}"))

(setq org-roam-capture-templates
      '(("n" "note" plain
         "* ${title}\n\n%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+created: %<%Y-%m-%d>")
         :unnarrowed t
         :empty-lines 1)))

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
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

;; initial frame size
(setq initial-frame-alist
      '((width . 150) (height . 50)))
;; Set the default frame width and height
(add-to-list 'default-frame-alist '(width . 170))  ; width set to 100 columns
(add-to-list 'default-frame-alist '(height . 60))  ; height set to 50 lines


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
  (setq! corfu-popupinfo-max-height 1)
  (setq corfu-auto nil
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-on-exact-match 'separator
        corfu-min-width 50
        corfu-max-width 80)
  ;; (map! :i "<tab>" #'completion-at-point)
  )

;; (after! dabbrev
;;   ;; This line adds a regex to ignore buffers ending in .csv for dabbrev
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'csv-mode))

(use-package! tab-bar
  :config
  (map! :leader :desc "tab bar mode" "t t" #'toggle-frame-tab-bar)
  (setq tab-bar-new-tab-choice t
        tab-bar-tab-name-truncated-max 20
        tab-bar-tab-hints t)
  (map! :n "]T" 'tab-bar-switch-to-next-tab)
  (map! :n "[T" 'tab-bar-switch-to-prev-tab)
  )


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
  (load! (expand-file-name "org-sliced-image-fix.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "citar_function.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "custom-functions" "~/.doom.d/lisp/"))
  )

(map! :n "s-;" #'skim-next-page)
(map! :n "s-'" #'skim-prev-page)
(setq org-image-actual-width nil)

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

(setq! org-image-actual-width nil)

(setq! TeX-command-extra-options "-shell-escape")

(setq! org-export-expand-links 'nil)

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

(add-hook 'prog-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-comment-face :slant 'italic)))

(use-package! rainbow-csv
  :hook (csv-mode . rainbow-csv-mode))

;; disable visual line mode in csv mode
(add-hook 'csv-mode-hook
          (lambda ()
            (when (eq major-mode 'csv-mode)
              (visual-line-mode -1))))

;; map space t b to breadcrumb-mode
(map! :leader :desc "breadcrumb mode" "t h" #'breadcrumb-mode)

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

;; (use-package! phscroll
;;   :hook (org-mode . org-phscroll-mode))

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

;; (defun insert-attachment-from-dir ()
;;   "Insert a link to a file from the current heading's attachment
;;    directory into the :ATTACHMENTS: property."
;;   (interactive)
;;   (require 'org-attach)
;;   (let* ((attach-dir (org-attach-dir t)) ;; Get the attachment dir, create if needed
;;          (files (when (file-directory-p attach-dir)
;;                   (directory-files attach-dir nil "^[^.]"))) ;; List non-dotfiles
;;          (file (completing-read "Choose attachment file: " files nil t))
;;          (link (concat "attachment:" file))
;;          (current (org-entry-get (point) "ATTACHMENTS")))
;;     ;; Update the ATTACHMENTS property
;;     (org-entry-put (point) "ATTACHMENTS"
;;                    (string-join (remove "" (list current (format "[[%s][%s]]" link file)))))))

(defun insert-attachment-link-from-dir ()
  "Insert a link to a file from the current heading's attachment directory at point."
  (interactive)
  (require 'org-attach)
  (let* ((attach-dir (org-attach-dir t)) ;; Create attachment dir if needed
         (files (when (file-directory-p attach-dir)
                  (directory-files attach-dir nil "^[^.]"))) ;; List non-dotfiles
         (file (completing-read "Choose attachment file: " files nil t))
         (link (format "[[attachment:%s][%s]]" file file)))
    (insert link)))

;; map to space m a i
(map! :leader :desc "insert attachment from dir" "m a i" #'insert-attachment-link-from-dir)

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

(use-package! org-latex-preview
  :hook (org-mode . org-latex-preview-mode)
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options :page-width 0.8)

  ;; ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-mode-ignored-commands
        '(next-line previous-line))

  ;; ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)
  
  (setq org-latex-preview-mode-display-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-mode-update-delay 0.25)

  (defun org--latex-preview-region (beg end)
    "Compatibility shim for old Org LaTeX preview function.
        Calls `org-latex-preview--preview-region' with a default
        processing type."
    (let ((processing-type org-latex-preview-process-default))
      (org-latex-preview--preview-region processing-type beg end)))
  )
(map! :leader :desc "macos open with default programe" "o m" #'+macos/open-in-default-program)

;; do not export when archived
(setq! org-export-with-archived-trees nil)

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

;; (use-package! phscroll
;;   :hook (org-mode . org-phscroll-mode)
;;   )

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
  )

(load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-remote.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-remote-test.el")
;; (load! "/Users/jiawei/Projects/Playground/flash_emacs/flash.emacs/flash-emacs-ts-search.el")

(defun flash-emacs--set-jump-before-jump (&rest _args)
  "Set a jump point before running `flash-emacs-jump`."
  (better-jumper-set-jump))

(advice-add 'flash-emacs-jump :before #'flash-emacs--set-jump-before-jump)

(setq avy-keys (append (number-sequence ?a ?z)))

(defvar-local my/project-todo-file nil
  "Path to the TODO file for the current project.")

;; map my/project-todo to space p t
(map! :leader :desc "Open project TODO" "p t" #'my/project-todo)

(set-popup-rule!
  "^\\*jupyter-output\\*$"
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
      :desc "recent dired" "r d" #'consult-dir)

(after! flycheck
  (setq flycheck-indication-mode 'right-fringe)
  ;; Apply the same bitmap to all levels using a loop
  (dolist (level '(error warning info))
    (flycheck-define-error-level level
      :overlay-category (intern (format "flycheck-%s-overlay" level))
      :fringe-bitmap 'flycheck-fringe-bitmap-double-left-arrow
      :fringe-face (intern (format "flycheck-fringe-%s" level)))))

;; eglot setttings
(setq eglot-send-changes-idle-time 0.1)

(use-package! eglot
  :config
  (setq eglot-events-buffer-config '(:size 0)
        eglot-report-progress nil
        eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:inlayHintProvider)))


(after! python
  (set-eglot-client! '(python-mode python-ts-mode)
                     ;; `("ty" "server")
                     ;; '("rass" "python")
                     '("basedpyright-langserver" "--stdio")
                     '("pyright-langserver" "--stdio")
                     '("pyright" "--stdio")
                     '("pyrefly" "lsp")
                     '("ruff" "server") "ruff-lsp"
                     "jedi-language-server"
                     "pylsp" "pyls"))

(use-package! citar)

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

;; (use-package! org-inline-pdf
;;   :hook (org-mode . org-inline-pdf-mode))

;; (defun my/org-toggle-inline-image-at-point ()
;;   "Toggle inline image (incl. PDF via org-inline-pdf) for the link at point."
;;   (interactive)
;;   (let* ((ctx (org-element-context))
;;          (beg (org-element-property :begin ctx))
;;          (end (org-element-property :end   ctx)))
;;     (unless (and beg end) (user-error "No link at point"))
;;     ;; Ensure PDFs can render inline
;;     (unless (bound-and-true-p org-inline-pdf-mode)
;;       (org-inline-pdf-mode 1))
;;     (let ((has-img
;;            (seq-some (lambda (ov) (overlay-get ov 'org-image-overlay))
;;                      (overlays-in beg end))))
;;       (if has-img
;;           ;; HIDE: remove overlays in this link region
;;           (org-remove-inline-images beg end)
;;         ;; SHOW: render inline just for this link region
;;         (org-display-inline-images t t beg end)))))

;; (defun my/org-open-pdf-inline-toggle (_file &optional _link)
;;   (my/org-toggle-inline-image-at-point)
;;   t) ;; returning non-nil tells Org we handled it

;; (after! org
;;   (setq org-file-apps (assoc-delete-all "\\.pdf\\'" org-file-apps))
;;   (add-to-list 'org-file-apps '("\\.pdf\\'" . my/org-open-pdf-inline-toggle)))

(setq! good-scroll-persist-vscroll-window-scroll 'nil)

(setq! org-journal-time-format ""
       org-journal-time-prefix "** TODO ")

(map! :leader
      :prefix ("n" . "notes") ;; or choose a better prefix name
      :desc "Find file in ./src/" "p"
      (lambda ()
        (interactive)
        (projectile-find-file-in-directory (expand-file-name "src/" (projectile-project-root)))))

(setq! uniquify-buffer-name-style 'post-forward)

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("myarticle"
                 "\\documentclass[11pt]{article}
                  \\usepackage[margin=1in]{geometry}
                  \\usepackage{amsmath}
                  \\usepackage{graphicx}
                  \\usepackage{hyperref}
                 \\usepackage{tikz}
                 \\usetikzlibrary{shapes, arrows, positioning, calc}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-latex-default-class "myarticle")

(setq org-file-apps
      '(( "\\.svg\\'" . default)
        (remote . emacs)
        (auto-mode . emacs)
        (directory . "open %s")   ;; ← use Finder instead of Dired
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)))

(after! org-roam
  ;; Slugify text the same way Org-roam does for filenames
  (defun my/org-roam-slugify (s)
    (org-roam-node-slug (org-roam-node-create :title s)))

  ;; Compute relative path from the node’s title
  ;; Example: "abc@paper" -> "paper/abc.org"
  ;;          "abc"       -> "abc.org"
  (defun my/org-roam-file-from-title (node)
    (let* ((title (or (org-roam-node-title node)
                      (plist-get org-capture-plist :title) ; fallback
                      ""))
           (parts (split-string title "@"))
           (base  (string-trim (car parts)))
           (sub   (and (cadr parts) (string-trim (cadr parts))))
           (base-slug (my/org-roam-slugify base))
           (sub-slug  (when sub (my/org-roam-slugify sub))))
      ;; Create subdir if needed
      (when sub-slug
        (let ((dir (expand-file-name sub-slug org-roam-directory)))
          (unless (file-directory-p dir)
            (make-directory dir t))))
      (if sub-slug
          (format "%s/%s.org" sub-slug base-slug)
        (format "%s.org" base-slug))))

  ;; Clean title so "#+title:" doesn’t keep the "@paper"
  (defun my/org-roam-clean-title (node)
    (let* ((raw (org-roam-node-title node))
           (base (car (split-string raw "@"))))
      (string-trim base)))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${my/org-roam-file-from-title}"
                              "#+title: ${my/org-roam-clean-title}\n")
           :unnarrowed t))))

(setq! org-latex-pdf-process
       '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f -synctex=1 -shell-escape"))

;; Put this once in your init.el
(defvar-local my/org-export-outfile nil)

(defun my/org-export-output-file-name (orig-fn ext &optional subtreep pub-dir)
  "Prefer #+EXPORT_FILE_NAME, else `my/org-export-outfile'; fallback to ORIG-FN."
  (let* ((kw (cdr (assoc "EXPORT_FILE_NAME"
                         (org-collect-keywords '("EXPORT_FILE_NAME")))))
         (file-local (car kw))                      ; first value if keyword present
         (choice (or file-local my/org-export-outfile)))
    (if (and choice (stringp choice) (not (string-empty-p choice)))
        (let* ((base (file-name-sans-extension choice))
               (target (concat base ext))
               ;; If absolute, keep it; if relative and pub-dir was provided (e.g. publish),
               ;; you *can* switch to (expand-file-name target pub-dir). For normal exports,
               ;; keep it relative to the Org file's directory (default-directory).
               (full (if (file-name-absolute-p target)
                         target
                       (expand-file-name target))))
          ;; ensure destination directory exists
          (when-let ((dir (file-name-directory full)))
            (unless (file-exists-p dir) (make-directory dir t)))
          full)
      (funcall orig-fn ext subtreep pub-dir))))
(advice-add 'org-export-output-file-name :around #'my/org-export-output-file-name)

;; (use-package! atomic-chrome
;;   :config
;;   (setq atomic-chrome-url-major-mode-alist
;;       '(("github\\.com" . gfm-mode)
;;         ("redmine" . textile-mode)
;;         ("overleaf.com" . LaTeX-mode)
;;         ))
;;   (setq! atomic-chrome-buffer-open-style 'full)
;;   

(setq! org-attach-dir-relative 't)

;; ;; Put this in your config.el
(with-eval-after-load 'org
  (dolist (pair '(("jupyter-R"     . r)
                  ("jupyter-r"     . r)    ; or `. r` if you use r-mode
                  ("jupyter-python" . python)
                  ("jupyter-julia"  . julia)))
    (add-to-list 'org-src-lang-modes pair)))

(defun my/org-roam-find-global-node ()
  "Temporarily use the global org-roam directory and find a node."
  (interactive)
  (let ((org-roam-directory (expand-file-name "~/Documents/roam/note/"))
        (org-roam-db-location (expand-file-name "~/Documents/roam/note/org-roam.db")))
    ;; Ensure the DB is loaded with correct path
    (org-roam-node-find)))

;; map to space n R
(map! :leader :desc "org-roam find global node" "n R" #'my/org-roam-find-global-node)

(setq! org-startup-with-latex-preview 't)

;; config.el
(use-package! evil-visual-mark-mode
  :load-path "/Users/jiawei/Projects/Playground/evil-visual-mark-mode"
  :config
  (evil-visual-mark-mode 1))


(use-package! evil-marker-persist
  :load-path "/Users/jiawei/Projects/Playground/evil-visual-mark-mode"
  )

;;; Peek breadcrumb until next keypress
(defvar-local my--breadcrumb-peek-armed nil)

(defun my--breadcrumb-peek-pre-command ()
  "One-shot disabler for `breadcrumb-mode` before the next command."
  (when my--breadcrumb-peek-armed
    (setq my--breadcrumb-peek-armed nil)
    (remove-hook 'pre-command-hook #'my--breadcrumb-peek-pre-command t)
    (breadcrumb-mode -1)))

(defun my/breadcrumb-peek ()
  "Enable `breadcrumb-mode` until the next keypress."
  (interactive)
  (breadcrumb-mode 1)
  (setq my--breadcrumb-peek-armed t)
  ;; Buffer-local hook so it only affects this buffer
  (add-hook 'pre-command-hook #'my--breadcrumb-peek-pre-command nil t))

(map! :leader
      :desc "Breadcrumb peek"
      "b b" #'my/breadcrumb-peek)

;; temporary fix pdf view mode error
;; https://github.com/vedang/pdf-tools/issues/283
(defvar org-format-latex-header nil)

(use-package! ef-themes
  :load-path "/Users/jiawei/Projects/Playground/ef-themes"
  )

(add-hook! '+popup-buffer-mode
           ;; only adjust when we really are in a popup window
           (when (+popup-window-p)
             (set-window-margins (selected-window) 2 2)
             ;; optional: make the change buffer-local & refresh
             (setq-local left-margin-width 2
                         right-margin-width 2)
             (set-window-buffer (selected-window) (current-buffer))))

(defun my/project-todo ()
  "Open the TODO.org file for the current project in a popup with a unique name."
  (interactive)
  (let* ((project-name (projectile-project-name))
         (todo-file my/project-todo-file)
         (buf-name (format "*TODO:%s*" project-name)))
    (set-popup-rule!
      (format "^\\*TODO:%s\\*$" (regexp-quote project-name))
      :side 'right :size 0.4 :select t :quit 'current :ttl 0 :autosave t)
    (let ((buf (find-file-noselect todo-file)))
      (with-current-buffer buf
        (rename-buffer buf-name t))
      (pop-to-buffer buf)
      ;; give the popup window visible fringes (pixels)
      (when-let ((win (get-buffer-window buf)))
        (set-window-fringes win 8 8)   ; adjust px to taste
        ;; if you like, keep indicators minimal:
        ;; (setq-local fringe-indicator-alist '((continuation . nil) (truncation . nil)))
        ))))

;; Highest-priority map in Doom (general-override-mode-map)
;; (map! :map override
;;       :i "C-j" #'next-line
;;       :i "C-k" #'previous-line)

;; map space o p to +dired/dirvish-side-and-follow
(map! :leader :desc "dirvish side and follow" "o p" #'+dired/dirvish-side-and-follow)

;; Put this in your config.el
;; (add-hook 'quickrun--mode-hook #'hack-dir-local-variables-non-file-buffer)
(add-hook 'vterm-mode-hook #'hack-dir-local-variables-non-file-buffer)

;; dirvish bug
;; Fix Dirvish yank under Emacs 31 where built-in `all` clashes
(with-eval-after-load 'dirvish
  (defun my/dirvish--all-like-and (&rest xs)
    "Compat for old `(all …)` used by some code.
Return non-nil iff XS is non-empty AND every element is non-nil."
    (and xs (cl-every #'identity xs)))

  (advice-remove 'dirvish-yank--apply #'ignore) ; if you added any earlier stub
  (advice-add 'dirvish-yank--apply :around
              (lambda (orig-fn &rest args)
                (cl-letf (((symbol-function 'all) #'my/dirvish--all-like-and))
                  (apply orig-fn args)))))


;; map space l a to my/org-export-pdf-to-attach-dir
(map! :leader :desc "org export pdf to attach dir" "l a" #'my/org-export-pdf-to-attach-dir)

(map! :leader
      :desc "Wrap Text with red"
      "l r" #'wrap-text-with-color)

;; remove 
(after! csv-mode
  (defun my/disable-rainbow-csv-for-large-files ()
    (when (and (eq major-mode 'csv-mode)
               (> (buffer-size) (* 1 1024 1024)))
      (when (bound-and-true-p rainbow-csv-mode)
        (rainbow-csv-mode -1))))
  (add-hook 'csv-mode-hook #'my/disable-rainbow-csv-for-large-files t))

(defun my/projectile-find-file-in-src ()
  "Find a file under the project's src/ directory."
  (interactive)
  (let* ((root (projectile-project-root))
         (src-dir (expand-file-name "src" root)))
    (if (file-directory-p src-dir)
        (projectile-find-file-in-directory src-dir)
      (user-error "No src/ directory in project root"))))

(map! :leader
      :desc "Find file in src/"
      "p s" #'my/projectile-find-file-in-src)


(after! vterm
  (set-popup-rule! "^\\*vterm:.*\\*$"
    :size 0.3
    :vslot -4
    :select t
    :quit t
    :ttl nil) ;; keep buffer alive when popup closes
  )

(map! :leader :desc "Project vterm" "o t" #'jc/vterm-project-toggle)

(defun jc/vterm-project-toggle ()
  "Toggle a project-local vterm popup.

One vterm per project root:
- If it's visible, hide its window(s).
- If it exists but is hidden, show it.
- If it doesn't exist yet, create it at the project root."
  (interactive)
  (let* ((proj-root (or (doom-project-root) default-directory))
         (proj-name (file-name-nondirectory (directory-file-name proj-root)))
         (buf-name (format "*vterm:%s*" proj-name))
         (buf      (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf t))
        ;; Hide all windows showing this vterm
        (dolist (win (get-buffer-window-list buf nil t))
          (delete-window win))
      ;; Otherwise create if needed and show it
      (progn
        (unless (buffer-live-p buf)
          (let ((default-directory proj-root))
            (setq buf (vterm buf-name)))
          ;; make Doom treat it like a normal/real buffer
          (with-current-buffer buf
            (setq-local doom-real-buffer-p t)))
        (pop-to-buffer buf)))))

;; temp fix for rsync dirvish 
(after! dirvish
  ;; Just don’t use the magic 'all symbol; restrict to marked files only.
  (setq dirvish-yank-sources (lambda () (dirvish-yank--get-srcs 'all))))

(defun cc/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))


;; map ] F to ns-next-frame
(map! :n "] F" #'ns-next-frame)
                                        ; map [ F to ns-previous-frame]
(map! :n "[ F" #'ns-prev-frame)

;; tramp trick
(load! (expand-file-name "tramp_optim.el" "~/.doom.d/lisp/"))
(setq enable-remote-dir-locals t)


;; fix a quickrun bug
(defun my-quickrun--insert-header-advice (process)
  "Insert header to PROCESS buffer with correct default-directory."
  (unless quickrun-output-only
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t)
            (time (substring (current-time-string) 0 19))
            ;; Get the actual execution directory
            (exec-dir (or quickrun-option-default-directory
                          (with-current-buffer quickrun--original-buffer
                            default-directory))))
        (goto-char (point-min))
        ;; Remove the incorrect header line if it exists
        (when (looking-at "-\\*-.*-\\*-\n")
          (delete-region (point) (line-end-position 2)))
        ;; Insert correct header
        (insert "-*- mode: quickrun-; default-directory: \""
                exec-dir
                "\" -*-\n")))))

;; Advice the function to run after it completes
(with-eval-after-load 'quickrun
  (advice-add 'quickrun--insert-header :after #'my-quickrun--insert-header-advice))


(defun lsp! ()
  "Dispatch to call the currently used lsp client entrypoint.
Skip remote (TRAMP) buffers silently."
  (unless (file-remote-p default-directory)
    (when (require 'eglot nil t)
      (if (eglot--lookup-mode major-mode)
          (eglot-ensure)
        (eglot--message "No client defined for %s" major-mode)))))


(use-package! projectile
  :config
  (setq projectile-indexing-method 'alien)
  (defun projectile-find-file-hook-function ()
    "Called by `find-file-hook' when `projectile-mode' is on.
This version also runs fully on remote files."
    (projectile-maybe-limit-project-file-buffers)
    (when projectile-dynamic-mode-line
      (projectile-update-mode-line))
    (when projectile-auto-update-cache
      (projectile-cache-files-find-file-hook))
    (projectile-track-known-projects-find-file-hook)
    (projectile-visit-project-tags-table)))

;; consut dir using zlua 
(use-package! consult-dir
  :init
  (setq consult-dir-default-command #'consult-dir-dired)
  :config
  (defvar consult-dir--source-zlua
    `(:name     "Zlua Dir"
      :narrow   ?z
      :category file
      :face     consult-file
      :history  file-name-history
      :enabled  ,(lambda () (getenv "ZLUA_SCRIPT"))
      :items
      ,(lambda ()
         (nreverse (mapcar
                    (lambda (p) (abbreviate-file-name (file-name-as-directory p)))
                    ;; REQUIRE export `ZLUA_SCRIPT' in parent-shell
                    (split-string (shell-command-to-string
                                   "lua $ZLUA_SCRIPT -l | perl -lane 'print $F[1]'")
                                  "\n" t)))))
    "Zlua directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir--source-zlua t))

(after! tramp-sh
  (setq! tramp-default-remote-shell "/bin/zsh"))


;; do not move when hl 
(defun jc/evil-hl-word-under-cursor ()
  "Like Vim's *, but only set/search-highlight
    the word under cursor; don't move point."
  (interactive)
  (save-excursion
    (evil-ex-search-word-forward 1)))

(map! :n "*" #'jc/evil-hl-word-under-cursor)

(defun my/projectile-switch-to-buffer (arg)
  "Switch to a project buffer.
With prefix argument ARG (C-u), include *special* buffers in the list."
  (interactive "P")
  (require 'consult)
  (let* ((buffers (projectile-project-buffers))
         (filtered (if arg
                       buffers
                     (cl-remove-if (lambda (b)
                                     (string-prefix-p "*" (buffer-name b)))
                                   buffers))))
    (switch-to-buffer
     (consult--read
      (consult--buffer-query :sort 'visibility
                             :predicate (lambda (buf) (memq buf filtered))
                             :as #'buffer-name)
      :prompt "Switch to buffer: "
      :history 'buffer-name-history
      :sort nil
      :category 'buffer
      :state (consult--buffer-state)))))

(map! :leader 
      :desc "projectile switch to buffer" 
      "TAB" #'my/projectile-switch-to-buffer)

(use-package! dirvish
  :config
  (setq! dired-kill-when-opening-new-dired-buffer t)
  )

(use-package! javelin
  :config
  (global-javelin-minor-mode 1))

(after! consult
  (defun my/consult-tmux-session ()
    "Pick a tmux session with Consult and switch/attach to it (by name)."
    (interactive)
    (require 'consult)
    (let* ((sessions (+tmux-list-sessions))
           (cands
            (mapcar (lambda (s)
                      (let* ((name (plist-get (cdr s) :name))
                             (attached (plist-get (cdr s) :attached))
                             (label (format "%s%s" name (if attached "  (attached)" ""))))
                        (cons label name)))
                    sessions))
           (choice (consult--read (mapcar #'car cands)
                                  :prompt "tmux session: "
                                  :sort nil
                                  :require-match t)))
      (when choice
        (let ((name (cdr (assoc choice cands))))
          (condition-case _
              (+tmux (format "switch-client -t %s" (shell-quote-argument name)))
            (error
             (+tmux (format "attach-session -t %s" (shell-quote-argument name))))))))))

(setq! diff-refine 'navigation)

;; map j and k to use next line and previous line of native emacs
(after! evil
  (define-key evil-normal-state-map (kbd "j") #'next-line)
  (define-key evil-normal-state-map (kbd "k") #'previous-line))

;; (add-to-list 'load-path "/Users/jiawei/Projects/Playground/emacs-tramp-rpc/lisp")
; (require 'tramp-rpc)
    
(use-package! mini-echo
  :config
  (mini-echo-mode 1))

(add-hook 'emacs-startup-hook #'global-hide-mode-line-mode)

(defvar per-project-compile-history nil)

(define-advice project-compile (:around (&rest args) project-local-history)
  (let* ((root (project-root (project-current t)))
         (project-hist (alist-get root per-project-compile-history nil nil #'equal))
         (compile-history project-hist)
         (compile-command (and project-hist
                               (car project-hist))))
    (unwind-protect
        (apply args)
      (setf (alist-get root per-project-compile-history nil nil #'equal) compile-history))))
