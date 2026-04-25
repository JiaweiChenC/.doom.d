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

;; Fix hl-line ghost highlights when switching workspaces
(after! hl-line
  (add-hook 'persp-before-switch-functions
            (lambda (&rest _)
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (remove-overlays (point-min) (point-max) 'face 'hl-line)
                  (when (bound-and-true-p hl-line-overlay)
                    (delete-overlay hl-line-overlay)
                    (setq hl-line-overlay nil))
                  (when (bound-and-true-p global-hl-line-overlay)
                    (delete-overlay global-hl-line-overlay)
                    (setq global-hl-line-overlay nil))))))

  (add-hook 'persp-activated-functions
            (lambda (&rest _)
              (run-at-time 0.05 nil
                           (lambda ()
                             (when (bound-and-true-p hl-line-mode)
                               (hl-line-highlight)))))))

;; Fix: killing a shared buffer in one workspace should not kill it globally.
;; Doom's doom--switch-to-fallback-buffer-maybe-a calls
;; run-hook-with-args-until-failure on kill-buffer-query-functions as a
;; pre-check.  persp-kill-buffer-query-function removes the buffer from the
;; current perspective and returns nil (preventing kill).  But Doom then falls
;; through and the real kill-buffer calls the hook AGAIN — the buffer is now
;; "foreign" and persp-kill-foreign-buffer-behaviour = kill lets it die.
;; This advice runs first: if the buffer lives in ANY other perspective we
;; remove it from the current one (if present) and switch away, preventing the
;; global kill entirely.
(defun my/workspace-aware-kill-current-buffer-a (&rest _)
  "Remove buffer from current workspace instead of killing when shared."
  (when (and (bound-and-true-p persp-mode)
             (not (minibufferp)))
    (let* ((buf (current-buffer))
           (persps (persp--buffer-in-persps buf))
           (current-persp (get-current-persp))
           (other-persps (remq current-persp persps)))
      (when other-persps
        ;; Buffer lives in other workspaces — never kill it globally.
        (when (memq current-persp persps)
          (persp-remove-buffer buf current-persp))
        ;; Switch to another buffer that belongs to this workspace.
        (let ((replacement
               (or (car (cl-remove-if
                         (lambda (b)
                           (or (eq b buf)
                               (not (buffer-live-p b))
                               (minibufferp b)))
                         (persp-buffers current-persp)))
                   (doom-fallback-buffer))))
          (switch-to-buffer replacement t))
        t))))

(advice-add 'kill-current-buffer :before-until
            #'my/workspace-aware-kill-current-buffer-a
            '((depth . -90)))

;; Also guard `kill-buffer' itself — other code paths (e.g. layout-buffer
;; advice, or direct M-x kill-buffer) can bypass kill-current-buffer.
(defun my/workspace-aware-kill-buffer-a (orig-fn &optional buffer-or-name)
  "Prevent `kill-buffer' from killing buffers that belong to other workspaces."
  (if (not (bound-and-true-p persp-mode))
      (funcall orig-fn buffer-or-name)
    (let* ((buf (or (and buffer-or-name
                         (get-buffer buffer-or-name))
                    (current-buffer)))
           (persps (and (buffer-live-p buf) (persp--buffer-in-persps buf)))
           (current-persp (get-current-persp))
           (other-persps (remq current-persp persps)))
      (if (not other-persps)
          ;; Buffer only in current workspace (or none) — kill normally.
          (funcall orig-fn buffer-or-name)
        ;; Buffer is in other workspaces — just remove from current.
        (when (memq current-persp persps)
          (persp-remove-buffer buf current-persp))
        ;; If this buffer is displayed, switch its windows away.
        (dolist (win (get-buffer-window-list buf nil nil))
          (with-selected-window win
            (switch-to-prev-buffer win t)))
        t))))

(advice-add 'kill-buffer :around #'my/workspace-aware-kill-buffer-a
            '((depth . -90)))

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

;;; Core UI and editing behavior

(after! cdlatex
  (add-to-list 'cdlatex-math-modify-alist '( ?s "\\boldsymbol"  nil  t t nil ))
  (add-to-list 'cdlatex-math-modify-alist '( ?n "\\mathbb"      nil  t t nil )))

(after! texmathp
  (add-to-list 'texmathp-tex-commands '("tikzpicture" env-on)))

;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

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
               "R" #'org-roam-capture
               "m" #'my/org-roam-move-node))

;;; Secrets and local overrides

(when (file-exists-p "~/.emacs.d/.secret.el")
  (load "~/.emacs.d/.secret.el"))

(use-package! mathpix.el
  :custom ((mathpix-app-id mathpix-app-id)
           (mathpix-app-key mathpix-app-key))
  :bind
  ("C-x m" . mathpix-screenshot))

(setq mathpix-screenshot-method "screencapture -i %s")

(use-package! super-save
  :defer t
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers '+vterm/toggle)
    (setq super-save-remote-files nil)
(setq auto-save-default nil)
  )



;; vertico is already configured by Doom's vertico module;
;; just set the resize option here.
(setq vertico-resize 'grow-only)

;;; Org, citation, and roam

(setq org-image-max-width 1000)
(after! org
  (setopt org-pretty-entities nil)
  ;; start hl-todo-mode
  ;; disable org indent mode
  (setopt org-startup-folded 'content)
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-heading-lvl nil)
  ;; (setopt org-download-method 'directory)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (defadvice! recover-paragraph-seperate ()
    "Recover org paragraph mark position."
    :after 'org-setup-filling
    (setq-local paragraph-start "[\f\\|[ \t]*$]")
    (setq-local paragraph-separate "[ \t\f]*$"))
  (setq org-image-actual-width '(500))
  (org-link-set-parameters "zotero"
                           :follow (lambda (url arg) (browse-url (format "zotero:%s" url) arg)))
  (org-link-set-parameters "skim"
                           :follow (lambda (url arg) (browse-url (format "skim:%s" url) arg)))
  (map! :map org-mode-map
        "C-M-y" #'zz/org-download-paste-clipboard)
  )

(defadvice! +org/insert-item-below-with-newline (fn direction)
  "Insert an extra blank line before a new heading inserted below."
  :around #'+org--insert-item
  (funcall fn direction)
  (when (and (eq direction 'below) (org-at-heading-p))
    (save-excursion
      (beginning-of-line)
      (insert "\n"))))


(use-package! ef-themes
  :load-path "/Users/jiawei/Projects/Playground/ef-themes"
  )

;; citar configuration
(setopt org-cite-csl-styles-dir "~/Zotero/styles")
(setq citar-bibliography
      (list (expand-file-name "/Users/jiawei/Documents/roam/biblibrary/references.bib")))
(use-package! citar
  :config
  (setopt citar-org-roam-note-title-template "${title}"))

(setq org-roam-capture-templates
      '(("n" "note" plain
         "* ${title}\n\n%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+created: %<%Y-%m-%d>")
         :unnarrowed t
         :empty-lines 1)))

;;; AI tools and assistants

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

;;; Window/frame and shell helpers

(load! (expand-file-name "layout-buffer-consult" "~/.doom.d/lisp/"))

;; set the default frame
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; bind quickrun to space r r
(map! :leader :desc "quickrun" "r r" #'quickrun)

;; bind quickrun kill process to space r k
(map! :leader :desc "quickrun kill process" "r k" #'quickrun--kill-running-process)

(setq org-journal-file-type 'monthly)

;; initial frame size
(setq initial-frame-alist
      '((width . 150) (height . 50)))
;; Set the default frame width and height
(add-to-list 'default-frame-alist '(width . 170))  ; width set to 100 columns
(add-to-list 'default-frame-alist '(height . 60))  ; height set to 50 lines

(defun jc/frame-snap-right ()
  "Set the current frame to a saved position and size (right half of screen)."
  (interactive)
  (set-frame-position (selected-frame) 1306 80)
  (set-frame-size (selected-frame) 1526 1300 t))

(global-set-key (kbd "C-M-l") #'jc/frame-snap-right)

(defun jc/frame-snap-center ()
  "Set the current frame to a saved position and size."
  (interactive)
  (set-frame-position (selected-frame) 809 93)
  (set-frame-size (selected-frame) 1518 1294 t))

(global-set-key (kbd "C-M-;") #'jc/frame-snap-center)


;; after python mode, start evil vimish fold mode
(add-hook 'python-mode-hook #'evil-vimish-fold-mode)

(setopt quickrun-timeout-seconds 1000000)

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

;;; macOS integration

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
(map! :leader :desc "citar open files" "o z" #'my/citar-open-pdf)

;;; Completion and tab UI

(use-package! corfu
  ;; :init
  ;; (corfu-global-mode)
  :config
  (setopt corfu-popupinfo-max-height 1)
  (setq corfu-auto nil
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-on-exact-match 'separator
        corfu-min-width 50
        corfu-max-width 80)
  ;; (map! :i "<tab>" #'completion-at-point)
  )

(use-package! tab-bar
  :config
  (map! :leader :desc "tab bar mode" "t t" #'toggle-frame-tab-bar)
  (setq tab-bar-new-tab-choice t
        tab-bar-tab-name-truncated-max 20
        tab-bar-tab-hints t)
  (map! :n "]T" 'tab-bar-switch-to-next-tab)
  (map! :n "[T" 'tab-bar-switch-to-prev-tab)
  )

(setopt ess-startup-directory 'default-directory)

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

(setopt dired-mouse-drag-files 'move)

(add-hook 'doom-load-theme-hook
          (lambda ()
            (set-face-background 'fringe (face-attribute 'default :background))))


;; Defer my-quickrun.el until quickrun is actually loaded
(after! quickrun
  (load! (expand-file-name "my-quickrun.el" "~/.doom.d/lisp/")))
;; load mytex.el after org
(after! org
  (add-to-list 'org-file-apps '("\\.svg\\'" . default))
  (load! (expand-file-name "mytex.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "skim.el" "~/.doom.d/lisp/"))
  (load! ".secret.el")
  (load! (expand-file-name "babel.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "org-sliced-image-fix.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "gterm_config" "~/.doom.d/lisp/"))
  (load! (expand-file-name "citar_function.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "custom-functions" "~/.doom.d/lisp/"))
  )

(map! :n "s-;" #'skim-next-page)
(map! :n "s-'" #'skim-prev-page)
(setopt org-highlight-latex-and-related '(native latex script entities))

(setq tex-fontify-script 'nil)

(setq org-latex-caption-above '(table src-block special-block math))

;; bind space b return to embard open bookmark external
(map! :leader :desc "open bookmark external" "e m" #'embark-bookmark-open-externally)

(setopt org-modern-table nil)
(setopt org-modern-block-fringe nil)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-autolinks t)
  )

(setopt TeX-command-extra-options "-shell-escape")

(setopt org-export-expand-links 'nil)

(setopt vterm-timer-delay 0.01)

;; space t e to mini-echo-mode
(map! :leader :desc "toggle mini echo mode" "t e" #'mini-echo-mode)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(after! org
  (define-key org-mode-map (kbd "S-s-<mouse-1>") 'hermanhelf-org-jump-to-pdf))

(add-hook 'prog-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-comment-face :slant 'italic)))

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

(setopt evil-kill-on-visual-paste nil)

(custom-set-variables
 '(zoom-size '(0.8 . 0.8)))

(use-package! phscroll
  :hook (org-mode . org-phscroll-mode))

(setopt visual-fill-column-width 100)

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


(setopt doom-big-font-increment 2)

(map! :n "C-;" #'send-scroll-up-to-other-window)
(map! :n "C-'" #'send-scroll-down-to-other-window)

(use-package! org-modern-indent
  :hook (org-mode . org-modern-indent-mode))

(use-package! org-modern
  :defer t
  :init
  (setopt org-modern-tag nil)
  )

;; ;;; Org Babel and Jupyter

;; (with-eval-after-load 'ob-jupyter
;;   (org-babel-jupyter-aliases-from-kernelspecs)
;;   )

;; ;; hack to make jupyter work with images
;; ;; https://github.com/emacs-jupyter/jupyter/issues/558
;; (defun skip-undo (orig-fun &rest args)
;;   "Execute ORIG-FUN with ARGS without recording undo information."
;;   (let ((buffer-undo-list t)) ; Temporarily disable undo recording
;;     (apply orig-fun args)))

;; (advice-add 'jupyter-generate-request :around #'skip-undo)

;; attachment
(setq org-attach-auto-tag nil)

(defun insert-attachment-link-from-dir ()
  "Insert a link to a file from the current heading's attachment directory at point."
  (interactive)
  (require 'org-attach)
  (let* ((attach-dir (org-attach-dir t)) ;; Create attachment dir if needed
         (files (when (file-directory-p attach-dir)
                  (directory-files attach-dir nil "^[^.]"))) ;; List non-dotfiles
         (file (completing-read "Choose attachment file: " files nil t))
         (link-name (read-string "Link name: " file))
         (link (format "[[attachment:%s][%s]]" file link-name)))
    (insert link)))

;; map to space m a i
(map! :leader :desc "insert attachment from dir" "m a i" #'insert-attachment-link-from-dir)

;; make copilot ignore .eld file
(setopt copilot-max-char-warning-disabled t)

(map! :after evil-org
      :map evil-org-mode-map
      :n "C-j" #'electric-newline-and-maybe-indent
      :n "C-k" #'kill-line)

;; add header-args to org default properties
(after! org
  (add-to-list 'org-default-properties "HEADER-ARGS"))

(setopt envrc-remote t)

;; Fix org-persist--add-to-index bug: :hash and :key lookups are
;; indexed by `inode' instead of their actual values, so persist
;; cache lookups by key (used by org-latex-preview) always miss.
;; Upstream bug in org-persist.el lines 587-588.
(after! org-persist
  (defun org-persist--add-to-index (collection &optional hash-only)
    "Add or update COLLECTION in `org-persist--index'.
When optional HASH-ONLY is non-nil, only modify the hash table."
    (org-persist-collection-let collection
      (let ((existing (org-persist--find-index collection)))
        (if existing
            (progn
              (plist-put existing :container container)
              (plist-put (plist-get existing :associated) :file path)
              (plist-put (plist-get existing :associated) :inode inode)
              (plist-put (plist-get existing :associated) :hash hash)
              (plist-put (plist-get existing :associated) :key key)
              existing)
          (unless hash-only (push collection org-persist--index))
          (unless org-persist--index-hash
            (setq org-persist--index-hash (make-hash-table :test 'equal)))
          (dolist (cont
                   (if (listp (car container))
                       (cons container container)
                     (list container)))
            (puthash (cons cont associated) collection org-persist--index-hash)
            (when path (puthash (cons cont (list :file path)) collection org-persist--index-hash))
            (when inode (puthash (cons cont (list :inode inode)) collection org-persist--index-hash))
            (when hash (puthash (cons cont (list :hash hash)) collection org-persist--index-hash))
            (when key (puthash (cons cont (list :key key)) collection org-persist--index-hash)))
          collection))))

  ;; Rebuild the hash table so entries written by the buggy upstream
  ;; code get correct :key/:hash shortcut lookups.
  (when org-persist--index
    (let ((ht (make-hash-table :test 'equal)))
      (dolist (collection org-persist--index)
        (org-persist-collection-let collection
          (dolist (cont
                   (if (and container (listp (car container)))
                       (cons container container)
                     (list container)))
            (puthash (cons cont associated) collection ht)
            (when path (puthash (cons cont (list :file path)) collection ht))
            (when inode (puthash (cons cont (list :inode inode)) collection ht))
            (when hash (puthash (cons cont (list :hash hash)) collection ht))
            (when key (puthash (cons cont (list :key key)) collection ht)))))
      (setq org-persist--index-hash ht))))

(use-package! org-latex-preview
  :hook (org-mode . org-latex-preview-mode)
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options :page-width 0.8)
  (plist-put org-latex-preview-appearance-options :zoom 1.1)

  ;; Fix org-block bg bleeding around LaTeX preview SVGs:
  ;; a separate high-priority overlay covers the region with default bg.
  (add-hook 'org-latex-preview-overlay-update-functions
            (lambda (ov)
              (when (overlay-get ov 'display)
                (let* ((s (overlay-start ov))
                       (e (overlay-end ov))
                       (end (if (eq (char-after e) ?\n) (1+ e) e))
                       (bg (face-attribute 'default :background)))
                  (unless (seq-find (lambda (o) (overlay-get o 'latex-preview-bg))
                                   (overlays-at s))
                    (let ((cover (make-overlay s end nil nil t)))
                      (overlay-put cover 'face (list :background bg :extend t))
                      (overlay-put cover 'priority 1)
                      (overlay-put cover 'latex-preview-bg t)
                      (overlay-put cover 'evaporate t)))))))

  ;; ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-mode-ignored-commands
        '(next-line previous-line))

  ;; ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)
  
  (setq org-latex-preview-mode-display-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-mode-update-delay 0.25)

  ;; Fix: dvisvgm's \special{dvisvgm:currentcolor on} does NOT persist
  ;; across DVI pages.  Only the first fragment in a batch receives the
  ;; \color + \special commands (via :continue-color optimisation), so
  ;; subsequent SVGs are rendered with hardcoded black instead of
  ;; currentColor.  Disable :continue-color for dvisvgm so every
  ;; fragment gets the special and its SVG uses currentColor.
  (define-advice org-latex-preview--tex-styled
      (:around (orig-fn processing-type value appearance-options)
               fix-dvisvgm-currentcolor)
    (when (and (eq processing-type 'dvisvgm)
               (plist-get appearance-options :continue-color))
      (setq appearance-options (copy-sequence appearance-options))
      (plist-put appearance-options :continue-color nil))
    (funcall orig-fn processing-type value appearance-options))


  (defun org--latex-preview-region (beg end)
    "Compatibility shim for old Org LaTeX preview function.
        Calls `org-latex-preview--preview-region' with a default
        processing type."
    (let ((processing-type org-latex-preview-process-default))
      (org-latex-preview--preview-region processing-type beg end)))

  )
(defvar my/remote-open-extra-env
  '(("100.105.242.51" . (("DISPLAY" . ":0")
                          ("WAYLAND_DISPLAY" . "wayland-0"))))
  "Extra GUI environment variables for remote `xdg-open' calls.
Each entry is keyed by the TRAMP host name.")

(defun my/open-target-at-point-or-default-directory ()
  "Return the file at point in Dired, or the current buffer target.
Falls back to `default-directory' when the current buffer has no file."
  (if (derived-mode-p 'dired-mode)
      (dired-get-file-for-visit)
    (or (buffer-file-name)
        default-directory
        (user-error "No file or directory is associated with this buffer"))))

(defun my/remote-open--env-exports (path)
  "Build shell export statements for PATH's remote host."
  (mapconcat
   (lambda (binding)
     (format "export %s=%s;" (car binding) (shell-quote-argument (cdr binding))))
   (alist-get (file-remote-p path 'host) my/remote-open-extra-env nil nil #'string=)
   " "))

(defun my/remote-open-with-default-program (path)
  "Open remote PATH on the remote desktop using `xdg-open'."
  (let* ((default-directory (if (file-directory-p path)
                                (file-name-as-directory path)
                              (file-name-directory path)))
         (target (shell-quote-argument (file-local-name path)))
         (buffer (generate-new-buffer " *my remote open*"))
         (script
          (format
           (concat
            "uid=$(id -u); "
            "runtime_dir=${XDG_RUNTIME_DIR:-/run/user/$uid}; "
            "export XDG_RUNTIME_DIR=\"$runtime_dir\"; "
            "export DBUS_SESSION_BUS_ADDRESS=${DBUS_SESSION_BUS_ADDRESS:-unix:path=$runtime_dir/bus}; "
            "%s"
            "if command -v xdg-open >/dev/null 2>&1; then "
            "  setsid -f xdg-open %s >/dev/null 2>&1; "
            "elif command -v gio >/dev/null 2>&1; then "
            "  setsid -f gio open %s >/dev/null 2>&1; "
            "else "
            "  printf 'Missing xdg-open or gio\\n' >&2; exit 127; "
            "fi")
           (my/remote-open--env-exports path)
           target
           target))
         (process
          (start-file-process-shell-command
           "my-remote-open"
           buffer
           (format "sh -lc %s" (shell-quote-argument script)))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (let ((proc-buffer (process-buffer proc)))
         (unless (process-live-p proc)
           (unwind-protect
               (unless (zerop (process-exit-status proc))
                 (message "Remote open failed: %s"
                          (string-trim
                           (if (buffer-live-p proc-buffer)
                               (with-current-buffer proc-buffer
                                 (buffer-string))
                             ""))))
             (when (buffer-live-p proc-buffer)
               (kill-buffer proc-buffer)))))))
    (message "Opening %s on %s"
             (file-local-name path)
             (file-remote-p path 'host))))

(defun my/+macos-open-with-remote-a (orig-fn &optional app-name path)
  "Use remote desktop openers for TRAMP PATHs.
Falls back to ORIG-FN for local paths."
  (let ((target (or path (my/open-target-at-point-or-default-directory))))
    (if (file-remote-p target)
        (my/remote-open-with-default-program target)
      (funcall orig-fn app-name path))))

(advice-remove '+macos-open-with #'my/+macos-open-with-remote-a)
(advice-add '+macos-open-with :around #'my/+macos-open-with-remote-a)

(map! :leader
      :desc "open current dir" "o o" #'+macos/reveal-in-finder
      :desc "open in default program" "o m" #'+macos/open-in-default-program)

;; do not export when archived
(setopt org-export-with-archived-trees nil)

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

(use-package! flash-emacs                                                                                                                 
:load-path "/Users/jiawei/Projects/Playground/flash-emacs"                                                                              
:defer t                                                                                                                                
:commands (flash-emacs-jump flash-emacs-ts-jump)                                                                                        
:init                                                                                                                                   
(after! evil                                                                                                                            
    (define-key evil-normal-state-map (kbd "s") #'flash-emacs-jump)                                                                       
    (define-key evil-insert-state-map (kbd "C-s") #'flash-emacs-jump)                                                                     
    (define-key evil-normal-state-map (kbd "S") #'flash-emacs-ts-jump))                                                                   
:config                                                                                                                                 
(require 'flash-emacs-remote)                                                                                                           
(require 'flash-emacs-ts)                                                                                                               
(require 'flash-emacs-search)                                                                                                           
(setopt flash-emacs-ts-rainbow-enabled t))     

;;; Evil navigation and jump behavior

(after! evil
  ;; disable evil surround global mode
  (global-evil-surround-mode -1)
  ;; flash-emacs keybindings are set in use-package! flash-emacs above
  )

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

;;; Project and coding tooling

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
                     '("pyright-langserver" "--stdio")
                     '("basedpyright-langserver" "--stdio")
                     '("pyright" "--stdio")
                     '("pyrefly" "lsp")
                     '("ruff" "server") "ruff-lsp"
                     "jedi-language-server"
                     "pylsp" "pyls"))

(after! org
  (set-popup-rule! "\\*Org Babel Results\\*"
    :size 0.33                   
    :select t))

(use-package! gptel
  :defer t
  :config                        
  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (setq gptel-model "gpt-5.4-pro")
  (set-popup-rule! "^\\*ChatGPT\\*$"
    :side 'right :size 0.3 :select t :quit t :ttl nil))

(use-package! eat                
  :defer t                       
  :config
  (setopt eat-term-name "xterm-256color")
  )

;; map j-k to evil-escape        
(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

(defun jc/vterm-apply-dir-locals-maybe ()
  "Apply dir-locals for local vterm buffers only."
  (unless (file-remote-p default-directory)
    (hack-dir-local-variables-non-file-buffer)))

(after! vterm
  (remove-hook 'vterm-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'vterm-mode-hook #'jc/vterm-apply-dir-locals-maybe)

  (defun jc/vterm-preserve-shell-cursor-shape ()
    "Let `vterm' keep owning cursor shape in this buffer."
    ;; Keep Doom's Emacs-state color, but don't let Evil continuously force a
    ;; cursor shape in `vterm'.
    (setq-local evil-emacs-state-cursor #'+evil-emacs-cursor-fn)
    (add-hook 'evil-emacs-state-entry-hook #'jc/vterm-emacs-state-line-cursor nil t))

  (defun jc/vterm-emacs-state-line-cursor ()
    "Default the cursor to a line when entering Emacs state in `vterm'."
    ;; Switching from Evil normal state can leave a box cursor behind.  Reset it
    ;; once on entry; after that, the shell's own cursor-shape escapes can take
    ;; over again.
    (when (derived-mode-p 'vterm-mode)
      (setq-local cursor-type 'bar)))

  (add-hook 'vterm-mode-hook #'jc/vterm-preserve-shell-cursor-shape))

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

(setopt good-scroll-persist-vscroll-window-scroll 'nil)

;;; Org export and file association tuning
(setopt org-journal-time-format ""
       org-journal-time-prefix "** TODO ")

(map! :leader
      :prefix ("n" . "notes") ;; or choose a better prefix name
      :desc "Find file in ./src/" "p"
      (lambda ()
        (interactive)
        (projectile-find-file-in-directory (expand-file-name "src/" (projectile-project-root)))))

(setopt uniquify-buffer-name-style 'post-forward)

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

  ;; Compute relative path from the node's title
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

  ;; Clean title so "#+title:" doesn't keep the "@paper"
  (defun my/org-roam-clean-title (node)
    (let* ((raw (org-roam-node-title node))
           (base (car (split-string raw "@"))))
      (string-trim base)))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${my/org-roam-file-from-title}"
                              "#+title: ${my/org-roam-clean-title}\n")
           :unnarrowed t))))

(setopt org-latex-pdf-process
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

(setopt org-attach-dir-relative 't)

;; ;; Put this in your config.el
;; (with-eval-after-load 'org
;;   (dolist (pair '(("jupyter-R"     . r)
;;                   ("jupyter-r"     . r)    ; or `. r` if you use r-mode
;;                   ("jupyter-python" . python)
;;                   ("jupyter-julia"  . julia)))
;;     (add-to-list 'org-src-lang-modes pair)))

(defun my/org-roam-find-global-node ()
  "Temporarily use the global org-roam directory and find a node."
  (interactive)
  (let ((org-roam-directory (expand-file-name "~/Documents/roam/note/"))
        (org-roam-db-location (expand-file-name "~/Documents/roam/note/org-roam.db")))
    ;; Ensure the DB is loaded with correct path
    (org-roam-node-find)))

;; map to space n R
(map! :leader :desc "org-roam find global node" "n R" #'my/org-roam-find-global-node)

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

;; map space o p to +dired/dirvish-side-and-follow
(map! :leader :desc "dirvish side and follow" "o p" #'+dired/dirvish-side-and-follow)

;; Put this in your config.el
(add-hook 'quickrun--mode-hook #'hack-dir-local-variables-non-file-buffer)

;; dirvish bugs

;; Fix subtree-toggle on files: dirvish-subtree--readin returns "" for
;; non-directories, so the file-error path in dirvish-subtree-toggle is
;; never reached and files are silently treated as empty dirs.
(after! dirvish-subtree
  (defun jc/dirvish-subtree-toggle-a (orig-fn)
    "Check file-directory-p before attempting subtree insert."
    (if (file-directory-p (dired-get-filename nil t))
        (funcall orig-fn)
      (dirvish-subtree--view-file)))
  (advice-add 'dirvish-subtree-toggle :around #'jc/dirvish-subtree-toggle-a))

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
    :quit nil
    :ttl nil) ;; keep buffer alive when popup closes
  (defun my/vterm-popup-send-escape ()
    "Send ESC to vterm even when the buffer is managed as a popup."
    (when (and (bound-and-true-p +popup-buffer-mode)
               (derived-mode-p 'vterm-mode))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map +popup-buffer-mode-map)
        (define-key map (kbd "<escape>") #'vterm-send-escape)
        (setq-local minor-mode-overriding-map-alist
                    (assq-delete-all '+popup-buffer-mode minor-mode-overriding-map-alist))
        (push (cons '+popup-buffer-mode map) minor-mode-overriding-map-alist))))
  (add-hook 'vterm-mode-hook #'my/vterm-popup-send-escape)
  )

(map! :leader :desc "Project vterm" "o t" #'jc/vterm-project-toggle)

(defun jc/vterm-project-toggle ()
  "Toggle a project-local vterm popup.

One vterm per project root:
- If it's visible, hide its window(s).
- If it exists but is hidden, show it.
- If it doesn't exist yet, create it at the project root."
  (interactive)         
  (let* ((proj-root (or (and (fboundp 'jc/project-root-for-dir)
                             (jc/project-root-for-dir default-directory))
                        (doom-project-root default-directory)
                        default-directory))
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
            (setq-local jc/vterm-origin-prefix (file-remote-p proj-root))
            (setq-local jc/project-root-local
                        (when (file-remote-p proj-root)
                          (file-name-as-directory
                           (tramp-file-name-localname
                            (tramp-dissect-file-name proj-root)))))
            (setq-local doom-real-buffer-p (not (file-remote-p default-directory)))))
        (with-current-buffer buf
          (when (fboundp 'jc/vterm-use-shell-vi)
            (jc/vterm-use-shell-vi)))
        (pop-to-buffer buf)))))

;; temp fix for rsync dirvish 
(after! dirvish         
  ;; Just don't use the magic 'all symbol; restrict to marked files only.
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


;;; Core command overrides

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
  (defun my/projectile-switch-project-find-file ()
    "Find a file from the project chosen by `projectile-switch-project'."
    ;; Use Doom's helper so the selected project root is bound explicitly.
    (doom-project-find-file default-directory))

  (setq projectile-switch-project-action #'my/projectile-switch-project-find-file)

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

;;; Projectile and Consult

;; Consult config
;; consult-dir with zlua source
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
  (setq consult-dir-sources
        (list 'consult-dir--source-recentf
              'consult-dir--source-project
              'consult-dir--source-default
              'consult-dir--source-bookmark
              'consult-dir--source-tramp-ssh
              'consult-dir--source-tramp-local)))

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


;;; TRAMP optimizations
(load! (expand-file-name "tramp_optim.el" "~/.doom.d/lisp/"))
;; TRAMP's internal command channel needs a fast, dumb shell — not ZSH.
;; ZSH loads .zshrc/.zprofile (slow), has complex prompts (TRAMP hangs
;; during prompt detection), and is unnecessary for non-interactive use.
;; Interactive shells (vterm) already use login-shell via vterm-tramp-shells.
(after! tramp-sh
  (setopt tramp-default-remote-shell "/bin/sh"))

;; You already configure ControlMaster in ~/.ssh/config.
;; When TRAMP also adds its own CM options, they can conflict (different
;; socket paths, double negotiation), causing hangs.  Let SSH handle it.
(after! tramp
  (setopt tramp-use-ssh-controlmaster-options nil))

;;; Evil search enhancements

;; do not move when hl 
(defun jc/evil-hl-word-under-cursor ()
  "Like Vim's *, but only set/search-highlight
    the word under cursor; don't move point."
  (interactive)         
  (save-excursion
    (evil-ex-search-word-forward 1)))

(map! :n "*" #'jc/evil-hl-word-under-cursor)

(defface jc/evil-search-count-face
  '((t (:foreground "#8aadf4")))
  "Face for Evil search count overlay.")

(defvar-local jc/evil-search-count-overlay nil
  "Overlay used to display [x/total] for Evil searches.")

(defun jc/evil-search--clear-count-overlay ()
  (when (overlayp jc/evil-search-count-overlay)
    (delete-overlay jc/evil-search-count-overlay)
    (setq jc/evil-search-count-overlay nil)))

(defun jc/evil-search--ensure-count-overlay (pos)
  (if (overlayp jc/evil-search-count-overlay)
      (move-overlay jc/evil-search-count-overlay pos pos)
    (setq jc/evil-search-count-overlay (make-overlay pos pos))
    (overlay-put jc/evil-search-count-overlay 'priority 1002)))

(defun jc/evil-search--count (pattern beg)
  (when (and pattern beg)
    (let ((regex (evil-ex-pattern-regex pattern))
          (case-fold-search (evil-ex-pattern-ignore-case pattern))
          (current 0)
          (total 0))
      (save-match-data
        (save-excursion 
          (goto-char (point-min))
          (while (and regex (re-search-forward regex nil t))
            (setq total (1+ total))
            (when (= (match-beginning 0) beg)
              (setq current total)))))
      (when (> total 0) 
        (list (max current 1) total)))))

(defun jc/evil-search--apply-count (pattern beg end)
  (if (and pattern beg end)
      (let* ((counts (jc/evil-search--count pattern beg))
             (pos (save-excursion (goto-char beg) (line-end-position))))
        (jc/evil-search--ensure-count-overlay pos)
        (overlay-put jc/evil-search-count-overlay 'after-string
                     (when counts
                       (propertize (format " [%d/%d]" (car counts) (cadr counts))
                                   'face 'jc/evil-search-count-face)))
        (add-hook 'pre-command-hook #'jc/evil-search--cleanup-count-overlay nil t))
    (jc/evil-search--clear-count-overlay)))

(defun jc/evil-search--cleanup-count-overlay ()
  (unless (memq this-command
                '(evil-ex-search-next
                  evil-ex-search-previous
                  evil-ex-search-forward
                  evil-ex-search-backward
                  evil-ex-search-word-forward
                  evil-ex-search-word-backward))
    (jc/evil-search--clear-count-overlay)
    (remove-hook 'pre-command-hook #'jc/evil-search--cleanup-count-overlay t)))

(defun jc/evil-search--after-update (pattern _offset beg end)
  (jc/evil-search--apply-count pattern beg end))

(defun jc/evil-search--after-search (&rest _args)
  (when (and evil-ex-search-pattern (match-beginning 0))
    (jc/evil-search--apply-count evil-ex-search-pattern
                                 (match-beginning 0)
                                 (match-end 0))))

(defvar jc/evil-search--target-buffer nil
  "Buffer where search count overlay was created.")

(defun jc/evil-search--minibuffer-exit ()
  "Clear search count overlay when exiting minibuffer."
  (when jc/evil-search--target-buffer
    (let ((buf jc/evil-search--target-buffer))
      (setq jc/evil-search--target-buffer nil)
      (when (buffer-live-p buf)
        (let ((ov (buffer-local-value 'jc/evil-search-count-overlay buf)))
          (when (overlayp ov)
            (delete-overlay ov)))))))

(add-hook 'minibuffer-exit-hook #'jc/evil-search--minibuffer-exit)

;; Track buffer when applying count
(advice-add 'jc/evil-search--apply-count :before
            (lambda (&rest _)
              (setq jc/evil-search--target-buffer (current-buffer)))
            '((name . track-buffer)))

(after! evil
  (setq evil-search-module 'evil-search)
  (advice-add 'evil-ex-search-update :after #'jc/evil-search--after-update)
  (advice-add 'evil-ex-search :after #'jc/evil-search--after-search))

;;; Misc UX tweaks

(use-package! dirvish
  :config
  (setopt dired-kill-when-opening-new-dired-buffer t))

(after! dired
  ;; Enabling `dired-omit-mode' during initial Dired setup can make an empty
  ;; directory look like a failed listing (after `.' and `..' are omitted),
  ;; which breaks nested directory entry from Dired/Dirvish. Defer it until the
  ;; buffer has opened successfully.
  (remove-hook 'dired-mode-hook #'dired-omit-mode)
  (add-hook 'dired-mode-hook #'jc/dired-enable-omit-delayed-h)
  (defun jc/dired-enable-omit-delayed-h ()
    (run-at-time
     0 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'dired-mode)
             (dired-omit-mode 1)))))
     (current-buffer))))

(after! dired-aux
  (defun jc/dired-vc-rename-tracked-file-p (file)
    "Use VC rename only for tracked, non-directory files."
    (and (not (file-directory-p file))
         (vc-backend file)))

  (define-advice dired-rename-file (:around (orig-fn file newname ok-if-already-exists) jc/tracked-files-only)
    (let ((dired-vc-rename-file (and dired-vc-rename-file
                                     (jc/dired-vc-rename-tracked-file-p file))))
      (funcall orig-fn file newname ok-if-already-exists))))

(setopt diff-refine 'navigation)

;; map j and k to use next line and previous line of native emacs
(after! evil            
  (define-key evil-normal-state-map (kbd "j") #'next-line)
  (define-key evil-normal-state-map (kbd "k") #'previous-line))

(use-package! mini-echo
  :defer t
  :init
  ;; Activate mini-echo and hide modeline together after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (mini-echo-mode 1)
              (global-hide-mode-line-mode 1))))

(defvar per-project-compile-history nil)

(define-advice project-compile (:around (&rest args) project-local-history)
  (let* ((root (project-root (project-current t)))
         (project-hist (alist-get root per-project-compile-history nil nil #'equal))
         (compile-history project-hist)
         (compile-command (and project-hist
                               (car project-hist))))
    (unwind-protect
        (apply args)
      (setf (alist-get root per-project-compile-test nil nil #'equal) compile-history))))

(defun jc/evil-ex-search-next ()
  "Like Vim C-g in /: jump to next match while staying in minibuffer."
  (interactive)
  (when (minibufferp)
    (let ((pattern-string (minibuffer-contents-no-properties)))
      (when (and pattern-string (> (length pattern-string) 0))
        (setq evil-ex-search-pattern
              (evil-ex-make-search-pattern pattern-string))
        (with-selected-window (minibuffer-selected-window)
          (evil-ex-search-next 1)
          ;; Update current-match highlight without moving
          (when (fboundp 'evil-ex-search-update)
            (evil-ex-search-update evil-ex-search-pattern
                                   evil-ex-search-offset
                                   evil-ex-search-match-beg
                                   evil-ex-search-match-end)))))))

(defun jc/evil-ex-search-previous ()
  "Like Vim C-t in /: jump to previous match while staying in minibuffer."
  (interactive)
  (when (minibufferp)
    (let ((pattern-string (minibuffer-contents-no-properties)))
      (when (and pattern-string (> (length pattern-string) 0))
        (setq evil-ex-search-pattern
              (evil-ex-make-search-pattern pattern-string))
        (with-selected-window (minibuffer-selected-window)
          (evil-ex-search-previous 1)
          ;; Update current-match highlight without moving
          (when (fboundp 'evil-ex-search-update)
            (evil-ex-search-update evil-ex-search-pattern
                                   evil-ex-search-offset
                                   evil-ex-search-match-beg
                                   evil-ex-search-match-end)))))))

(after! evil
  (define-key evil-ex-search-keymap (kbd "C-g") #'jc/evil-ex-search-next)
  (define-key evil-ex-search-keymap (kbd "C-t") #'jc/evil-ex-search-previous))



(setopt link-hint-avy-all-windows 't)


;;; Org-roam directory and dual DB sync

(setq org-roam-directory "~/Documents/roam/note/")

;; Doom sets find-file-visit-truename to t, which resolves symlinks in
;; buffer-file-name. This breaks org-roam for symlinked directories because
;; org-roam discovers files via symlink paths but stores them via truename
;; paths, causing the sync to immediately delete symlinked entries.
;;
;; Additionally, project .dir-locals.el files may set buffer-local
;; org-roam-directory/org-roam-db-location, which redirects DB operations
;; to a project-scoped database instead of the global one.
;;
;; These advices ensure:
;; 1. The GLOBAL org-roam DB is always updated/cleared (with symlink paths)
;; 2. If the buffer has project-scoped org-roam settings, the PROJECT DB
;;    is also updated/cleared (with the original truename paths)
(after! org-roam
  (defvar +org-roam--in-update-file nil
    "Non-nil when inside `org-roam-db-update-file' advice.
Prevents `org-roam-db-clear-file' advice from doing its own dual-DB
handling, since the update-file advice already manages both DBs.")

  (defun +org-roam--truename-to-symlink (file-path)
    "Convert truename FILE-PATH to its symlink equivalent under `org-roam-directory'.
If FILE-PATH is already under `org-roam-directory', return it unchanged."
    (let* ((roam-dir (expand-file-name
                      (file-name-as-directory (default-value 'org-roam-directory))))
           (expanded (expand-file-name file-path)))
      (if (string-prefix-p roam-dir expanded)
          file-path
        ;; Search top-level symlinks in org-roam-directory
        (catch 'found
          (dolist (entry (directory-files roam-dir t "^[^.]"))
            (when (file-symlink-p entry)
              (let ((target (file-name-as-directory (file-truename entry))))
                (when (string-prefix-p target expanded)
                  (throw 'found
                         (expand-file-name
                          (substring expanded (length target))
                          (file-name-as-directory entry)))))))
          file-path))))

  ;; --- Advice for org-roam-db-update-file (insert/update) ---
  (defadvice! +org-roam-update-file-dual-db-a (fn &optional file-path &rest args)
    :around #'org-roam-db-update-file
    (let* ((+org-roam--in-update-file t)
           (file-path (or file-path (buffer-file-name (buffer-base-buffer))))
           ;; Capture project-local values BEFORE overriding
           (project-roam-dir org-roam-directory)
           (project-db-loc org-roam-db-location)
           (global-roam-dir (default-value 'org-roam-directory))
           (global-db-loc (default-value 'org-roam-db-location))
           (has-project-p (not (equal (expand-file-name project-roam-dir)
                                      (expand-file-name global-roam-dir))))
           (symlink-path (+org-roam--truename-to-symlink file-path))
           (buf (or (get-file-buffer file-path)
                    (get-file-buffer symlink-path)))
           (orig-bfn (when buf (buffer-file-name buf))))

      ;; Step 1: Update PROJECT-LOCAL DB (if applicable)
      (when has-project-p
        (condition-case err
            (let ((org-roam-directory project-roam-dir)
                  (org-roam-db-location project-db-loc))
              (apply fn file-path args))
          (error
           (lwarn 'org-roam :warning
                  "Failed to update project DB for %s: %s"
                  file-path (error-message-string err)))))

      ;; Step 2: Update GLOBAL DB (with symlink path)
      (let ((find-file-visit-truename nil)
            (enable-dir-local-variables nil)
            (enable-local-variables :safe)
            (org-roam-directory global-roam-dir)
            (org-roam-db-location global-db-loc))
        (when (and buf (not (equal orig-bfn symlink-path)))
          (with-current-buffer buf (setq buffer-file-name symlink-path)))
        (unwind-protect
            (apply fn symlink-path args)
          (when (and buf orig-bfn (buffer-live-p buf)
                     (not (equal orig-bfn symlink-path)))
            (with-current-buffer buf (setq buffer-file-name orig-bfn)))))))

  ;; --- Advice for org-roam-db-clear-file (delete/rename) ---
  (defadvice! +org-roam-clear-file-dual-db-a (fn &optional file)
    :around #'org-roam-db-clear-file
    (if +org-roam--in-update-file
        ;; Inside update-file: the update advice already handles both DBs
        (funcall fn file)
      ;; Standalone clear (delete/rename): clear from both DBs
      (let* ((file (or file (buffer-file-name (buffer-base-buffer))))
             (project-roam-dir org-roam-directory)
             (project-db-loc org-roam-db-location)
             (global-roam-dir (default-value 'org-roam-directory))
             (global-db-loc (default-value 'org-roam-db-location))
             (has-project-p (not (equal (expand-file-name project-roam-dir)
                                        (expand-file-name global-roam-dir))))
             (symlink-path (+org-roam--truename-to-symlink file)))

        ;; Clear from project DB
        (when has-project-p
          (condition-case nil
              (let ((org-roam-directory project-roam-dir)
                    (org-roam-db-location project-db-loc))
                (funcall fn file))
            (error nil)))

        ;; Clear from global DB (with symlink path)
        (let ((org-roam-directory global-roam-dir)
              (org-roam-db-location global-db-loc))
          (funcall fn symlink-path))))))

(after! org-roam
  (defun jc/org-roam-link-project-org-dirs ()
    "Symlink all ~/Projects/*/org/ directories into org-roam under note/."
    (interactive)
    (let* ((projects-root (expand-file-name "~/Projects/"))
           (roam-note-dir org-roam-directory))

      ;; Ensure destination exists
      (unless (file-directory-p roam-note-dir)
        (make-directory roam-note-dir t))

      ;; Iterate over ~/Projects/*/
      (dolist (proj (directory-files projects-root t "^[^.]" t))
        (let ((org-dir (expand-file-name "org" proj)))
          (when (file-directory-p org-dir)
            (let* ((proj-name (file-name-nondirectory
                               (directory-file-name proj)))
                   (link-path (expand-file-name proj-name roam-note-dir)))
              (unless (file-exists-p link-path)
                (make-symbolic-link org-dir link-path t)
                (message "Linked: %s → %s" org-dir link-path)))))))

      (org-roam-db-sync)
      (message "Org-roam projects synced.")))

;; ;;; Org cache and persist performance

;; ;; org-element in-memory cache — keeps org-map-entries and friends fast.
;; ;; Disk persistence is off: the element cache (one entry per .org file) is
;; ;; the main contributor to org-persist bloat, and loading a large index
;; ;; synchronously on the first org-file open causes multi-second stalls.
;; ;; The in-memory cache rebuilds per-buffer and is nearly instant.
;; (setopt org-element-use-cache t
;;        org-element-cache-persistent nil)

;; ;; When element-cache persistence is off, org-element-cache-reset still
;; ;; calls org-persist-unregister on every org-mode buffer init.  That
;; ;; triggers org-persist--merge-index-with-disk → O(n²) cl-set-difference
;; ;; on the full persist index.  Skip the persistence codepath entirely
;; ;; when there's nothing to register or unregister.
;; (defadvice! +org-element-cache-reset-no-persist-a (fn &optional all no-persistence)
;;   :around #'org-element-cache-reset
;;   (funcall fn all (or no-persistence (not org-element-cache-persistent))))

;; ;; Shorten org-persist expiry so remaining data (LaTeX previews, export
;; ;; caches, etc.) doesn't accumulate indefinitely.  Default is 30 days.
;; (setopt org-persist-default-expiry 7)

;; ;; org-persist--merge-index-with-disk is called on EVERY persist read/write.
;; ;; It re-reads the index file from disk and runs cl-set-difference (O(n²))
;; ;; to merge with the in-memory index.  With thousands of LaTeX preview
;; ;; entries this takes seconds and causes visible freezes on buffer revert.
;; ;; The merge is only needed when multiple Emacs instances share the persist
;; ;; directory.  For single-Emacs use, skip it entirely after initial load.
;; (defvar +org-persist--index-loaded nil
;;   "Non-nil after the persist index has been loaded once.")
;; (defadvice! +org-persist-skip-merge-a (fn)
;;   :around #'org-persist--merge-index-with-disk
;;   (if +org-persist--index-loaded
;;       nil  ; already loaded, skip the expensive merge
;;     (setq +org-persist--index-loaded t)
;;     (funcall fn)))

(after! org-roam
  (cl-defmethod org-roam-node-jc-type ((node org-roam-node))
    "Return node type robustly across global and project Org-roam roots."
    (when-let* ((file (org-roam-node-file node))
                (file (expand-file-name file)))
      (let* ((roots (seq-uniq
                     (delq nil
                           (mapcar (lambda (path)
                                     (when path
                                       (file-name-as-directory
                                        (expand-file-name path))))
                                   (list (bound-and-true-p org-roam-directory)
                                         (default-value 'org-roam-directory)
                                         (and (boundp 'org-roam-db-location)
                                              (file-name-directory org-roam-db-location))
                                         (file-name-directory
                                          (default-value 'org-roam-db-location)))))
                     #'string=))
             (root (car (sort (cl-remove-if-not
                               (lambda (dir) (string-prefix-p dir file))
                               roots)
                              (lambda (a b) (> (length a) (length b))))))
             (relative-dir (when root
                             (file-name-directory (file-relative-name file root)))))
        (cond
         (relative-dir
          (directory-file-name relative-dir))
         ((string-match "/org/\\(.+\\)/[^/]+\\'" file)
          (match-string 1 file))
         (t nil)))))

  (add-to-list 'org-roam-node-template-prefixes '("jc-type" . "@"))

  (setq org-roam-node-display-template
        (concat
         "${doom-hierarchy:*} "
         (propertize "${jc-type:30}" 'face 'font-lock-keyword-face)
         " "
         (propertize "${doom-tags:42}" 'face '(:inherit org-tag :box nil))))

  (defadvice! +org-roam-node-completions-trim-a (fn &rest args)
    "Trim trailing whitespace from completion keys so Tab doesn't insert padding."
    :around #'org-roam-node-read--completions
    (mapcar (lambda (cell)
              (cons (string-trim-right (car cell)) (cdr cell)))
            (apply fn args)))
  )

(defun jc/quickrun-refresh-locals (&rest _)
  "Reload file-local variables before quickrun."
  (when buffer-file-name
    (hack-local-variables)))

(advice-add 'quickrun :before #'jc/quickrun-refresh-locals)

(setq inhibit-compacting-font-caches t)                                             

(setopt org-startup-with-link-previews 't)
(setopt org-startup-with-latex-preview 't)

;; this is to include extra files in the indexing using .projectile file
(setopt projectile-indexing-method 'hybrid)

(use-package! msgpack
  :defer t
  )

;; `tramp-rpc` currently requires `tramp-rpc-magit` while `tramp-rpc` itself is
;; still loading, which can recurse in this Doom/Emacs 31 setup.  Provide a
;; tiny local compatibility shim instead of loading the package's Magit layer.
;; This keeps core `tramp-rpc' working and avoids a fragile global `require'
;; advice. The tradeoff is that tramp-rpc-specific Magit/Projectile
;; optimizations are disabled.
;; Defer tramp-rpc-magit shim until tramp is loaded (avoids ~70 defuns at startup)
(after! tramp
  (unless (featurep 'tramp-rpc-magit)
  (defvar tramp-rpc--file-exists-cache (make-hash-table :test 'equal))
  (defvar tramp-rpc--file-truename-cache (make-hash-table :test 'equal))
  (defvar tramp-rpc--suppress-fs-notifications nil)
  (defvar tramp-rpc--watched-directories (make-hash-table :test 'equal))
  (defvar tramp-rpc-magit--debug nil)
  (defvar tramp-rpc-magit--process-caches nil)

  (defun tramp-rpc--cache-get (cache key)
    (gethash key cache))

  (defun tramp-rpc--cache-put (cache key value)
    (puthash key value cache))

  (defun tramp-rpc--invalidate-cache-for-path (filename)
    (let ((expanded (expand-file-name filename)))
      (remhash expanded tramp-rpc--file-exists-cache)
      (remhash expanded tramp-rpc--file-truename-cache)))

  (defun tramp-rpc--directory-watched-p (_localname _vec)
    nil)

  (defun tramp-rpc--handle-notification (_process _method _params)
    nil)

  (defun tramp-rpc-clear-file-exists-cache ()
    (clrhash tramp-rpc--file-exists-cache))

  (defun tramp-rpc-clear-file-truename-cache ()
    (clrhash tramp-rpc--file-truename-cache))

  (defun tramp-rpc--cleanup-watches-for-connection (_vec)
    nil)

  (defun tramp-rpc--clear-file-caches-for-connection (_vec)
    (tramp-rpc-clear-file-exists-cache)
    (tramp-rpc-clear-file-truename-cache))

  (defun tramp-rpc-magit--process-cache-lookup (_program _args)
    nil)

  (defun tramp-rpc-magit--file-exists-p (filename)
    (file-exists-p filename))

  (defun tramp-rpc-magit--clear-cache ()
    (setq tramp-rpc-magit--process-caches nil))

  (defun tramp-rpc-magit-enable ()
    (interactive))

  (defun tramp-rpc-magit-disable ()
    (interactive))

  (defun tramp-rpc-magit-enable-debug ()
    (interactive)
    (setq tramp-rpc-magit--debug t))

  (defun tramp-rpc-magit-disable-debug ()
    (interactive)
    (setq tramp-rpc-magit--debug nil))

  (defun tramp-rpc-projectile-enable ()
    (interactive))

  (defun tramp-rpc-projectile-disable ()
    (interactive))

  (provide 'tramp-rpc-magit)))

(use-package! tramp-rpc
  :defer t
  :after tramp
  :init
  )

(defun jc/dired-remote-lightweight-mode ()
  (when (file-remote-p default-directory)
    (when (bound-and-true-p diff-hl-dired-mode)
      (diff-hl-dired-mode -1))))

;; Append so this runs after Doom's `+vc-gutter-enable-maybe-h` hook and can
;; turn `diff-hl-dired-mode` back off on remote Dired buffers.
(add-hook 'dired-mode-hook #'jc/dired-remote-lightweight-mode t)

;; a temp fix for the warning tm status file killed
(after! diff-hl-dired
  (defun jc/diff-hl-dired-disable-process-query-a
      (orig-fn destination okstatus command file-or-list &rest flags)
    "Keep diff-hl's temp VC process from blocking Dired refreshes."
    (let ((result (apply orig-fn destination okstatus command file-or-list flags)))
      (when (and (eq okstatus 'async)
                 (processp result)
                 (buffer-live-p (process-buffer result))
                 (string-prefix-p " *diff-hl-dired* tmp status"
                                  (buffer-name (process-buffer result))))
        (set-process-query-on-exit-flag result nil))
      result))
  (advice-add #'vc-do-command :around
              #'jc/diff-hl-dired-disable-process-query-a))

(after! vterm
  (defun jc/vterm--shell-dir-from-default-directory (dir)
    "Convert DIR into a path suitable for sending to a shell in vterm.
If DIR is remote via TRAMP, strip the TRAMP prefix (including rpc hop)
and return only the localname on the remote host."
    (if (file-remote-p dir)
        (let ((vec (tramp-dissect-file-name dir)))
          (tramp-file-name-localname vec))
      dir))

  (defun jc/vterm-cd-to-buffer-above ()
    "In vterm, cd to the directory of the buffer in the window above."
    (interactive)
    (unless (derived-mode-p 'vterm-mode)
      (user-error "Run this inside vterm"))
    (let* ((above-win (windmove-find-other-window 'up))
           (raw-dir
            (when above-win
              (with-current-buffer (window-buffer above-win)
                default-directory)))
           (dir
            (when raw-dir
              (jc/vterm--shell-dir-from-default-directory raw-dir))))
      (unless dir
        (user-error "No window above with a usable default-directory"))
      (vterm-send-string
       (concat "cd " (shell-quote-argument (expand-file-name dir))))
      (vterm-send-return)))
  )

(use-package! arrow
  :defer t
  :load-path "/Users/jiawei/Projects/Playground/arrow.el"
  :config
  (require 'arrow-evil)
  (setq arrow-enable-evil-integration t
        arrow-evil-enable-extra-mappings t)
  (arrow-evil-mode 1))

(after! tramp
  (require 'jupyter-tramp nil t))

(use-package! anvil
  :defer 10
  :config
  (anvil-enable)
  (anvil-server-start))

(use-package! vertico
  :config
  (setopt vertico-resize 'grow-only))
