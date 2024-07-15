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

(after! cdlatex
  (add-to-list 'cdlatex-math-modify-alist '( ?s "\\boldsymbol"  nil  t t nil ))
  (add-to-list 'cdlatex-math-modify-alist '( ?n "\\mathbb"      nil  t t nil ))
  )

(setq org-latex-src-block-backend "listings")

(setq org-roam-directory "~/Documents/roam/")

(setq vterm-tramp-shells '(("ssh" "/usr/bin/bash")))

(setq org-agenda-files '("~/org/journal/"))
(setq org-journal-enable-agenda-integration t)
;; add all the todo.org file to the agenda

;; do not highlight the current line
(setq org-fontify-done-headline t)

;; do not highlight the current line
;; (setq global-hl-line-mode nil)
;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
;; (custom-set-faces!
;;   '(hl-line :background "#dad8d8"))

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

;; (use-package! websocket
;;   :after org-roam)

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


(after! org
  (setq org-startup-indented nil)
  ;; start hl-todo-mode
  ;; disable org indent mode
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir "images")
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (defadvice! recover-paragraph-seperate ()
    "Recover org paragraph mark position."
    :after 'org-setup-filling
    (setq-local paragraph-start "[\f\\|[ \t]*$]")
    (setq-local paragraph-separate "[ \t\f]*$"))
  (setq org-startup-folded 'content)
  (setq org-image-actual-width '(400))
  (org-link-set-parameters "zotero"
                           :follow (lambda (url arg) (browse-url (format "zotero:%s" url) arg)))
  (org-link-set-parameters "skim"
                           :follow (lambda (url arg) (browse-url (format "skim:%s" url) arg)))
  (map! :map org-mode-map
        "C-M-y" #'zz/org-download-paste-clipboard)
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
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("b" "booknotes" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/booknotes.org]" org-roam-directory)
           :target (file "booknotes/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "coding" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/coding.org]" org-roam-directory)
           :target (file "coding/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("l" "lectures" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/lectures.org]" org-roam-directory)
           :target (file "lectures/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("r" "research" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/research.org]" org-roam-directory)
           :target (file "research/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("a" "paper" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/paper.org]" org-roam-directory)
           :target (file "paper/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("m" "math" plain
           ,(format "#+title: ${title}\n\n* ${title}\n%%[%s/template/math.org]" org-roam-directory)
           :target (file "math/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("z" "literature note" plain
           "%?"
           :target (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
            "#+title: ${note-title}.\n#+created: %U\n \n* ${note-title}\n")
           :unnarrowed t)
          )
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))))

(setq! org-noter-notes-search-path '("/Users/jiawei/Documents/roam/booknotes"))
(setq citar-org-roam-capture-template-key "z")
;; citar configuration
(setq! org-cite-csl-styles-dir "~/Zotero/styles")
(setq! citar-bibliography '("~/Documents/roam/biblibrary/references.bib"))
(setq! citar-library-paths '("/Users/jiawei/Documents/roam/paper/"))
(setq! citar-notes-paths '("/Users/jiawei/Documents/roam/paper/"))
(setq citar-symbol-separator "  ")
(setq! citar-org-roam-subdir "paper/")
(setq citar-org-roam-note-title-template "${title}")

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

;; open warp terminal in current directory
(defvar last-warp-dir nil
  "Directory where the last Warp Terminal was opened.")

(defun send-scroll-up-to-other-frame ()
  (interactive)
  (let ((other-frame (next-frame)))  ; Get the next frame
    (with-selected-frame other-frame  ; Work within the context of the other frame
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the frame is in pdf-view-mode
             (pdf-view-next-line-or-next-page 5))  ; If true, go to the next page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the frame is in image-mode
             (image-next-file 1))  ; If true, go to the next image file
            (t
             (scroll-up-command))))))  ; For all other modes, perform a normal scroll up

(defun send-scroll-down-to-other-frame ()
  (interactive)
  (let ((other-frame (next-frame)))  ; Get the next frame
    (with-selected-frame other-frame  ; Work within the context of the other frame
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the frame is in pdf-view-mode
             (pdf-view-previous-line-or-previous-page 5))  ; If true, go to the next page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the frame is in image-mode
             (image-previous-file 1))  ; If true, go to the next image file
            (t
             (scroll-down-command))))))  ; For all other modes, perform a normal scroll up

(map! :n "C-;" #'send-scroll-up-to-other-frame)
(map! :n "C-'" #'send-scroll-down-to-other-frame)

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


(defun +macos/find-file-with-default-program ()
  "Select a file in Emacs and open it using the default program on your Mac."
  (interactive)
  (let ((file-name (read-file-name "Select file: ")))
    (call-process-shell-command (concat "open " file-name))))

;; map open file with mac default to space m m
(map! :leader :desc "find file with mac default" "m m" #'+macos/find-file-with-default-program)

(defun my/export-org-to-latex-body-only ()
  "Export current Org file to a LaTeX file with body only."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil))

(use-package copilot
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda ()
                    (setq-local copilot--indent-warning-printed-p t))))


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

(setq font-latex-fontify-script nil)

(defun my/org-tab-conditional ()
  (interactive)
  (if (yas-active-snippets)
      (yas-next-field-or-maybe-expand)
    (org-cycle)))

(map! :after evil-org
      :map evil-org-mode-map
      :i "<tab>" #'my/org-tab-conditional)

(defun my/citar-open-pdf ()
  "Open all PDF files associated with selected references on macOS using the default system application."
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

(after! corfu
  (setq corfu-max-width 60)
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

(setq! dired-kill-when-opening-new-dired-buffer t)

;; map space y to yank from kill ring
;; (map! :leader :desc "yank from kill ring" "y y" #'yank-from-kill-ring)

(defun my/copy-image-to-clipboard ()
  "Copy the image at point or current image buffer to the clipboard in macOS.
Handles Org mode, Dired mode, and image buffers."
  (interactive)
  (cond
   ;; In Org mode, try to get the path and copy file
   ((derived-mode-p 'org-mode)
    (if-let ((image-path (org-element-property :path (org-element-context))))
        (let ((full-path (expand-file-name image-path)))
          (if (file-exists-p full-path)
              (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                     full-path
                                     "\") as JPEG picture)'"))
            (message "File does not exist: %s" full-path)))
      (message "No image file at point!")))
   ;; In Dired mode, copy the file at point
   ((derived-mode-p 'dired-mode)
    (let ((file-path (dired-get-file-for-visit)))
      (if (file-exists-p file-path)
          (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                 file-path
                                 "\") as JPEG picture)'"))
        (message "Selected file does not exist!"))))
   ;; In image mode, copy the image data directly
   ((eq major-mode 'image-mode)
    (let ((image-file (buffer-file-name)))
      (if image-file
          (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                 image-file
                                 "\") as JPEG picture)'"))
        (message "No file associated with this buffer!"))))
   ;; Default message when not in applicable mode
   (t (message "Not in an Org, Dired, or Image buffer!"))))

;; map space y p
(map! :leader :desc "copy image file at point to clipboard" "y p" #'my/copy-image-to-clipboard)

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

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width))

;; open a file using zotero
(eval-after-load 'citar-file
  '(progn
     (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))))

(setq! dired-mouse-drag-files 'move)

(setq doom-modeline-modal nil)

(use-package! org
  :hook (org-mode . org-modern-mode)
  )

(add-hook 'doom-load-theme-hook
          (lambda ()
            (set-face-background 'fringe (face-attribute 'default :background))))

;; set treemacs position to right
(after! treemacs
  (setq treemacs-position 'right)
  )

;; load after org
(after! org
  (load (expand-file-name "skim.el" "~/.doom.d/lisp/")))

;; load mytex.el after org
(after! org
  (add-to-list 'org-file-apps '("\\.svg\\'" . default))
  (load (expand-file-name "mytex.el" "~/.doom.d/lisp/"))
  )

(map! :n "s-;" #'skim-next-page)
(map! :n "s-'" #'skim-prev-page)
(setq org-image-actual-width nil)

(setq imagemagick-types-inhibit (append imagemagick-types-inhibit '(SVG)))

(setq-hook! LaTeX-mode TeX-command-default "LaTeXMk")

(setq! org-highlight-latex-and-related '(native latex script entities))

(defun desktop-mode ()
  "Activate desktop mode with larger font size."
  (interactive)
  ;; Set the primary font with specified size and family
  (setq doom-font (font-spec :family "JetBrains Mono" :size 13))
  ;; Reload the font settings to apply changes
  (doom/reload-font)
  (message "Desktop mode activated: font size set to 13."))


(setq! citar-open-entry-function 'citar-open-entry-in-zotero)

(add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)

(setq tex-fontify-script 'nil)

;; (setq org-latex-caption-above '(table src-block special-block math))
