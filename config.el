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
(setq doom-theme 'my-default
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

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq org-latex-src-block-backend "listings")

(setq org-roam-directory "~/Documents/roam/")


(setq vterm-tramp-shells '(("ssh" "/usr/bin/bash")))

(setq org-agenda-files '("~/org/journal/"))
(setq org-journal-enable-agenda-integration t)
;; add all the todo.org file to the agenda

;; (global-hide-mode-line-mode)

;; do not highlight the current line
(setq org-fontify-done-headline t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq org-log-done t)

(setq org-preview-latex-default-process 'dvisvgm)

(setq fancy-splash-image (concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(global-hide-mode-line-mode)

(setq doom-modeline-major-mode-icon t)

(map! :n "j" 'evil-next-visual-line)
(map! :n "k" 'evil-previous-visual-line)
(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmds! (org-on-heading-p)
                      #'org-backward-element
                      #'evil-previous-line)
       :n "gj" (cmds! (org-on-heading-p)
                      #'org-forward-element
                      #'evil-next-line))
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
  ;; (setq org-indent-mode nil)
  (setq! org-startup-indented nil)
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
;; (add-hook 'doom-big-font-mode-hook
;;           (lambda ()
;;             (if doom-big-font-mode
;;                 (setq org-image-actual-width 700)
;;               (setq org-image-actual-width 400))))

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
 :bind (("M-TAB" . 'copilot-accept-completion-by-word)
        ("M-<tab>" . 'copilot-accept-completion-by-word)
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

;; set space o m as mac-os-open-with
(map! :leader :desc "macos open with default programe" "o m" #'+macos/open-in-default-program)

;; open warp terminal in current directory
(defvar last-warp-dir nil
  "Directory where the last Warp Terminal was opened.")


;; set the default frame
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; bind quickrun to space r r
(map! :leader :desc "quickrun" "r r" #'quickrun)
;; bind quickrun kill process to space r k
(map! :leader :desc "quickrun kill process" "r k" #'quickrun--kill-running-process)

;; (use-package! dired
;;   :config
;;   (set-popup-rule! "^\\*image-dired" :ignore t))

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

(setq! quickrun-timeout-seconds 10000)

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

;; (setq doom-modeline-modal nil)

;; (use-package! org
;;   :hook (org-mode . org-modern-mode)
;;   )

(add-hook 'doom-load-theme-hook
          (lambda ()
            (set-face-background 'fringe (face-attribute 'default :background))))


;; (load! (expand-file-name "my-quickrun.el" "~/.doom.d/lisp/"))
;; load mytex.el after org
(after! org
  (add-to-list 'org-file-apps '("\\.svg\\'" . default))
  (load! (expand-file-name "mytex.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "skim.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "babel.el" "~/.doom.d/lisp/"))
  (load! (expand-file-name "citar_function.el" "~/.doom.d/lisp/"))
  ;; (load! (expand-file-name "lib-gptel" "~/.doom.d/lisp/"))
  (load! (expand-file-name "custom-functions" "~/.doom.d/lisp/"))
  )

(map! :n "s-;" #'skim-next-page)
(map! :n "s-'" #'skim-prev-page)
(setq org-image-actual-width nil)

(setq! imagemagick-types-inhibit (append imagemagick-types-inhibit '(SVG)))

(setq-hook! LaTeX-mode TeX-command-default "LaTeXMk")

(setq! org-highlight-latex-and-related '(native latex script entities))

;; (setq! citar-open-entry-function 'citar-open-entry-in-zotero)
;; (setq! citar-at-point-function 'embark-act)
;; (map! :map citar-embark-citation-map
;;       :n
;;       "<return>" nil
;;       "<return>" #'zot-open-pdf)

;; (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)

(setq tex-fontify-script 'nil)

(setq org-latex-caption-above '(table src-block special-block math))

;; bind space b return to embard open bookmark external
(map! :leader :desc "open bookmark external" "e m" #'embark-bookmark-open-externally)

(use-package! embark)

(setq! org-modern-table nil)
;; (setq! org-modern-block-fringe nil)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq! org-appear-autolinks t)
  )

(setq eglot-send-changes-idle-time 0.1)

(use-package! citar)

(setq! org-latex-src-block-backend 'listings)

(setq! org-image-actual-width nil)

(setq! TeX-command-extra-options "-shell-escape")

(setq! org-export-expand-links 'nil)

(use-package! bookmark-in-project
  :commands (bookmark-in-project-jump
             bookmark-in-project-jump-next
             bookmark-in-project-jump-previous
             bookmark-in-project-delete-all)

  ;; Example key bindings.
  ;; map bookmark in project to space p m
        :config
        (map! :leader :desc "bookmark in project" "p m" #'bookmark-in-project-toggle)
        (map! :leader :desc "bookmark in project jump" "p j" #'bookmark-in-project-jump))


;; ;; map space e g to gptel
;; (map! :leader :desc "gptel" "e g" #'gptel)

;; (setq! corfu-popupinfo-max-height 1)

(setq! vterm-timer-delay 0.01)

(use-package! writeroom-mode
  :config
  (setq! writeroom-fullscreen-effect 'nil)
  (setq! writeroom-width 120)
  (setq! writeroom-extra-line-spacing 0.5))

;; global writeroom mode to space t W
(map! :leader :desc "writeroom mode" "t W" #'writeroom-mode)

;; disable org block highlight
(after! org
;; ;;   ;; Function to apply custom face attributes for org blocks
;; ;;   (defun my-org-mode-frame-customizations ()
;; (set-face-attribute 'org-block nil :foreground 'unspecified :background 'unspecified)
(set-face-attribute 'org-block-begin-line nil :foreground 'unspecified :background 'unspecified)
(set-face-attribute 'org-block-end-line nil :foreground 'unspecified :background 'unspecified)
)

;;   ;; Add this function to the hook that runs after making a new frame
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (with-selected-frame frame
;;                 (my-org-mode-frame-customizations))))

;;   ;; Apply the customizations to the initial frame
;;   (my-org-mode-frame-customizations)
;; )

;; space t e to mini-echo-mode
(map! :leader :desc "toggle mini echo mode" "t e" #'mini-echo-mode)

;;(after! popup
;;  (set-popup-rule! "^\\*doom:scratch\\*" :size 0.3 :quit t :select t :ttl 5))

;; (setq! hide-mode-line-excluded-modes nil)

(add-hook 'emacs-startup-hook #'global-hide-mode-line-mode)

(use-package! mini-echo
  :config
  (mini-echo-mode)
  )

(after! flycheck
  (setq flycheck-indication-mode 'right-fringe)
  ;; Apply the same bitmap to all levels using a loop
  (dolist (level '(error warning info))
    (flycheck-define-error-level level
      :overlay-category (intern (format "flycheck-%s-overlay" level))
      :fringe-bitmap 'flycheck-fringe-bitmap-double-left-arrow
      :fringe-face (intern (format "flycheck-fringe-%s" level)))))

(setq-default bidi-display-reordering nil)

(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(use-package! sideline
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t
        ;; sideline-order-right 'up                     ; or 'down
        ;; sideline-format-left "%s   "                 ; format for left aligment
        ;; sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t
        ;; sideline-backends-left '(sideline-eglot)
        sideline-backends-right '(sideline-flycheck)
        )
  :hook ((flycheck-mode . sideline-mode)   ; for `sideline-flycheck`
         (flymake-mode  . sideline-mode)
         (eglot-mode . sideline-mode)       ; for `sideline-eglot`))
         ))            ; display the backend name

(use-package! sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(with-eval-after-load 'ob-jupyter
 (org-babel-jupyter-aliases-from-kernelspecs)
 )

(setq! org-preview-html-viewer 'xwidget)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(after! org
  (define-key org-mode-map (kbd "S-s-<mouse-1>") 'hermanhelf-org-jump-to-pdf))

;; (set-frame-parameter nil 'alpha-background 0.7)

;; map space l n to ni-narrow-to-region-floating
(map! :leader :desc "narrow to region floating" "l n" #'ni-narrow-to-region-floating)

;; map space l m  to ni-clear-aggregation
(map! :leader :desc "clear aggregation" "l m" #'ni-clear-aggregation)

(map! :leader :desc "float narrow t" "l t" #'ni-clear-aggregation)

;; (use-package! float-narrow-indirect)
(setq ni-floating-frame-border-color nil)

;; (setq ni-floating-frame-transparency '(95 . 50))
;; (after! float-narrow-indirect-mode
;;   (setq fni-floating-frame-border-color nil))

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(after! corfu
  (setq corfu-auto nil))

(add-hook 'prog-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-comment-face :slant 'italic)))
;; alpaa backround
;; (set-frame-parameter nil 'alpha 90)

(use-package! rainbow-csv
  :hook (csv-mode . rainbow-csv-mode))

;; map space t b to breadcrumb-mode
(map! :leader :desc "breadcrumb mode" "t h" #'breadcrumb-mode)

;; (setq! csv-align-max-width 10000)

;; (use-package! cell-mode
  ;; :config
  ;; (setq! cell-cursor-blink-p 'nil)
  ;; )
;;
;; (remove-hook 'find-file-hook #'diff-hl-mode)
;; (remove-hook 'vc-dir-mode-hook #'diff-hl-mode)
;; (remove-hook 'doom-first-file #'diff-hl-mode)
;; (setq! catppuccin-flavor 'latte)

(defun wrap-text-with-color ()
  "Wrap the selected text with [[color:red][text]]."
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[[color:red][%s]]" selection)))
    (message "No text selected!")))

;; To bind this function to a key in Doom Emacs:
(map! :leader
      :desc "Wrap Text with Color"
      "l r" #'wrap-text-with-color)

;; (use-package! org-cite-overlay)


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

;; map C-j to electric-newline-and-maybe-indent
;; (after! org
;;   ;; Unbind C-j from its current function in org-mode
;;   (map! :map org-mode-map
;;         "C-j" nil)

;;   ;; Bind C-j to your new custom function in org-mode
;;   (map! :map org-mode-map
;;         "C-j" #'electric-newline-and-maybe-indent))

;; (setq citar-file-open-functions '(("pdf" . zot-open-pdf)
;;                                   ("html" . citar-file-open-external)
;;                                   (t . find-file)))
;; (setq citar-open-entry-function 'citar-open-entry-in-zotero)
;; (setq citar-at-point-function 'embark-act)
(map! :map citar-embark-citation-map
      :n
      "<return>" nil
      "<return>" #'zot-open-pdf)

(setq! evil-kill-on-visual-paste nil)

(use-package! zoom)

(custom-set-variables
 '(zoom-size '(0.8 . 0.8)))

;; (use-package pet
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode))

;; (use-package! window-stool
  ;; :hook (prog-mode . window-stool-mode))

(use-package! envrc
  :hook (after-init . envrc-global-mode))

(use-package! eglot
  ;; :ensure t
  :config
  (setq eglot-events-buffer-config 0
        eglot-report-progress nil)
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))
  (setq-default
   eglot-workspace-configuration
   '(:basedpyright (:typeCheckingMode "basic")
     :basedpyright.analysis
     (:diagnosticSeverityOverrides
      (:reportUnusedCallResult "none"
       :reportArgumentType "none")
      :inlayHints (:callArgumentNames :json-false)))))
  ;; (setq-default
  ;;  eglot-workspace-configuration
  ;;  '(:basedpyright (:typeCheckingMode "recommended")
  ;;    :basedpyright.analysis
  ;;    (:diagnosticSeverityOverrides
  ;;     (:reportUnusedCallResult "none")
  ;;     :inlayHints (:callArgumentNames :json-false))))
  ;; )

;; This is because org-src-mode-hook runs before the temporary buffer created by org-edit-special
;;is fully initialized, which can lead to issues when starting Eglot.

;; (after! org
;;   (org-src-context-mode))

;; (defun my/org-src-activate-eglot-if-needed ()
;;   "Activate Eglot in org-src-edit buffer if it's not already active."
;;   (when (and (derived-mode-p 'org-src-mode)
;;              (not (eglot-current-server)))
;;     (run-at-time 0 nil #'eglot-ensure)))
;; (add-hook 'org-src-mode-hook #'my/org-src-activate-eglot-if-needed)

;; (defun my/org-disable-visual-line-in-tables ()
;;   "Disable `visual-line-mode` in Org tables when point is in a table."
;;   (when (and (derived-mode-p 'org-mode)
;;              visual-fill-column-mode)
;;     (if (org-at-table-p)
;;         (visual-line-mode -1)
;;       (visual-line-mode 1))))

;; (add-hook 'post-command-hook #'my/org-disable-visual-line-in-tables)
;;
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

