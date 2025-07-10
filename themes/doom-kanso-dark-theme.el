;;; doom-kanso-dark-theme.el --- A dark port of Kanso theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Kanso theme port
;; Keywords: custom themes, faces
;; Homepage: https://github.com/rebelot/kanso.nvim
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; A port of the Kanso.nvim theme (ink variant) to Doom Emacs
;; Based on the original Kanso theme by rebelot
;;
;;; Code:

(require 'doom-themes)

;;; Variables
(defgroup doom-kanso-dark-theme nil
  "Options for the `doom-kanso-dark' theme."
  :group 'doom-themes)

(defcustom doom-kanso-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanso-dark-theme
  :type 'boolean)

(defcustom doom-kanso-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanso-dark-theme
  :type 'boolean)

(defcustom doom-kanso-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-kanso-dark-theme
  :type '(choice integer boolean))

;;; Theme definition
(def-doom-theme doom-kanso-dark
  "A dark port of Kanso theme (ink variant)"

  ;; Main theme colors (based on Kanso ink palette)
  (
    ;; name           default   256       16
    (bg             '("#14171d" "#14171d" "black"       )) ; inkBlack0
    (bg-alt         '("#1f1f26" "#1f1f26" "brightblack" )) ; inkBlack1
    (base0          '("#14171d" "#14171d" "black"       )) ; inkBlack0
    (base1          '("#1f1f26" "#1f1f26" "brightblack" )) ; inkBlack1
    (base2          '("#24262D" "#24262D" "brightblack" )) ; inkBlack2
    (base3          '("#393B42" "#393B42" "brightblack" )) ; inkBlack3
    (base4          '("#5C6066" "#5C6066" "brightblack" )) ; inkAsh/inkGray3
    (base5          '("#75797f" "#75797f" "brightblack" )) ; inkGray2
    (base6          '("#909398" "#909398" "brightblack" )) ; inkGray1
    (base7          '("#A4A7A4" "#A4A7A4" "white"       )) ; inkGray
    (base8          '("#C5C9C7" "#C5C9C7" "brightwhite" )) ; inkWhite
    (fg             '("#C5C9C7" "#C5C9C7" "brightwhite" )) ; inkWhite
    (fg-alt         '("#A4A7A4" "#A4A7A4" "white"       )) ; inkGray

    (grey           '("#5C6066" "#5C6066" "brightblack" )) ; inkAsh
    (red            '("#c4746e" "#c4746e" "red"         )) ; inkRed
    (orange         '("#b6927b" "#b6927b" "brightred"   )) ; inkOrange
    (green          '("#87a987" "#87a987" "green"       )) ; inkGreen
    (teal           '("#8ea4a2" "#8ea4a2" "brightgreen" )) ; inkAqua
    (yellow         '("#c4b28a" "#c4b28a" "yellow"      )) ; inkYellow
    (blue           '("#8ba4b0" "#8ba4b0" "brightblue"  )) ; inkBlue2
    (dark-blue      '("#658594" "#658594" "blue"        )) ; inkBlue
    (magenta        '("#a292a3" "#a292a3" "brightmagenta")) ; inkPink
    (violet         '("#8992a7" "#8992a7" "magenta"     )) ; inkViolet
    (cyan           '("#8ea4a2" "#8ea4a2" "brightcyan"  )) ; inkAqua
    (dark-cyan      '("#949fb5" "#949fb5" "cyan"        )) ; inkTeal

    ;; Face categories
    (highlight      base3)
    (vertical-bar   base2)
    (selection      base3)
    (builtin        blue)
    (comments       (if doom-kanso-dark-brighter-comments base5 base4))
    (doc-comments   (if doom-kanso-dark-brighter-comments base5 base4))
    (constants      orange)
    (functions      blue)
    (keywords       violet)
    (methods        cyan)
    (operators      base6)
    (type           teal)
    (strings        green)
    (variables      violet)
    (numbers        magenta)
    (region         base3)
    (error          red)
    (warning        yellow)
    (success        green)

    ;; Custom kanso colors
    (kanso-bg       bg)
    (kanso-bg-alt   bg-alt)
    (kanso-fg       fg)
    (kanso-comment  base5)
    (kanso-keyword  violet)
    (kanso-string   green)
    (kanso-func     blue)
    (kanso-var      violet)
    (kanso-const    orange)
    (kanso-type     teal)
    (kanso-number   magenta)

    ;; Git/VCS colors (using autumn colors from kanso)
    (vc-added       '("#76946A" "#76946A" "green"))     ; autumnGreen
    (vc-deleted     '("#C34043" "#C34043" "red"))       ; autumnRed
    (vc-modified    '("#DCA561" "#DCA561" "yellow"))    ; autumnYellow

    ;; Modeline
    (modeline-fg              fg)
    (modeline-fg-alt          base6)
    (modeline-bg              (if doom-kanso-dark-brighter-modeline base3 base1))
    (modeline-bg-alt          (if doom-kanso-dark-brighter-modeline base4 base2))
    (modeline-bg-inactive     base0)
    (modeline-fg-inactive     base4)
    (modeline-bg-inactive-alt base1)
    (modeline-fg-inactive-alt base4))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :foreground comments
    :slant 'italic)
   ((font-lock-doc-face &override) :slant 'italic)
   ((font-lock-keyword-face &override) :weight 'bold :foreground kanso-keyword)
   ((font-lock-constant-face &override) :weight 'bold :foreground kanso-const)
   ((font-lock-function-name-face &override) :foreground kanso-func)
   ((font-lock-variable-name-face &override) :foreground kanso-var)
   ((font-lock-string-face &override) :foreground kanso-string)
   ((font-lock-type-face &override) :slant 'italic :foreground kanso-type)
   ((font-lock-builtin-face &override) :slant 'italic :foreground blue)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if doom-kanso-dark-padded-modeline `(:line-width ,(if (integerp doom-kanso-dark-padded-modeline) doom-kanso-dark-padded-modeline 4) :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-inactive
    :box (if doom-kanso-dark-padded-modeline `(:line-width ,(if (integerp doom-kanso-dark-padded-modeline) doom-kanso-dark-padded-modeline 4) :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanso-dark-brighter-modeline base8 base7))

   ;;;; CSS mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; Doom modeline
   (doom-modeline-bar :background blue)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;;;; Ivy
   (ivy-current-match :background base3 :distant-foreground base0 :weight 'normal)
   (ivy-minibuffer-match-face-1 :foreground blue :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground violet :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground orange :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground magenta :weight 'bold)

   ;;;; LaTeX mode
   (font-latex-math-face :foreground green)
   (font-latex-sectioning-5-face :foreground blue :weight 'bold)

   ;;;; Markdown mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background base1)

   ;;;; Org mode
   (org-level-1 :foreground orange :weight 'bold)
   (org-level-2 :foreground magenta :weight 'bold)
   (org-level-3 :foreground blue :weight 'bold)
   (org-level-4 :foreground violet :weight 'bold)
   (org-level-5 :foreground green :weight 'bold)
   (org-level-6 :foreground teal :weight 'bold)
   (org-level-7 :foreground dark-blue :weight 'bold)
   (org-level-8 :foreground dark-cyan :weight 'bold)
   (org-block :background base1 :extend t)
   (org-block-begin-line :background base2 :foreground comments :extend t)
   (org-block-end-line :background base2 :foreground comments :extend t)

   ;;;; Solaire mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if doom-kanso-dark-padded-modeline `(:line-width ,(if (integerp doom-kanso-dark-padded-modeline) doom-kanso-dark-padded-modeline 4) :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if doom-kanso-dark-padded-modeline `(:line-width ,(if (integerp doom-kanso-dark-padded-modeline) doom-kanso-dark-padded-modeline 4) :color ,modeline-bg-inactive-alt)))

   ;;;; Helm
   (helm-selection :inherit 'bold :background base3 :distant-foreground base0)

   ;;;; Company
   (company-tooltip-selection :background blue :foreground base0)

   ;;;; Treemacs
   (treemacs-root-face :foreground magenta :weight 'bold :height 1.2)
   (treemacs-directory-face :foreground blue)
   (treemacs-file-face :foreground fg)
   (treemacs-git-modified-face :foreground orange)
   (treemacs-git-added-face :foreground green)
   (treemacs-git-renamed-face :foreground magenta)
   (treemacs-git-ignored-face :foreground base5)
   (treemacs-git-untracked-face :foreground base5)
   (treemacs-git-renamed-face :foreground magenta)
   (treemacs-git-conflict-face :foreground red)

   ;;;; Outline
   (outline-1 :foreground orange)
   (outline-2 :foreground magenta)
   (outline-3 :foreground blue)
   (outline-4 :foreground violet)
   (outline-5 :foreground green)
   (outline-6 :foreground teal)
   (outline-7 :foreground dark-blue)
   (outline-8 :foreground dark-cyan)

   ;;;; Misc
   (hl-line :background base1)
   (region :background base3 :distant-foreground (doom-lighten fg 0.2) :extend t)
   (cursor :background fg)
   (vertical-border :foreground base2)
   (window-divider :foreground base2)
   (window-divider-first-pixel :foreground base2)
   (window-divider-last-pixel :foreground base2)))

;;; doom-kanso-dark-theme.el ends here 