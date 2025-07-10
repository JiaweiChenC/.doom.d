;;; doom-kanso-theme.el --- A light port of Kanso theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Kanso theme port
;; Keywords: custom themes, faces
;; Homepage: https://github.com/rebelot/kanso.nvim
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; A port of the Kanso.nvim theme (pearl variant) to Doom Emacs
;; Based on the original Kanso theme by rebelot
;;
;;; Code:

(require 'doom-themes)

;;; Variables
(defgroup doom-kanso-theme nil
  "Options for the `doom-kanso' theme."
  :group 'doom-themes)

(defcustom doom-kanso-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanso-theme
  :type 'boolean)

(defcustom doom-kanso-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanso-theme
  :type 'boolean)

(defcustom doom-kanso-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-kanso-theme
  :type '(choice integer boolean))

;;; Theme definition
(def-doom-theme doom-kanso
  "A light port of Kanso theme (pearl variant)"

  ;; Main theme colors (based on Kanso pearl palette)
  (
    ;; name           default   256       16
    (bg             '("#f2f1ef" "#f2f1ef" "white"       )) ; pearlWhite0
    (bg-alt         '("#e2e1df" "#e2e1df" "brightwhite" )) ; pearlWhite1
    (base0          '("#f2f1ef" "#f2f1ef" "white"       )) ; pearlWhite0
    (base1          '("#e2e1df" "#e2e1df" "brightwhite" )) ; pearlWhite1
    (base2          '("#dddddb" "#dddddb" "brightwhite" )) ; pearlWhite2
    (base3          '("#c9cbd1" "#c9cbd1" "brightblack" )) ; pearlViolet3
    (base4          '("#9F9F99" "#9F9F99" "brightblack" )) ; pearlGray4
    (base5          '("#6D6D69" "#6D6D69" "brightblack" )) ; pearlGray3
    (base6          '("#5C6068" "#5C6068" "brightblack" )) ; pearlGray2
    (base7          '("#545464" "#545464" "brightblack" )) ; pearlInk1
    (base8          '("#24262D" "#24262D" "black"       )) ; pearlInk0
    (fg             '("#24262D" "#24262D" "black"       )) ; pearlInk0
    (fg-alt         '("#545464" "#545464" "brightblack" )) ; pearlInk1

    (grey           '("#9F9F99" "#9F9F99" "brightblack" )) ; pearlGray4
    (red            '("#c84053" "#c84053" "red"         )) ; pearlRed
    (orange         '("#cc6d00" "#cc6d00" "brightred"   )) ; pearlOrange
    (green          '("#6f894e" "#6f894e" "green"       )) ; pearlGreen
    (teal           '("#597b75" "#597b75" "brightgreen" )) ; pearlAqua
    (yellow         '("#77713f" "#77713f" "yellow"      )) ; pearlYellow
    (blue           '("#4d699b" "#4d699b" "brightblue"  )) ; pearlBlue4
    (dark-blue      '("#4e8ca2" "#4e8ca2" "blue"        )) ; pearlTeal1
    (magenta        '("#b35b79" "#b35b79" "brightmagenta")) ; pearlPink
    (violet         '("#624c83" "#624c83" "magenta"     )) ; pearlViolet4
    (cyan           '("#5e857a" "#5e857a" "brightcyan"  )) ; pearlAqua2
    (dark-cyan      '("#4e8ca2" "#4e8ca2" "cyan"        )) ; pearlTeal1

    ;; Face categories
    (highlight      base3)
    (vertical-bar   base2)
    (selection      base2)
    (builtin        blue)
    (comments       (if doom-kanso-brighter-comments base5 base4))
    (doc-comments   (if doom-kanso-brighter-comments base5 base4))
    (constants      orange)
    (functions      blue)
    (keywords       violet)
    (methods        cyan)
    (operators      base5)
    (type           teal)
    (strings        green)
    (variables      violet)
    (numbers        magenta)
    (region         base2)
    (error          red)
    (warning        yellow)
    (success        green)

    ;; Custom kanso colors
    (kanso-bg       bg)
    (kanso-bg-alt   bg-alt)
    (kanso-fg       fg)
    (kanso-comment  base4)
    (kanso-keyword  violet)
    (kanso-string   green)
    (kanso-func     blue)
    (kanso-var      violet)
    (kanso-const    orange)
    (kanso-type     teal)
    (kanso-number   magenta)

    ;; Git/VCS colors
    (vc-added       green)
    (vc-deleted     red)
    (vc-modified    yellow)

    ;; Modeline
    (modeline-fg              fg)
    (modeline-fg-alt          base6)
    (modeline-bg              (if doom-kanso-brighter-modeline base3 base1))
    (modeline-bg-alt          (if doom-kanso-brighter-modeline base4 base2))
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
    :box (if doom-kanso-padded-modeline `(:line-width ,(if (integerp doom-kanso-padded-modeline) doom-kanso-padded-modeline 4) :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-inactive
    :box (if doom-kanso-padded-modeline `(:line-width ,(if (integerp doom-kanso-padded-modeline) doom-kanso-padded-modeline 4) :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanso-brighter-modeline base8 base7))

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
   (ivy-current-match :background base2 :distant-foreground base0 :weight 'normal)
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
    :box (if doom-kanso-padded-modeline `(:line-width ,(if (integerp doom-kanso-padded-modeline) doom-kanso-padded-modeline 4) :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if doom-kanso-padded-modeline `(:line-width ,(if (integerp doom-kanso-padded-modeline) doom-kanso-padded-modeline 4) :color ,modeline-bg-inactive-alt)))

   ;;;; Helm
   (helm-selection :inherit 'bold :background base2 :distant-foreground base0)

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
   (region :background base2 :distant-foreground (doom-darken fg 0.2) :extend t)
   (cursor :background fg)
   (vertical-border :foreground base3)
   (window-divider :foreground base3)
   (window-divider-first-pixel :foreground base3)
   (window-divider-last-pixel :foreground base3)))

;;; doom-kanso-theme.el ends here 