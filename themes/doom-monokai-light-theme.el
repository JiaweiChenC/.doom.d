;;; doom-monokai-light-theme.el --- A light port of Monokai Pro Light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: AI Assistant
;; Keywords: custom themes, faces
;; Homepage: https://github.com/monokai/monokai-pro
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; A faithful port of the Monokai Pro Light theme from VSCode to Emacs.
;; Uses the exact same color codes as the original VSCode theme.
;;
;;; Code:

(require 'doom-themes)

;;; Variables
(defgroup doom-monokai-light-theme nil
  "Options for the `doom-monokai-light' theme."
  :group 'doom-themes)

(defcustom doom-monokai-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-monokai-light-theme
  :type 'boolean)

(defcustom doom-monokai-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-monokai-light-theme
  :type 'boolean)

(defcustom doom-monokai-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-monokai-light-theme
  :type '(choice integer boolean))

;;; Theme definition
(def-doom-theme doom-monokai-light
  "A light port of Monokai Pro Light theme"

  ;; Main theme colors (exact VSCode color codes)
  (
    ;; Base colors from VSCode Monokai Pro Light
    ;; (bg-main        '("#faf4f2" "#faf4f2" "white"       ))  ; editor.background
    (bg-main        '("#FAF4F2" "#FAF4F2" "white"       ))  ; editor.background
    (bg-alt         '("#FAF4F2" "#FAF4F2" "brightwhite" ))  ; Various widget backgrounds
    (bg-sidebar     '("#ede7e5" "#ede7e5" "brightblack" ))  ; sideBar.background
    (bg-highlight   '("#EEE6DE" "#EEE6DE" "grey"        ))  ; editor.lineHighlightBackground
    (bg-region      '("#EEE6DE" "#EEE6DE" "grey"        ))  ; editor.selectionBackground
    (fg-main        '("#29242a" "#29242a" "black"       ))  ; editor.foreground
    (fg-comment     '("#a59fa0" "#a59fa0" "brightblack" ))  ; comments
    (fg-alt         '("#706b6e" "#706b6e" "brightblack" ))  ; activityBar.foreground
    (fg-dim         '("#918c8e" "#918c8e" "brightblack" ))  ; breadcrumb.foreground

    ;; Syntax highlighting colors (exact VSCode mappings)
    (red            '("#e14775" "#e14775" "red"         ))  ; Keywords, errors
    (orange         '("#e16032" "#e16032" "orange"      ))  ; Parameters, placeholders  
    (yellow         '("#cc7a0a" "#cc7a0a" "yellow"      ))  ; Strings
    (green          '("#269d69" "#269d69" "green"       ))  ; Functions, links
    (blue           '("#1c8ca8" "#1c8ca8" "blue"        ))  ; Types, classes
    (purple         '("#7058be" "#7058be" "magenta"     ))  ; Constants, numbers
    (magenta        purple)                                 ; Alias for purple
    (violet         purple)                                 ; Alias for purple
    (cyan           '("#1c8ca8" "#1c8ca8" "cyan"        ))  ; Same as blue for consistency
    (teal           cyan)                                   ; Alias for cyan
    (dark-blue      blue)                                   ; Alias for blue
    (dark-cyan      cyan)                                   ; Alias for cyan

    ;; UI element colors
    (highlight      '("#706b6e" "#706b6e" "brightblack" ))  ; cursor color
    (selection      '("#EEE6DE" "#EEE6DE" "grey"        ))  ; selection
    (region         '("#EEE6DE" "#EEE6DE" "grey"        ))  ; visual selection
    (vertical-bar   '("#d3cdcc" "#d3cdcc" "grey"        ))  ; window split

    ;; Variables required by doom theme
    (bg             bg-main)
    (fg             fg-main)
    (bg-alt         bg-alt)
    (fg-alt         fg-alt)
    
    ;; Base color spectrum
    (base0          bg-alt)
    (base1          bg-main)
    (base2          "#f0ece9")
    (base3          "#e0dad9")
    (base4          "#d3cdcc")
    (base5          "#bfb9ba")
    (base6          "#a59fa0")
    (base7          "#918c8e")
    (base8          fg-main)
    (grey           fg-comment)

    ;; Semantic color assignments
    (comments       (if doom-monokai-light-brighter-comments fg-alt fg-comment))
    (doc-comments   (if doom-monokai-light-brighter-comments fg-alt fg-comment))

    (builtin        blue)
    (constants      purple)
    (functions      green)
    (keywords       red)
    (methods        green)
    (numbers        purple)
    (operators      red)
    (strings        yellow)
    (type           blue)
    (variables      fg-main)

    (error          red)
    (success        green)
    (warning        orange)

    (vc-added       green)
    (vc-deleted     red)
    (vc-modified    orange)

    ;; Modeline colors
    (modeline-bg                (if doom-monokai-light-brighter-modeline base3 bg-sidebar))
    (modeline-fg                fg-main)
    (modeline-bg-alt            (if doom-monokai-light-brighter-modeline base4 base3))
    (modeline-fg-alt            fg-alt)
    (modeline-bg-inactive       bg-main)
    (modeline-fg-inactive       fg-alt)
    (modeline-bg-inactive-alt   bg-main)
    (modeline-fg-inactive-alt   fg-alt)
    (modeline-pad
      (when doom-monokai-light-padded-modeline
        (if (integerp doom-monokai-light-padded-modeline) 
            doom-monokai-light-padded-modeline 
          4))))

  ;; Base theme face overrides
  (

    (default :background bg-main :foreground fg-main)

    ;; Font styling
    ((font-lock-comment-face &override)
      :slant 'italic
      :background (if doom-monokai-light-brighter-comments (doom-blend teal bg-main 0.07)))
    ((font-lock-type-face &override) :slant 'italic)
    ((font-lock-builtin-face &override) :slant 'italic)
    ((font-lock-function-name-face &override) :foreground green)
    ((font-lock-keyword-face &override) :weight 'bold)
    ((font-lock-constant-face &override) :weight 'bold)
    ((font-lock-string-face &override) :foreground yellow)
    ((font-lock-variable-name-face &override) :foreground "#8448aa")

    ;; Highlight line
    (hl-line :background "#F1EAE9")

    ;; Line numbers
    ((line-number &override) :foreground "#bfb9ba")
    ((line-number-current-line &override) :foreground fg-main)

    ;; Mode line
    (mode-line
      :background modeline-bg
      :foreground modeline-fg
      :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
    (mode-line-inactive
      :background modeline-bg-inactive
      :foreground modeline-fg-inactive
      :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive)))
    (mode-line-emphasis
      :foreground (if doom-monokai-light-brighter-modeline fg-main fg-alt))

    ;; Company
    (company-tooltip-selection :background blue :foreground bg-main)

    ;; CSS mode
    (css-proprietary-property :foreground orange)
    (css-property             :foreground green)
    (css-selector             :foreground green)

    ;; Vertical border
    (vertical-border :background "#d3cdcc" :foreground "#d3cdcc")

    ;; Bookmark
    (bookmark-face :background base2 :distant-foreground orange)

    ;; Doom modeline
    (doom-modeline-bar :background green)
    (doom-modeline-evil-emacs-state  :foreground purple)
    (doom-modeline-evil-normal-state :foreground green)
    (doom-modeline-evil-visual-state :foreground purple)
    (doom-modeline-evil-insert-state :foreground orange)

    ;; Helm
    (helm-selection :foreground bg-main :weight 'bold :background blue)

    ;; Ivy
    (ivy-current-match :background base2 :distant-foreground fg-main)
    (ivy-minibuffer-match-face-1 :foreground green :background nil :weight 'bold)
    (ivy-minibuffer-match-face-2 :foreground purple :background nil :weight 'bold)
    (ivy-minibuffer-match-face-3 :foreground yellow :background nil :weight 'bold)
    (ivy-minibuffer-match-face-4 :foreground orange :background nil :weight 'bold)
    (ivy-minibuffer-match-highlight :foreground red :weight 'bold)
    (ivy-posframe :background modeline-bg-alt)

    ;; Tree-sitter
    (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
    (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face :underline nil)

    ;; Markdown mode
    (markdown-markup-face :foreground fg-main)
    (markdown-header-face :inherit 'bold :foreground red)
    ((markdown-code-face &override) :background bg-alt)

    ;; corfu mode
    (corfu-current :background "#F1EAE9")

    ;; Org mode
    (org-block :background (doom-blend yellow bg-main 0.04) :extend t)
    (org-block-background :background (doom-blend yellow bg-main 0.04))
    (org-block-begin-line :background (doom-blend yellow bg-main 0.08) :foreground comments :extend t)
    (org-block-end-line :background (doom-blend yellow bg-main 0.08) :foreground comments :extend t)
    (org-level-1 :foreground yellow :inherit 'bold)
    (org-level-2 :foreground orange :inherit 'bold)
    (org-level-3 :foreground green :inherit 'bold)
    (org-level-4 :foreground purple :inherit 'bold)
    (org-level-5 :foreground yellow :inherit 'bold)
    (org-level-6 :foreground orange :inherit 'bold)
    (org-level-7 :foreground green :inherit 'bold)
    (org-level-8 :foreground purple :inherit 'bold)

    ;; Solaire mode line
    (solaire-mode-line-face
      :inherit 'mode-line
      :background modeline-bg-alt
      :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-alt)))
    (solaire-mode-line-inactive-face
      :inherit 'mode-line-inactive
      :background modeline-bg-inactive-alt
      :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive-alt)))

    ;; Widget
    (widget-field :foreground fg-main :background base4)
    (widget-single-line-field :foreground fg-main :background base4)

    ;; Swiper
    (swiper-line-face :background base3)
    (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
    (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
    (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
    (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)))

;;; doom-monokai-light-theme.el ends here
