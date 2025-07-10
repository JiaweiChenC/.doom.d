;;; ef-kanso-theme.el --- Kanso-inspired light theme -*- lexical-binding:t -*-

;; Author: Kanso Theme Port
;; URL: https://github.com/yourname/ef-kanso-theme
;; Keywords: faces, theme, accessibility

(eval-and-compile
  (require 'ef-themes)

;;;###theme-autoload
  (deftheme ef-kanso
    "A light theme inspired by Kanso.nvim (pearl variant)."
    :background-mode 'light
    :kind 'color-scheme
    :family 'ef)

  (defconst ef-kanso-palette
    '(
      ;; Basic values
      (bg-main     "#f2f1ef") ; pearlWhite0
      (fg-main     "#24262D") ; pearlInk0
      (bg-dim      "#e2e1df") ; pearlWhite1
      (fg-dim      "#545464") ; pearlInk1
      (bg-alt      "#dddddb") ; pearlWhite2
      (fg-alt      "#6D6D69") ; pearlGray3

      (bg-active   "#b7d0ae") ; pearlGreen3 (for active elements)
      (bg-inactive "#e2e1df") ; pearlGray

      ;; Basic hues for foreground values
      (red             "#c84053") ; pearlRed
      (red-warmer      "#d7474b") ; pearlRed2
      (red-cooler      "#e82424") ; pearlRed3
      (red-faint       "#d9a594") ; pearlRed4
      (green           "#6f894e") ; pearlGreen
      (green-warmer    "#6e915f") ; pearlGreen2
      (green-cooler    "#b7d0ae") ; pearlGreen3
      (green-faint     "#e2e1df") ; pearlGray
      (yellow          "#77713f") ; pearlYellow
      (yellow-warmer   "#836f4a") ; pearlYellow2
      (yellow-cooler   "#de9800") ; pearlYellow3
      (yellow-faint    "#f9d791") ; pearlYellow4
      (blue            "#4d699b") ; pearlBlue4
      (blue-warmer     "#5d57a3") ; pearlBlue5
      (blue-cooler     "#b5cbd2") ; pearlBlue2
      (blue-faint      "#c7d7e0") ; pearlBlue1
      (magenta         "#b35b79") ; pearlPink
      (magenta-warmer  "#624c83") ; pearlViolet4
      (magenta-cooler  "#a09cac") ; pearlViolet1
      (magenta-faint   "#766b90") ; pearlViolet2
      (cyan            "#5e857a") ; pearlAqua2
      (cyan-warmer     "#597b75") ; pearlAqua
      (cyan-cooler     "#6693bf") ; pearlTeal2
      (cyan-faint      "#d7e3d8") ; pearlCyan

      ;; Diffs
      (bg-added        "#b7d0ae") ; pearlGreen3
      (bg-added-faint  "#e2e1df") ; pearlGray
      (bg-added-refine "#6e915f") ; pearlGreen2
      (fg-added        "#6f894e") ; pearlGreen

      (bg-changed        "#f9d791") ; pearlYellow4
      (bg-changed-faint  "#e2e1df") ; pearlGray
      (bg-changed-refine "#de9800") ; pearlYellow3
      (fg-changed        "#836f4a") ; pearlYellow2

      (bg-removed        "#d9a594") ; pearlRed4
      (bg-removed-faint  "#e2e1df") ; pearlGray
      (bg-removed-refine "#d7474b") ; pearlRed2
      (fg-removed        "#c84053") ; pearlRed

      ;; Special hues
      (bg-mode-line       "#b5cbd2") ; pearlBlue2
      (fg-mode-line       "#24262D") ; pearlInk0
      (bg-completion      "#c7d7e0") ; pearlBlue1
      (bg-hover           "#f9d791") ; pearlYellow4
      (bg-hl-line         "#e2e1df") ; pearlGray
      (bg-paren           "#b35b79") ; pearlPink
      (bg-err             "#d7474b") ; pearlRed2
      (bg-warning         "#de9800") ; pearlYellow3
      (bg-info            "#5e857a") ; pearlAqua2

      (border        "#9F9F99") ; pearlGray4
      (cursor        "#c84053") ; pearlRed
      (fg-intense    "#24262D") ; pearlInk0

      ;; Mappings
      (err red)
      (warning yellow)
      (info cyan)
      (link blue)
      (link-alt magenta)
      (name blue)
      (keybind cyan)
      (identifier magenta)
      (prompt magenta)

      (bg-region "#b5cbd2")
      (fg-region unspecified)

      ;; Code mappings
      (builtin magenta)
      (comment blue-faint)
      (constant orange)
      (fnname blue)
      (keyword magenta-warmer)
      (preprocessor cyan-cooler)
      (docstring yellow-faint)
      (string red-faint)
      (type green)
      (variable magenta)
      (rx-escape cyan-cooler)
      (rx-construct red-warmer)

      ;; Accent mappings
      (accent-0 yellow)
      (accent-1 red)
      (accent-2 magenta)
      (accent-3 blue)
      )
    "The `ef-kanso' palette.")

  (defcustom ef-kanso-palette-overrides nil
    "Overrides for `ef-kanso-palette'."
    :group 'ef-themes
    :type '(repeat (list symbol (choice symbol string))))

  (ef-themes-theme ef-kanso ef-kanso-palette ef-kanso-palette-overrides)

  (provide-theme 'ef-kanso))
;;; ef-kanso-theme.el ends here 