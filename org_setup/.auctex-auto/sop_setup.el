;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "sop_setup"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("setspace" "") ("babel" "english") ("fancyhdr" "") ("graphicx" "") ("adjustbox" "") ("subcaption" "") ("wrapfig" "") ("float" "") ("xcolor" "") ("hyperref" "") ("multirow" "") ("geometry" "margin=1in") ("titlesec" "")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "inputenc"
    "fontenc"
    "setspace"
    "babel"
    "fancyhdr"
    "graphicx"
    "adjustbox"
    "subcaption"
    "wrapfig"
    "float"
    "xcolor"
    "hyperref"
    "multirow"
    "geometry"
    "titlesec"))
 :latex)

