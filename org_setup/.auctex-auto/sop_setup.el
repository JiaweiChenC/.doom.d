(TeX-add-style-hook
 "sop_setup"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("babel" "english") ("geometry" "margin=1in")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
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
    "parskip"
    "multirow"
    "geometry"
    "titlesec"))
 :latex)

