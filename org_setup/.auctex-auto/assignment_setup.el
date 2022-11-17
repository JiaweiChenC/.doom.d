(TeX-add-style-hook
 "assignment_setup"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("algorithm2e" "ruled") ("mdframed" "framemethod=tikz") ("inputenc" "utf8") ("fontenc" "T1")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "amsmath"
    "amsfonts"
    "stmaryrd"
    "amssymb"
    "enumerate"
    "algorithm2e"
    "mdframed"
    "listings"
    "geometry"
    "inputenc"
    "fontenc"
    "XCharter")
   (TeX-add-symbols
    "mdfilename"
    "questionTitle"
    "l")
   (LaTeX-add-environments
    '("info" LaTeX-env-args ["argument"] 0)
    '("warn" LaTeX-env-args ["argument"] 0)
    '("question" LaTeX-env-args ["argument"] 0)
    '("file" LaTeX-env-args ["argument"] 0)
    "commandline")
   (LaTeX-add-counters
    "Question")
   (LaTeX-add-mdframed-mdfdefinestyles
    "commandline"
    "file"
    "question"
    "warning"
    "info"))
 :latex)

