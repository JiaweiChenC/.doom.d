(TeX-add-style-hook
 "jrnl_setup"
 (lambda ()
   (TeX-run-style-hooks
    "threeparttable"
    "booktabs"
    "multirow")
   (TeX-add-symbols
    '("mymulticolumn" 2))
   (LaTeX-add-environments
    '("@IEEEbogusbiography" LaTeX-env-args ["argument"] 1)
    '("IEEEbiography" LaTeX-env-args ["argument"] 1)
    '("biography" LaTeX-env-args ["argument"] 1)))
 :latex)

