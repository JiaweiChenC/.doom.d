;;; lisp/mytex.el -*- lexical-binding: t; -*-
(defun org-export-latex-body-only ()
  "Export current Org file to a LaTeX file with body only, open and compile it."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil))

(map! :leader :desc "export latex body only" "l b" #'org-export-latex-body-only)

;;;;;;;;;;;;;;;;;;;;;;; hack to make org table work perfectly ;;;;;;;;;;;;;;;;;;;;;;
(defun org-export-cmidrule-filter-latex (row backend info)
  "Replace <startcidend> with \\cmidrule{start-end} in LaTeX export."
  (while (string-match "\\(<\\([0-9]+\\)cid\\([0-9]+\\)?>[[:blank:]]*\\)" row)
    (let ((start (string-to-number (match-string 2 row)))
          (end (string-to-number (match-string 3 row))))
      (setq row (replace-match (format "\\\\\\cmidrule(lr){%d-%d}" start end) t t row)))
    ;; Clean up unnecessary spaces and & that might be left over around the cmidrule
    (setq row (replace-regexp-in-string "\\(&\\s-*\\|\\s-*\\\\\\\\\\)" "" row))
    (setq row (replace-regexp-in-string "\\[0pt\\]" "" row)))
  row)

(defun org-export-multicolumnv-filter-latex (row backend info)
  (while (string-match
          "\\(<\\([0-9]+\\)colv\\([lrc]\\)?>[[:blank:]]*\\([^&]+\\)\\)" row)
    (let ((columns (string-to-number (match-string 2 row)))
          (start (match-end 0))
          (contents (replace-regexp-in-string
                     "\\\\" "\\\\\\\\"
                     (replace-regexp-in-string "[[:blank:]]*$" ""
                                               (match-string 4 row))))
          (algn (or (match-string 3 row) "l")))
      (setq row (replace-match
                 (format "\\\\multicolumn{%d}{%s|}{%s}" columns algn contents)
                 nil nil row 1))
      (while (and (> columns 1) (string-match "&" row start))
        (setq row (replace-match "" nil nil row))
        (cl-decf columns))))
  row)

(defun org-export-multicolumn-filter-latex (row backend info)
  (while (string-match
          "\\(<\\([0-9]+\\)col\\([lrc]\\)?>[[:blank:]]*\\(.*?\\)\\)\\(?:&\\|\\\\\\\\\\)" row)
    (let ((columns (string-to-number (match-string 2 row)))
          (start (match-end 0))
          (contents (replace-regexp-in-string
                     "\\\\" "\\\\\\\\"
                     (replace-regexp-in-string "[[:blank:]]*$" ""
                                               (match-string 4 row))))
          (algn (or (match-string 3 row) "l")))
      (setq row (replace-match
                 (format "\\\\multicolumn{%d}{%s}{%s}" columns algn contents)
                 nil nil row 1))
      (while (and (> columns 1) (string-match "&" row start))
        (setq row (replace-match "" nil nil row))
        (cl-decf columns))))
  row)

(defun org-export-multirow-filter-latex (row backend info)
  (while (string-match
          "\\(<\\([0-9]+\\)row\\([lrc\\*]\\)?>[[:blank:]]*\\(.*?\\)\\)\\(?:&\\|\\\\\\\\\\)" row)
    (let ((columns (string-to-number (match-string 2 row)))
          (start (match-end 0))
          (contents (replace-regexp-in-string
                     "\\\\" "\\\\\\\\"
                     (replace-regexp-in-string "[[:blank:]]*$" ""
                                               (match-string 4 row))))
          (algn (or (match-string 3 row) "l")))
      (setq row (replace-match
                 (format "\\\\multirow{%d}{%s}{%s}" columns algn contents)
                 nil nil row 1))))
  row)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-export-filter-table-row-functions
               'org-export-cmidrule-filter-latex)
  (add-to-list 'org-export-filter-table-row-functions
               'org-export-multicolumn-filter-latex)
  (add-to-list 'org-export-filter-table-row-functions
               'org-export-multirow-filter-latex)
  (add-to-list 'org-export-filter-table-row-functions
               'org-export-multicolumnv-filter-latex))

(defun convert-zotero-links-in-buffer ()
  "Convert Zotero links to Org-mode links in the current buffer."
  (interactive)
  (evil-ex "%s/(\\[\\([^)]+\\)\\](\\([^)]+))\\)/\([[\\2][\\1]]\)/g")
  (evil-ex "%s/^\\(«.+»\\)\\(.*\\)/***\\2\\n\\1/g/")
  (evil-ex "%s/^\(".+"\)\(.*\)/***\2\n\1/g/"))

(defun modify-and-paste-clipboard-content-at-end ()
  "Modify the clipboard's content to convert Zotero
links to Org-mode links and paste it at the end of the buffer."
  (interactive)
  (let ((content (gui-get-selection 'CLIPBOARD)))
    (setq content (replace-regexp-in-string
                   "\\[\\([^]]+\\)\\](\\([^)]+\\))"
                   "[[\\2][\\1]]"
                   content))
    (insert content)))

(map! :leader
      :desc "Modify clipboard and paste at end"
      "y z" #'modify-and-paste-clipboard-content-at-end)

(defun my/org-color-link-follow (path)
  "A function that could be used to follow the color link, but is not used here."
  (message "This link is for display only."))

(defun my/org-color-link-export (path desc backend)
  "Export the color link with PATH, DESC, and depending on BACKEND."
  (cond ((eq backend 'html)
         (format "<span style=\"color:%s;\">%s</span>" path desc))
        ((eq backend 'latex)
         (let ((processed-desc (replace-citations desc)))
           (format "\\textcolor{%s}{%s}" path processed-desc)))
        (t desc)))

(defun my/org-color-link-face (path)
  "Return a face for the color link."
  (list :foreground path))

(org-link-set-parameters "color"
                         :follow #'my/org-color-link-follow
                         :export #'my/org-color-link-export
                         :face 'my/org-color-link-face)

(defun my/org-delete-link-follow (path)
  "A function that could be used to follow the delete link, but is not used here."
  (message "This link is for display only."))

(defun my/org-delete-link-export (path desc backend)
  "Export the delete link with PATH, DESC, and depending on BACKEND."
  (let ((color (if (string-empty-p path) "red" path)))
    (cond ((eq backend 'html)
           (format "<span style=\"color:%s; text-decoration: line-through;\">%s</span>" color desc))
          ((eq backend 'latex)
           (let ((processed-desc (replace-citations desc)))
             (format "\\textcolor{%s}{\\sout{%s}}" color processed-desc)))
          (t desc))))

(defun my/org-delete-link-face (path)
  "Return a face for the delete link with specified PATH as color."
  (let ((color (if (string-empty-p path) "red" path)))
    (list :foreground color :strike-through t)))

(org-link-set-parameters "delete"
                         :follow #'my/org-delete-link-follow
                         :export #'my/org-delete-link-export
                         :face 'my/org-delete-link-face)

(defvar org-latexmk--last-comp-buf nil
  "Last latexmk compilation buffer used by `org-compile-latex-and-close-latexmk'.")

(defun org-compile-latex-and-close-latexmk (&optional open-pdf)
  "Export current Org buffer to LaTeX, then compile via latexmk (async).

- Does NOT pop up the compilation buffer.
- Silences Org export 'Wrote ...' messages.
- When finished successfully, prints: \"PDF exported: ...\".
If OPEN-PDF is non-nil (C-u), open the resulting PDF when compilation succeeds."
  (interactive "P")
  (require 'subr-x)

  (let* ((org-buf (current-buffer))
         (texfile (let ((inhibit-message t)
                        (message-log-max nil))
                    (org-latex-export-to-latex nil nil nil t nil))))
    (when texfile
      (let* ((texdir (file-name-directory texfile))
             (master-name (or (and (boundp 'TeX-master)
                                   (stringp TeX-master)
                                   (not (string-empty-p TeX-master))
                                   TeX-master)
                              "main.tex"))
             (master (cond
                      ((file-name-absolute-p master-name) master-name)
                      ((file-exists-p (expand-file-name master-name texdir))
                       (expand-file-name master-name texdir))
                      ((file-exists-p (expand-file-name master-name default-directory))
                       (expand-file-name master-name default-directory))
                      (t (expand-file-name master-name texdir))))
             (pdffile (concat (file-name-sans-extension master) ".pdf"))
             (comp-buf-name (format "*latexmk: %s*"
                                    (file-name-nondirectory master)))
             (default-directory (file-name-directory master)))

        (when-let ((tex-buf (get-file-buffer texfile)))
          (bury-buffer tex-buf))

        (setq org-latexmk--last-comp-buf (get-buffer-create comp-buf-name))
        (with-current-buffer org-latexmk--last-comp-buf
          (let ((inhibit-read-only t))
            (erase-buffer))
          (compilation-mode))

        (let* ((cmd (format "latexmk -pdf -synctex=1 -interaction=nonstopmode %s"
                            (shell-quote-argument (file-name-nondirectory master))))
               (proc (start-process-shell-command "latexmk"
                                                  org-latexmk--last-comp-buf
                                                  cmd)))
          (set-process-sentinel
           proc
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (if (= (process-exit-status p) 0)
                   (progn
                     (when (file-exists-p pdffile)
                       (message "PDF exported: %s" (abbreviate-file-name pdffile))
                       (when open-pdf
                         (org-open-file pdffile))))
                 (message "latexmk failed (see buffer %s)"
                          (buffer-name org-latexmk--last-comp-buf)))))))

        (when (buffer-live-p org-buf)
          (pop-to-buffer org-buf))

        pdffile))))

(map! :leader :desc "compile latex" "l c" #'org-compile-latex-and-close-latexmk)
