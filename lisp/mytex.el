;;; lisp/mytex.el -*- lexical-binding: t; -*-
(defun org-export-latex-body-only ()
  "Export current Org file to a LaTeX file with body only, open and compile it."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil)) ;; Run the compile command
;; map to space l b
(map! :leader :desc "export latex body only" "l b" #'org-export-latex-body-only)

;; map space m J to jump to the exported latex


;; (defun compile-main-tex-with-latexmk ()
;;   "Compile the main.tex file using latexmk."
;;   (interactive)
;;   (let ((command "latexmk -pdf -pdflatex='pdflatex -interaction=nonstopmode' -use-make main.tex"))
;;     (shell-command command)
;;     (message "Compilation with latexmk finished!")))

(defun compile-main-tex-with-latexmk-no-popup ()
  "Compile the main.tex file using latexmk without popping up the shell window."
  (interactive)
(let ((process (start-process "latexmk" "*latexmk-output*" "latexmk"
                              "-pdf"
                              "-pdflatex=pdflatex -interaction=nonstopmode -synctex=1"
                              "-use-make"
                              "main.tex")))
    (set-process-sentinel process
                          (lambda (proc event)
                            (when (string= event "finished\n")
                              (message "Compilation with latexmk finished!"))))))

(defun org-compile-latex ()
  "Export current Org file to a LaTeX file with body only,
compile it, then switch back to the Org file and kill the LaTeX buffer."
  (interactive)
  (let ((original-buffer (current-buffer)) ; Store the current buffer (Org file)
        (output-file (org-latex-export-to-latex nil nil nil t nil))) ; Export and get the output file name
    (find-file output-file) ; Open the LaTeX file
    (call-interactively '+latex/compile) ; Run the compile command
    (switch-to-buffer original-buffer) ; Switch back to the Org file
    ))

;; (defun org-compile-latex-and-close ()
;;   "Export current Org file to a LaTeX file with body only, compile it,
;; and then kill the LaTeX buffer after compilation, preserving any existing sentinel behavior."
;;   (interactive)
;;   (let ((original-buffer (current-buffer)))  ; Store the current Org file buffer
;;     ;; Export and get the LaTeX output file name
;;     (when-let ((output-file (org-latex-export-to-latex nil nil nil t nil)))
;;       ;; Open the LaTeX file in the background
;;       (let ((latex-buffer (find-file-noselect output-file)))
;;         (with-current-buffer latex-buffer
;;           ;; Call the compile command interactively in the LaTeX buffer
;;           (call-interactively '+latex/compile)
;;           ;; Capture any existing sentinel attached to the compile process
;;           (let ((existing-sentinel (process-sentinel (get-buffer-process (current-buffer)))))
;;             ;; Set a new process sentinel that incorporates the old one
;;             (set-process-sentinel
;;              (get-buffer-process (current-buffer))
;;              (lambda (proc event)
;;                ;; Call the existing sentinel, if there was one
;;                (when existing-sentinel
;;                  (funcall existing-sentinel proc event))
;;                ;; New behavior based on the process event
;;                (cond
;;                 ((string-match-p "finished" event)
;;                  ;; (message "Compilation succeeded")
;;                  (kill-buffer latex-buffer))
;;                 ((string-match-p "exited abnormally" event)
;;                  ;; (message "Compilation failed with errors")
;;                  ))
;;                (when (buffer-live-p original-buffer)
;;                  (switch-to-buffer original-buffer)
;;                  (hermanhelf-org-jump-to-pdf)
;;                  )))))))))

;; (defun org-compile-latex-and-close ()
;;   "Export current Org file to a LaTeX file with body only, compile it,
;; and then kill the LaTeX buffer after compilation, preserving any existing sentinel behavior."
;;   (interactive)
;;   (let ((original-buffer (current-buffer)))  ; Store the current Org file buffer
;;     ;; Export and get the LaTeX output file name
;;     (when-let ((output-file (org-latex-export-to-latex nil nil nil t nil)))
;;       ;; Open the LaTeX file in the background
;;       (let ((latex-buffer (find-file-noselect output-file)))
;;         (with-current-buffer latex-buffer
;;           ;; Call the compile command interactively in the LaTeX buffer
;;           (call-interactively '+latex/compile)
;;           ;; Capture any existing sentinel attached to the compile process
;;           (let ((existing-sentinel (process-sentinel (get-buffer-process (current-buffer)))))
;;             ;; Set a new process sentinel that incorporates the old one
;;             (set-process-sentinel
;;              (get-buffer-process (current-buffer))
;;              (lambda (proc event)
;;                ;; Call the existing sentinel, if there was one
;;                (when existing-sentinel
;;                  (funcall existing-sentinel proc event))
;;                ;; New behavior based on the process event
;;                (cond
;;                 ((string-match-p "finished" event)
;;                  ;; (message "Compilation succeeded")
;;                  (bury-buffer latex-buffer))
;;                 ((string-match-p "exited abnormally" event)
;;                  ;; (message "Compilation failed with errors")
;;                  ))
;;                (when (buffer-live-p original-buffer)
;;                  (switch-to-buffer original-buffer))))))))))

(defun org-compile-latex-and-close (&optional open-pdf)
  "Export current Org buffer to LaTeX, then compile the master TeX file
using `org-latex-compile`. Kills the intermediate Org-exported .tex buffer.
If OPEN-PDF is non-nil, open the resulting PDF."
  (interactive "P")
  (let* ((org-buf (current-buffer))
         (texfile (org-latex-export-to-latex nil nil nil t nil)))
    (when texfile
      ;; Determine the master TeX file
      (let* ((master (or (and (boundp 'TeX-master)
                              (stringp TeX-master)
                              (not (string= TeX-master ""))
                              TeX-master)
                         ;; fallback: plain "main.tex" in current dir
                         "main.tex"))
             (pdffile (org-latex-compile master nil open-pdf)))
        ;; Kill the exported .tex buffer
        (when-let ((tex-buf (get-file-buffer texfile)))
          (kill-buffer tex-buf))
        ;; Return to the Org buffer
        (when (buffer-live-p org-buf)
          (pop-to-buffer org-buf))
        pdffile))))

;; map compile latex to space l c
(map! :leader :desc "compile latex" "l c" #'org-compile-latex-and-close)

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

;; (defun my-org-export-remove-amps (row backend info)
;;   "Filter function to remove a number of '&' signs as specified in <rm[0-9]> pattern."
;;   (when (eq backend 'latex)  ; Only apply this for LaTeX export, adjust as necessary
;;     (let ((new-row row)
;;           match number)
;;       ;; Find the pattern and extract the number
;;       (when (string-match "<rm\\([0-9]+\\)>" row)
;;         (setq number (string-to-number (match-string 1 row)))  ; Get the number following 'rm'
;;         ;; Remove the pattern itself from the row
;;         (setq new-row (replace-regexp-in-string "<rm[0-9]+>" "" row))
;;         ;; Remove the specified number of '&' signs after the pattern
;;         (with-temp-buffer
;;           (insert new-row)
;;           (goto-char (point-min))
;;           (cl-loop repeat number
;;                    when (search-forward "&" nil t)  ; Search for '&'
;;                    do (replace-match "" nil t))  ; Replace '&' with nothing
;;           (setq new-row (buffer-string))))
;;       new-row)))

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
                 nil nil row 1))
      ))
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

;; (defun org-attach-expand (file &optional convert-to-relative)
;; "Return the full path to the current entry's attachment file FILE.
;; Basically, this adds the path to the attachment directory. If optional
;; argument `convert-to-relative' is non-nil, then return path relative to
;; `default-directory'."
;; (let ((filepath (expand-file-name file (org-attach-dir))))
;; (if convert-to-relative
;;         (dired-make-relative filepath)
;; filepath)))

;; (defun org-attach-expand-new (file)
;;   "Return a simple concatenation of the attachment directory and FILE."
;;   (let ((attach-dir (org-attach-dir)))
;;     (if attach-dir
;;         (concat (file-name-as-directory attach-dir) file)
;;       (error "No attachment directory exists"))))

;;   (defun org-attach-expand-links (_)
;;     "Expand links in current buffer.
;;   It is meant to be added to `org-export-before-parsing-hook'."
;;     (save-excursion
;;       (while (re-search-forward "attachment:" nil t)
;;         (let ((link (org-element-context)))
;;     (when (and (org-element-type-p link 'link)
;;            (string-equal "attachment"
;;                  (org-element-property :type link)))
;;       (let* ((description (and (org-element-contents-begin link)
;;                    (buffer-substring-no-properties
;;                     (org-element-contents-begin link)
;;                     (org-element-contents-end link))))
;;          (file (org-element-property :path link))
;;          (new-link (org-link-make-string
;;                 ;; only difference is the following two lines
;;                 ;; (concat "file:" (org-attach-expand file))
;;                 (concat "file:" (org-attach-expand file 'convert-to-relative))
;;                 description)))
;;         (goto-char (org-element-end link))
;;         (skip-chars-backward " \t")
;;         (delete-region (org-element-begin link) (point))
;;         (insert new-link)))))))

;; (advice-add 'org-export-resolve-fuzzy-link :override #'my/org-export-resolve-fuzzy-link)
;; (defun my/org-export-resolve-fuzzy-link (link info &rest pseudo-types)
;;   "Return LINK destination.

;; INFO is a plist holding contextual information.

;; Return value can be an object or an element:

;; - If LINK path matches a target object (i.e. <<path>>) return it.

;; - If LINK path exactly matches the name or results affiliated keyword
;;   (i.e. #+NAME: path or #+RESULTS: name) of an element, return that
;;   element.

;; - If LINK path exactly matches any headline name, return that
;;   element.

;; - Otherwise, throw an error.

;; PSEUDO-TYPES are pseudo-elements types, i.e., elements defined
;; specifically in an export backend, that could have a name
;; affiliated keyword.

;; Assume LINK type is \"fuzzy\".  White spaces are not
;; significant."
;;   (let* ((search-cells (org-export-string-to-search-cell
;; 			(org-element-property :path link)))
;; 	 (link-cache (or (plist-get info :resolve-fuzzy-link-cache)
;; 			 (let ((table (make-hash-table :test #'equal)))
;;                            ;; Cache all the element search cells.
;;                            (org-element-map (plist-get info :parse-tree)
;; 		               (append pseudo-types '(target) org-element-all-elements)
;; 	                     (lambda (datum)
;; 		               (dolist (cell (org-export-search-cells datum))
;; 		                 (if (gethash cell table)
;;                                      (push datum (gethash cell table))
;;                                    (puthash cell (list datum) table)))))
;; 			   (plist-put info :resolve-fuzzy-link-cache table)
;; 			   table)))
;; 	 (cached (gethash search-cells link-cache 'not-found)))
;;     (if (not (eq cached 'not-found)) cached
;;       (let ((matches
;;              (let (result)
;;                (dolist (search-cell search-cells)
;;                  (setq result
;;                        (nconc
;;                         result
;; 	                (gethash search-cell link-cache))))
;;                (delq nil result))))
;; 	(if (null matches)
;; 	  ;; (signal 'org-link-broken (list (org-element-property :path link)))
;;     nil
;;   (puthash
;;     search-cells
;;     ;; There can be multiple matches for un-typed searches, i.e.,
;;     ;; for searches not starting with # or *.  In this case,
;;     ;; prioritize targets and names over headline titles.
;;     ;; Matching both a name and a target is not valid, and
;;     ;; therefore undefined.
;;     (or (cl-some (lambda (datum)
;;         (and (not (org-element-type-p datum 'headline))
;;             datum))
;;             matches)
;;         (car matches))
;;     link-cache)
;;     )
;; 	))))

;; (setq org-latex-link-with-unknown-path-format "\\ref{%s}")

(defun convert-zotero-links-in-buffer ()
"Convert Zotero links to Org-mode links in the current buffer."
(interactive)
(evil-ex "%s/(\\[\\([^)]+\\)\\](\\([^)]+))\\)/\([[\\2][\\1]]\)/g")
(evil-ex "%s/^\\(«.+»\\)\\(.*\\)/***\\2\\n\\1/g/")
(evil-ex "%s/^\(“.+”\)\(.*\)/***\2\n\1/g/")
)

(defun modify-and-paste-clipboard-content-at-end ()
  "Modify the clipboard's content to convert Zotero
links to Org-mode links and paste it at the end of the buffer."
  (interactive)
  (let ((content (gui-get-selection 'CLIPBOARD)))
    ;; Perform replacements to convert Zotero links to Org-mode format without changing the order
    (setq content (replace-regexp-in-string
                   "\\[\\([^]]+\\)\\](\\([^)]+\\))"
                   "[[\\2][\\1]]"
                   content))
    ;; Go to the end of the buffer and insert the modified content
    (with-current-buffer (current-buffer)  ; Ensure we're in the current buffer
      (insert content)
      )
  )
)

;; Map the function to the key sequence `SPC y z`
(map! :leader
      :desc "Modify clipboard and paste at end"
      "y z" #'modify-and-paste-clipboard-content-at-end)

;;; org-colored-text.el --- Colored text for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;; Taken and adapted from org-colored-text
;; (org-add-link-type
;;  "color"
;;  (lambda (path)
;;    "No follow action.")
;;  (lambda (color description backend)
;;    (let ((description (replace-citations description))) ; Apply citation replacement to description
;;      (cond
;;       ((eq backend 'latex)
;;        (format "{\\color{%s}%s}" color description))
;;       ((eq backend 'html)
;;        (let ((rgb (assoc color color-name-rgb-alist))
;;              r g b)
;;          (if rgb
;;              (progn
;;                (setq r (* 255 (/ (nth 1 rgb) 65535.0))
;;                      g (* 255 (/ (nth 2 rgb) 65535.0))
;;                      b (* 255 (/ (nth 3 rgb) 65535.0)))
;;                (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
;;                        (truncate r) (truncate g) (truncate b)
;;                        description))
;;            (format "No Color RGB for %s" color))))))))

;; (defface my-red-link
;;   '((t (:foreground "red" :underline t)))
;;   "Custom face for red links.")

;; (org-link-set-parameters "red"
;;                          :face 'my-red-link)

;; (defun next-color-link (limit)
;;   (when (re-search-forward
;; 	 "color:[a-zA-Z]\\{2,\\}" limit t)
;;     (forward-char -2)
;;     (let* ((next-link (org-element-context))
;; 	   color beg end post-blanks)
;;       (if next-link
;; 	  (progn
;; 	    (setq color (org-element-property :path next-link)
;; 		  beg (org-element-property :begin next-link)
;; 		  end (org-element-property :end next-link)
;; 		  post-blanks (org-element-property :post-blank next-link))
;; 	    (set-match-data
;; 	     (list beg
;; 		   (- end post-blanks)))
;; 	    (ov-clear beg end 'color)
;; 	    (ov beg
;; 		(- end post-blanks)
;; 	     'color t
;; 	     'face
;; 	     `((:foreground ,color)))
;; 	    (goto-char end))
;; 	(goto-char limit)
;; 	nil))))


;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (font-lock-add-keywords
;; 	     nil
;; 	     '((next-color-link (0 'org-link t)))
;; 	     t)))


(defun my/org-color-link-follow (path)
  "A function that could be used to follow the color link, but is not used here."
  (message "This link is for display only."))

(defun my/org-color-link-export (path desc backend)
  "Export the color link with PATH, DESC, and depending on BACKEND."
  (cond ((eq backend 'html)
         (format "<span style=\"color:%s;\">%s</span>" path desc))
        ((eq backend 'latex)
         ;; Apply citation replacement only for LaTeX exports
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
  (let ((color (if (string-empty-p path) "red" path))) ; Default to red if no color specified
    (cond ((eq backend 'html)
           (format "<span style=\"color:%s; text-decoration: line-through;\">%s</span>" color desc))
          ((eq backend 'latex)
           (let ((processed-desc (replace-citations desc)))
           (format "\\textcolor{%s}{\\sout{%s}}" color desc)))
          (t desc))))

(defun my/org-delete-link-face (path)
  "Return a face for the delete link with specified PATH as color."
  (let ((color (if (string-empty-p path) "red" path))) ; Default to red if no color specified
    (list :foreground color :strike-through t)))

(org-link-set-parameters "delete"
                         :follow #'my/org-delete-link-follow
                         :export #'my/org-delete-link-export
                         :face 'my/org-delete-link-face)


