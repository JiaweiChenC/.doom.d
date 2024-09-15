;;; lisp/mytex.el -*- lexical-binding: t; -*-
(defun org-export-latex-body-only ()
  "Export current Org file to a LaTeX file with body only, open and compile it."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil)) ;; Run the compile command
;; map to space l b
(map! :leader :desc "export latex body only" "l b" #'org-export-latex-body-only)


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

(defun org-compile-latex-and-close ()
  "Export current Org file to a LaTeX file with body only, compile it,
and then kill the LaTeX buffer after compilation, preserving any existing sentinel behavior."
  (interactive)
  (let ((original-buffer (current-buffer)))  ; Store the current Org file buffer
    ;; Export and get the LaTeX output file name
    (when-let ((output-file (org-latex-export-to-latex nil nil nil t nil)))
      ;; Open the LaTeX file in the background
      (let ((latex-buffer (find-file-noselect output-file)))
        (with-current-buffer latex-buffer
          ;; Call the compile command interactively in the LaTeX buffer
          (call-interactively '+latex/compile)
          ;; Capture any existing sentinel attached to the compile process
          (let ((existing-sentinel (process-sentinel (get-buffer-process (current-buffer)))))
            ;; Set a new process sentinel that incorporates the old one
            (set-process-sentinel
             (get-buffer-process (current-buffer))
             (lambda (proc event)
               ;; Call the existing sentinel, if there was one
               (when existing-sentinel
                 (funcall existing-sentinel proc event))
               ;; New behavior based on the process event
               (cond
                ((string-match-p "finished" event)
                 ;; (message "Compilation succeeded")
                 (kill-buffer latex-buffer))
                ((string-match-p "exited abnormally" event)
                 ;; (message "Compilation failed with errors")
                 ))
               (when (buffer-live-p original-buffer)
                 (switch-to-buffer original-buffer))))))))))

;; map compile latex to space l c
(map! :leader :desc "compile latex" "l c" #'org-compile-latex)

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


(defun org-attach-expand-new (file)
  "Return a simple concatenation of the attachment directory and FILE."
  (let ((attach-dir (org-attach-dir)))
    (if attach-dir
        (concat (file-name-as-directory attach-dir) file)
      (error "No attachment directory exists"))))

(defun org-attach-expand-links (_)
  "Expand links in current buffer.
It is meant to be added to `org-export-before-parsing-hook'."
  (save-excursion
    (while (re-search-forward "attachment:" nil t)
      (let ((link (org-element-context)))
	(when (and (org-element-type-p link 'link)
		   (string-equal "attachment"
				 (org-element-property :type link)))
	  (let* ((description (and (org-element-contents-begin link)
				   (buffer-substring-no-properties
				    (org-element-contents-begin link)
				    (org-element-contents-end link))))
		 (file (org-element-property :path link))
		 (new-link (org-link-make-string
			    (concat "file:" (org-attach-expand-new file))
			    description)))
	    (goto-char (org-element-end link))
	    (skip-chars-backward " \t")
	    (delete-region (org-element-begin link) (point))
	    (insert new-link)))))))
