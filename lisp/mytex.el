;;; lisp/mytex.el -*- lexical-binding: t; -*-
(defun org-export-latex-body-only ()
  "Export current Org file to a LaTeX file with body only, open and compile it."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil)) ;; Run the compile command
;; map to space l b
(map! :leader :desc "export latex body only" "l b" #'org-export-latex-body-only)

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
and then kill the LaTeX buffer after compilation."
  (interactive)
  (let ((original-buffer (current-buffer)))  ; Store the current Org file buffer
    ;; Export and get the LaTeX output file name
    (when-let ((output-file (org-latex-export-to-latex nil nil nil t nil)))
      ;; Open the LaTeX file in the background
      (let ((latex-buffer (find-file-noselect output-file)))
        (with-current-buffer latex-buffer
          ;; Call the compile command interactively in the LaTeX buffer
          (call-interactively '+latex/compile)
          ;; Set a process sentinel to close the buffer once the process completes
          (set-process-sentinel
           (get-buffer-process (current-buffer))
           (lambda (proc _)
             (when (memq (process-status proc) '(exit signal))
               (kill-buffer latex-buffer)  ; Kill the LaTeX buffer
               (when (buffer-live-p original-buffer)
                 (switch-to-buffer original-buffer))))))))))

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

(defun my-org-export-remove-amps (row backend info)
  "Filter function to remove a number of '&' signs as specified in <rm[0-9]> pattern."
  (when (eq backend 'latex)  ; Only apply this for LaTeX export, adjust as necessary
    (let ((new-row row)
          match number)
      ;; Find the pattern and extract the number
      (when (string-match "<rm\\([0-9]+\\)>" row)
        (setq number (string-to-number (match-string 1 row)))  ; Get the number following 'rm'
        ;; Remove the pattern itself from the row
        (setq new-row (replace-regexp-in-string "<rm[0-9]+>" "" row))
        ;; Remove the specified number of '&' signs after the pattern
        (with-temp-buffer
          (insert new-row)
          (goto-char (point-min))
          (cl-loop repeat number
                   when (search-forward "&" nil t)  ; Search for '&'
                   do (replace-match "" nil t))  ; Replace '&' with nothing
          (setq new-row (buffer-string))))
      new-row)))

;; Add the function to the org export filter for table rows
(with-eval-after-load 'ox-latex
  (add-to-list 'org-export-filter-table-row-functions
               'org-export-cmidrule-filter-latex)
  (add-to-list 'org-export-filter-table-row-functions
               'my-org-export-remove-amps))
