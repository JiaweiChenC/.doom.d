;;; lisp/skim.el -*- lexical-binding: t; -*-
;; open in another frame
(require 's)
(require 'pdf-sync)

(defun search-in-file (needle filename)
  (select-frame-set-input-focus (selected-frame))
  (find-file filename)
  (goto-char 0)
  (search-forward needle)
  (hl-line-highlight)
  (beginning-of-line)
  (evil-scroll-line-to-center nil)
  ;; find the line at the point if it is on a heading, open the heading
  )

(defun replace-ref-with-brackets (line)
  "Replace all occurrences of \\ref{...} in LINE with [[...]]."
  (let ((pattern "\\\\ref{\\([^}]+\\)}")
        (replacement "[[\\1]]"))
    (replace-regexp-in-string pattern replacement line)))

(defun remove-labels (text)
  "Remove all occurrences of LaTeX \\label{...} from TEXT."
  (replace-regexp-in-string "\\\\label{[^}]*}" "" text))

(defun extract-caption-content (line)
  "Extracts the text inside the \\caption{} from the given LINE."
  (when (string-match "\\\\caption{\\(.*?\\)}" line)
    (match-string 1 line)))

;; Example usage with your string
(defun extract-caption (line)
  (extract-caption-content (remove-labels line))
  )

(defun extract-cite-targets (line)
  "Extract targets from the first occurrence of \\cite{target1, target2} in LINE."
  (when (string-match "\\\\cite{\\([^}]+\\)}" line)
    (split-string (match-string 1 line) ", *" t)))

(defun replace-first-cite-with-brackets (line)
  "Replace the first occurrence of \\cite{...} in LINE with [cite:@...]."
  (if (string-match "\\\\cite{\\([^}]+\\)}" line)
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (targets (split-string (match-string 1 line) ", *" t))
             (formatted-citation (format-citation-targets targets)))
        (concat (substring line 0 begin)
                formatted-citation
                (substring line end)))
    line))

(defun format-citation-targets (targets)
  "Formats a list of citation TARGETS into a string
formatted as [cite:@target1;@target2;@target3]."
  (concat "[cite:"
          (mapconcat (lambda (target) (concat "@" target)) targets ";")
          "]"))

(defun replace-all-cites-with-brackets (line)
  "Replace all occurrences of \\cite{...} in LINE with
[cite:@...], using iterative single replacements."
  (while (string-match "\\\\cite{\\([^}]+\\)}" line)
    (setq line (replace-first-cite-with-brackets line)))
  line)

(defun heading-text-latex (origin-text)
  "return heading text from a string ORIGIN-TEXT containing {heading}"
  (let* ((heading-start (1+ (string-search "{" origin-text)) )
        (heading-end  (string-search "}" origin-text))
        (heading-text-latex (substring origin-text heading-start heading-end)))
    heading-text-latex))

(defun heading-text-org (origin-text)
  "get heading from * heading "
(let* ((heading-start (+ 2 (string-search "* " origin-text)) )
        (heading-end  nil)
        (heading-text-latex (substring origin-text heading-start)))
    heading-text-latex))

(defun remove-trailing-whitespace (str)
  "Remove trailing whitespace and newlines from the string STR."
  (if (string-match "\\(.*?\\)[ \t\n\r]*$" str)
      (match-string 1 str)
    str))

;; (defun hermanhelf-latex-jump-to-org ()
;;   (interactive)
;;   ;; write me a elisp snippet that gets the start and end position of the current line at point
;;   (let* ((line (thing-at-point 'line t))
;;         (tex-filename (buffer-file-name))
;;         (org-filename (concat (file-name-directory (directory-file-name (file-name-directory tex-filename)))
;;                         (file-name-base tex-filename)
;;                         ".org")))
;;         ;; (kill-buffer (current-buffer))
;;         ;; (let ((processed-line (replace-cite-with-brackets line)))
;;         (let ((processed-line (remove-trailing-whitespace
;;                         (replace-all-cites-with-brackets
;;                                 (replace-ref-with-brackets line)))))
;;         (if (s-contains? "section{" processed-line t)
;;         (search-in-file (concat "* " (heading-text-latex processed-line)) org-filename) ;; it is a heading, find the text in the {}, and search for "* heading" in the corresponding org-file
;;         (if (s-contains? "includesvg" processed-line t)
;;         (search-in-file (extract-graphics-path processed-line) org-filename)
;;         (if (s-contains? "\\caption{" processed-line t)
;;         (search-in-file (extract-caption processed-line) org-filename)
;;          (search-in-file processed-line org-filename)))))))

(defun hermanhelf-latex-jump-to-org ()
  (interactive)
  ;; Get the start and end position of the current line at point
  (let* ((line (thing-at-point 'line t))
         (tex-filename (buffer-file-name))
         (org-filename (concat (file-name-directory (directory-file-name (file-name-directory tex-filename)))
                               (file-name-base tex-filename)
                               ".org")))
    ;; Check if the Org file exists
    (if (file-exists-p org-filename)
        (let ((processed-line (remove-trailing-whitespace
                               (replace-all-cites-with-brackets
                                (replace-ref-with-brackets line))))
              (tex-buffer (current-buffer)))  ; Save the current buffer to close later
          (cond
           ((s-contains? "section{" processed-line t)
            (search-in-file (concat "* " (heading-text-latex processed-line)) org-filename))
           ((s-contains? "includesvg" processed-line t)
            (search-in-file (extract-graphics-path processed-line) org-filename))
           ((s-contains? "\\caption{" processed-line t)
            (search-in-file (extract-caption processed-line) org-filename))
           (t
            (search-in-file processed-line org-filename)))
          ;; Kill the TeX buffer after performing the search
          (kill-buffer tex-buffer))
      (message "Associated Org file does not exist: %s" org-filename))))

(defun hermanhelf-org-jump-to-latex ()
  (let* ((line (string-trim (thing-at-point 'line t)))
         (org-filename (buffer-file-name))
         (tex-filename (concat (file-name-directory org-filename) "latex/" (file-name-base org-filename) ".tex")))
    (cond
     ((s-contains? "cite:@" line t)  ;; Handles citations
      (let ((modified-line (replace-citations line)))
        (message "Modified line: %s" modified-line)
        (search-in-file modified-line tex-filename)))
     ((s-contains? "* " line t)  ;; Handles headings
      (search-in-file (concat "{" (heading-text-org line) "}") tex-filename))
     ((s-contains? "[[fig:" line t)  ;; Handles citations
      (let ((modified-line (replace-fig-ref line)))
        (message "Modified line: %s" modified-line)
        (search-in-file modified-line tex-filename)))
     ((s-contains? "[[tbl:" line t)  ;; Handles citations
      (let ((modified-line (replace-tbl-ref line)))
        (message "Modified line: %s" modified-line)
        (search-in-file modified-line tex-filename)))
     ((s-contains? "attachment:" line t)  ;; Handles attachments
      (let ((attachment-name (replace-regexp-in-string "\\[\\[attachment:\\([^]]+\\)\\]\\]" "\\1" line)))
        (setq attachment-name (replace-regexp-in-string "\\.[^.]+\\'" "" attachment-name))  ;; Remove the file extension
        (search-in-file attachment-name tex-filename)))
     ((s-contains? "#+caption:" line t)  ;; Handles captions
      (let ((caption-text (replace-regexp-in-string "^#\\+caption: " "" line)))
        (message "Caption text: %s" caption-text)
        (search-in-file caption-text tex-filename)))
     (t  ;; Handles generic lines
      (search-in-file line tex-filename)))))

(defun hermanhelf-org-jump-to-pdf ()
  (interactive)
  (ignore-errors(hermanhelf-org-jump-to-latex))
  ;; (pdf-sync-forward-search)
  (TeX-view)
  (kill-buffer (current-buffer))
  ;; (previous-buffer)
  )

(defun open-file-jump-to-line-and-call-function (file line)
  "Open FILE, jump to LINE, and call `hermanhelf-latex-jump-to-org`."
  (find-file file)
  (goto-line line)
  ;; close the current buffer
  (hermanhelf-latex-jump-to-org))

(defun extract-graphics-path (input-line)
  (let ((content (and (string-match "{\\(.*?\\)}" input-line)
                      (match-string 1 input-line))))
    (if content
        (let ((formatted-content (file-name-nondirectory content)))
          (message "Formatted Content: %s" formatted-content)
          (concat "#+NAME: fig:" formatted-content))
      (message "No content found between braces.")
      nil)))

(map! :map org-mode-map
      :localleader "j" #'hermanhelf-org-jump-to-pdf
      )

(defun replace-fig-ref (input-string)
  "Replace all occurrences of [[fig:LABEL]] in INPUT-STRING with \\ref{fig:LABEL}."
  (with-temp-buffer
    (insert input-string)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(fig:.*?\\)\\]\\]" nil t)
      (replace-match "\\\\ref{\\1}"))
    (buffer-string)))


(defun replace-tbl-ref (input-string)
  "Replace all occurrences of [[fig:LABEL]] in INPUT-STRING with \\ref{fig:LABEL}."
  (with-temp-buffer
    (insert input-string)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(tbl:.*?\\)\\]\\]" nil t)
      (replace-match "\\\\ref{\\1}"))
    (buffer-string)))
