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
  )

(defun replace-ref-with-brackets (line)
  "Replace all occurrences of \\ref{...} in LINE with [[...]]."
  (let ((pattern "\\\\ref{\\([^}]+\\)}")
        (replacement "[[\\1]]"))
    (replace-regexp-in-string pattern replacement line)))

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

(defun hermanhelf-latex-jump-to-org ()
  (interactive)
  ;; write me a elisp snippet that gets the start and end position of the current line at point
  (let* ((line (thing-at-point 'line t))
        (tex-filename (buffer-file-name))
        (org-filename (concat (file-name-directory (directory-file-name (file-name-directory tex-filename)))
                        (file-name-base tex-filename)
                        ".org")))
        ;; (kill-buffer (current-buffer))
        ;; (let ((processed-line (replace-cite-with-brackets line)))
        (let ((processed-line (remove-trailing-whitespace
                        (replace-all-cites-with-brackets
                                (replace-ref-with-brackets line)))))
        (if (s-contains? "section{" processed-line t)
        (search-in-file (concat "* " (heading-text-latex processed-line)) org-filename) ;; it is a heading, find the text in the {}, and search for "* heading" in the corresponding org-file
        (if (s-contains? "includegraphics" processed-line t)
        (search-in-file (extract-graphics-path processed-line) org-filename)
        (search-in-file processed-line org-filename)))) ;; default action if none of the above conditions are met
        ))

(defun hermanhelf-org-jump-to-latex ()
  (let* ((line (string-trim (thing-at-point 'line t)))
        (org-filename (buffer-file-name))
        (tex-filename (concat (file-name-directory org-filename) "latex/" (file-name-base org-filename) ".tex")))
        (if (s-contains? "* " line t)
            (search-in-file (concat "{" (heading-text-org line) "}") tex-filename) ;; it is a heading, find the text in the {}, and search for "* heading" in the corresponding tex-file
          (search-in-file line tex-filename);;  it is no a heading, search for the line in the tex-file
            )))

(defun hermanhelf-org-jump-to-pdf ()
  (interactive)
  (ignore-errors(hermanhelf-org-jump-to-latex))
  (pdf-sync-forward-search)
  (previous-buffer)
  )

(defun open-file-jump-to-line-and-call-function (file line)
  "Open FILE, jump to LINE, and call `hermanhelf-latex-jump-to-org`."
  (find-file file)
  (goto-line line)
  (hermanhelf-latex-jump-to-org))

(defun extract-graphics-path (input-line)
  (let ((content (and (string-match "{\\(.*?\\)}" input-line)
                      (match-string 1 input-line))))
    (if content
        (let ((formatted-content (file-name-nondirectory content)))
          (message "Formatted Content: %s" formatted-content)
          formatted-content)
      (message "No content found between braces.")
      nil)))

(map! :map org-mode-map
      :localleader "j" #'hermanhelf-org-jump-to-pdf
      )


