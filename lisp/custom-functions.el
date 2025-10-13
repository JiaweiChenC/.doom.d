;;; lisp/custom-functions.el -*- lexical-binding: t; -*-
;; (defun my/copy-image-to-clipboard ()
;;   "Copy the image at point or current image buffer to the clipboard in macOS.
;; Handles Org mode, Dired mode, and image buffers."
;;   (interactive)
;;   (cond
;;    ;; In Org mode, try to get the path and copy file
;;    ((derived-mode-p 'org-mode)
;;     (if-let ((image-path (org-element-property :path (org-element-context))))
;;         (let ((full-path (expand-file-name image-path)))
;;           (if (file-exists-p full-path)
;;               (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
;;                                      full-path
;;                                      "\") as JPEG picture)'"))
;;             (message "File does not exist: %s" full-path)))
;;       (message "No image file at point!")))
;;    ;; In Dired mode, copy the file at point
;;    ((derived-mode-p 'dired-mode)
;;     (let ((file-path (dired-get-file-for-visit)))
;;       (if (file-exists-p file-path)
;;           (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
;;                                  file-path
;;                                  "\") as JPEG picture)'"))
;;         (message "Selected file does not exist!"))))
;;    ;; In image mode, copy the image data directly
;;    ((eq major-mode 'image-mode)
;;     (let ((image-file (buffer-file-name)))
;;       (if image-file
;;           (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
;;                                  image-file
;;                                  "\") as JPEG picture)'"))
;;         (message "No file associated with this buffer!"))))
;;    ;; Default message when not in applicable mode
;;    (t (message "Not in an Org, Dired, or Image buffer!"))))

(defun my/copy-image-to-clipboard ()
  "Copy the image at point or current image buffer to the clipboard on macOS.
Supports Org mode (including attachment links), Dired, and image buffers."
  (interactive)
  (cond
   ;; Org mode: handle file and attachment links
   ((derived-mode-p 'org-mode)
    (let* ((context (org-element-context))
           (type (org-element-property :type context))
           (path (org-element-property :path context))
           (full-path
            (cond
             ;; Handle attachment: links
             ((and (string= type "attachment") path)
              (ignore-errors (org-attach-expand path)))
             ;; Handle file: links
             ((and (string= type "file") path)
              (expand-file-name path))
             (t nil))))
      (if (and full-path (file-exists-p full-path))
          (shell-command
           (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as JPEG picture)'" full-path))
        (message "No valid image file at point!"))))

   ;; Dired mode
   ((derived-mode-p 'dired-mode)
    (let ((file-path (dired-get-file-for-visit)))
      (if (file-exists-p file-path)
          (shell-command
           (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as JPEG picture)'" file-path))
        (message "Selected file does not exist!"))))

   ;; Image mode
   ((eq major-mode 'image-mode)
    (let ((image-file (buffer-file-name)))
      (if (and image-file (file-exists-p image-file))
          (shell-command
           (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as JPEG picture)'" image-file))
        (message "No file associated with this buffer!"))))

   (t
    (message "Not in an Org, Dired, or Image buffer!"))))

(map! :leader :desc "copy image file at point to clipboard" "y p" #'my/copy-image-to-clipboard)

(defun send-scroll-up-to-other-frame ()
  (interactive)
  (let ((other-frame (next-frame)))  ; Get the next frame
    (with-selected-frame other-frame  ; Work within the context of the other frame
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the frame is in pdf-view-mode
             (pdf-view-next-line-or-next-page 5))  ; If true, go to the next page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the frame is in image-mode
             (image-next-file 1))  ; If true, go to the next image file
            ((derived-mode-p 'xwidget-webkit-mode)  ; Check if the frame is in xwidget-webkit-mode
             (xwidget-webkit-scroll-up-line 5))  ; If true, scroll up a line in xwidget-webkit
            (t
             (scroll-up-command))))))  ; For all other modes, perform a normal scroll up

(defun send-scroll-down-to-other-frame ()
  (interactive)
  (let ((other-frame (next-frame)))  ; Get the next frame
    (with-selected-frame other-frame  ; Work within the context of the other frame
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the frame is in pdf-view-mode
             (pdf-view-previous-line-or-previous-page 5))  ; If true, go to the previous page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the frame is in image-mode
             (image-previous-file 1))  ; If true, go to the previous image file
            ((derived-mode-p 'xwidget-webkit-mode)  ; Check if the frame is in xwidget-webkit-mode
             (xwidget-webkit-scroll-down-line 5))  ; If true, scroll down a line in xwidget-webkit
            (t
             (scroll-down-command))))))  ; For all other modes, perform a normal scroll down

(defun send-scroll-up-to-other-window ()
  (interactive)
  (let ((other-window (next-window)))  ; Get the next window
    (with-selected-window other-window  ; Work within the context of the other window
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the window is in pdf-view-mode
             (pdf-view-next-line-or-next-page 5))  ; If true, go to the next page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the window is in image-mode
             (image-next-file 1))  ; If true, go to the next image file
            (t
             (scroll-up-command))))))  ; For all other modes, perform a normal scroll up

(defun send-scroll-down-to-other-window ()
  (interactive)
  (let ((other-window (next-window)))  ; Get the next window
    (with-selected-window other-window  ; Work within the context of the other window
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the window is in pdf-view-mode
             (pdf-view-previous-line-or-previous-page 5))  ; If true, go to the previous page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the window is in image-mode
             (image-previous-file 1))  ; If true, go to the previous image file
            (t
             (scroll-down-command))))))  ; For all other modes, perform a normal scroll down

;; Keybindings remain the same, assuming Doom Emacs keybinding syntax
;; (map! :n "C-;" #'send-scroll-up-to-other-frame)
;; (map! :n "C-'" #'send-scroll-down-to-other-frame)

(defun open-project-file-externally ()
  "Find a file in the current project and open it externally on macOS."
  (interactive)
  (require 'projectile)
  (projectile-ensure-project (projectile-project-root))
  (let ((file (projectile-completing-read "Find file: " (projectile-current-project-files))))
    (shell-command (concat "open " (shell-quote-argument (expand-file-name file (projectile-project-root)))))))

(map! :leader
      :desc "Open project file externally"
      "e e" #'open-project-file-externally)

;; (defun update-breadcrumb-mode-based-on-window-count ()
;;   "Toggle breadcrumb-mode based on the number of visible windows."
;;   (let ((more-than-one-window (> (length (window-list)) 1)))
;;     (unless (eq more-than-one-window breadcrumb-mode)
;;       (if more-than-one-window
;;           (breadcrumb-mode 1)
;;         (breadcrumb-mode -1)))))

;; (add-hook 'window-configuration-change-hook 'update-breadcrumb-mode-based-on-window-count)

(defun citar-org-roam-ref-add ()
  "Add a roam_ref to the node at point, and ensure the node has an Org ID.

This is just a wrapper for `org-roam-ref-add', with added functionality to ensure
the node has an Org ID."
  (interactive)
  (let ((ref (citar-select-ref)))
    ;; Ensure the current node has an Org ID
    (unless (org-id-get)
      (org-id-get-create))
    ;; Add the reference
    (org-roam-ref-add (concat "@" ref))))




(setq org-babel-default-header-args:jupyter-python
      '((:results . "both")
        ;; This seems to lead to buffer specific sessions!
        (:session . (lambda () (file-name-nondirectory (buffer-file-name))))
        (:kernel . "python3")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))

(defun scimax-jupyter-jump-to-error ()
  "In a src block, jump to the line indicated as an error in the results.
In a SyntaxError, there is not a traceback with a line number, so
we handle it separately. It doesn't seem like it should be that
way, but it is."
  (interactive)
  (let* ((cp (point))
         (location (org-babel-where-is-src-block-result))
         (case-fold-search t))

    (when (and location
               (goto-char location)
               (looking-at org-babel-result-regexp))
      (cond
       ;; Check for SyntaxError
       ((string-match "SyntaxError:" (buffer-substring location (org-babel-result-end)))
        (re-search-forward (rx (zero-or-more " ") "^") nil (org-babel-result-end))
        (previous-line)
        (let ((pattern (string-trim-left
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
          (goto-char cp)
          (goto-char (org-element-property :begin (org-element-context)))
          (unless
              (search-forward pattern (org-element-property :end (org-element-context)) t)
            (message "No SyntaxError found like %s" pattern))))

       ;; search for something like --> 21
       (t
        (goto-char location)
        (re-search-forward "-*> \\([[:digit:]]*\\)" (org-babel-result-end))
        (save-match-data
          (goto-char cp)
          (goto-char (org-element-property :begin (org-element-context))))
        (forward-line (string-to-number (match-string-no-properties 1))))))))

(setq org-babel-default-header-args:jupyter-R
      '((:results . "value")
        (:session . "jupyter-R")
        (:kernel . "ir")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))

(defun scimax-jupyter-ansi ()
  "Replaces ansi-codes in exceptions with colored text.
I thought emacs-jupyter did this automatically, but it may only
happen in the REPL. Without this, the tracebacks are very long
and basically unreadable.

We also add some font properties to click on goto-error.

This should only apply to jupyter-lang blocks."
  (when (string-match "^jupyter" (car (or (org-babel-get-src-block-info t) '(""))))
    (let* ((r (org-babel-where-is-src-block-result))
           (result (when r
                     (save-excursion
                       (goto-char r)
                       (org-element-context)))))
      (when result
        (ansi-color-apply-on-region (org-element-property :begin result)
                                    (org-element-property :end result))

        ;; Let's fontify "# [goto error]" to it is clickable
        (save-excursion
          (goto-char r)
          (when (search-forward "# [goto error]" (org-element-property :end result) t)
            (add-text-properties
             (match-beginning 0) (match-end 0)
             (list 'help-echo "Click to jump to error."
                   'mouse-face 'highlight
                   'local-map (let ((map (copy-keymap help-mode-map)))
                                (define-key map [mouse-1] (lambda ()
                                                            (interactive)
                                                            (search-backward "#+BEGIN_SRC")
                                                            (scimax-jupyter-jump-to-error)))
                                map))))))

      t)))


(add-to-list 'org-babel-after-execute-hook 'scimax-jupyter-ansi t)


;; attachment customization
(setq org-attach-store-link-p 'attached)

;; map org attach attach to spc m a A
;; (map! :leader :desc "org attach attach" "m a A" #'org-attach-attach)

;; (defun scimax-org-attach-attach-advice (&rest file)
;;   "Add link to attached file in the property"
;;   (let ((attach (pop org-stored-links)))
;;     (org-entry-put (point) "ATTACHMENTS"
;;                    (concat
;;                     (org-entry-get (point) "ATTACHMENTS")
;;                     (format " [[%s][%s]]" (car attach) (cadr attach))))))

;; (advice-add 'org-attach-attach :after 'scimax-org-attach-attach-advice)

;; (defun my/org-open-attachment-from-property ()
;;   "If there is one attachment in the ATTACHMENTS property, open it.
;;    If more, prompt to select."
;;   (interactive)
;;   (require 'org)
;;   (let* ((attachments (org-entry-get (point) "ATTACHMENTS"))
;;          (links (when attachments
;;                   (save-match-data
;;                     (let (result)
;;                       (with-temp-buffer
;;                         (insert attachments)
;;                         (goto-char (point-min))
;;                         (while (re-search-forward
;;                                 "\\[\\[\\(attachment:\\(.*?\\)\\)\\]\\[\\(.*?\\)\\]\\]"
;;                                 nil t)
;;                           (push (cons (match-string 3) (match-string 2)) result))
;;                       (nreverse result))))))
;;          (target
;;           (cond
;;            ((null links) nil)
;;            ((= (length links) 1)
;;             (cdar links))  ;; Only one, use its filename directly
;;            (t
;;             (let ((choice (completing-read "Open attachment: " (mapcar #'car links) nil t)))
;;               (cdr (assoc choice links)))))))
;;     (when target
;;       (find-file (expand-file-name target (org-attach-dir t))))))

;; ;; map to spc m a h

;; (defun my/org-attach-new-add-link-to-attachments (file)
;;   "After creating new FILE, add its link to the ATTACHMENTS property at the right Org entry."
;;   (let ((filename (file-name-nondirectory file))
;;         (origin-buffer (current-buffer))) ;; save the attachment buffer
;;     (when (org-back-to-heading t) ;; move to org heading if possible
;;       (let* ((link (format "[[attachment:%s][%s]]" filename filename))
;;              (current (org-entry-get (point) "ATTACHMENTS")))
;;         (org-entry-put (point) "ATTACHMENTS"
;;                        (if (and current (not (string-blank-p current)))
;;                            (concat current " " link)
;;                          link))))
;;     (switch-to-buffer origin-buffer))) ;; switch back to the attachment buffer

;; (advice-add 'org-attach-new :before #'my/org-attach-new-add-link-to-attachments)


(defun my/org-collect-named-src-blocks (&optional file visited)
  "Return a list of (NAME BUFFER POSITION) for all named src blocks in FILE and its includes."
  (let ((visited (or visited '()))
        (results '()))
    (with-current-buffer (if file (find-file-noselect file) (current-buffer))
      (unless (member (buffer-file-name) visited)
        (push (buffer-file-name) visited)
        (org-with-point-at 1
          ;; Find named src blocks
          (let ((regexp "^[ \t]*#\\+begin_src ")
                (case-fold-search t))
            (while (re-search-forward regexp nil t)
              (let ((element (org-element-at-point)))
                (when (eq (org-element-type element) 'src-block)
                  (let ((name (org-element-property :name element)))
                    (when name
                      (push (list name (current-buffer) (org-element-property :begin element))
                            results))))))

          ;; Recursively handle includes
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+INCLUDE:[ \t]+\"\\([^\"]+\\)\"" nil t)
            (let* ((included-path (string-trim (match-string 1)))
                   (base-dir (or (and file (file-name-directory file))
                                 default-directory))
                   (abs-path (expand-file-name included-path base-dir)))
              (when (file-exists-p abs-path)
                (setq results (append results
                                      (my/org-collect-named-src-blocks abs-path visited)))))))))
    results)))

(defun my/org-extract-block-name-at-point (names)
  "Try to infer the block name at point from context, falling back to symbol at point."
  (let* ((context (org-element-context))
         (type (org-element-type context))
         (noweb-ref (and (memq type '(inline-src-block src-block))
                         (org-in-regexp (org-babel-noweb-wrap)))))
    (cond
     (noweb-ref
      (buffer-substring
       (+ (car noweb-ref) (length org-babel-noweb-wrap-start))
       (- (cdr noweb-ref) (length org-babel-noweb-wrap-end))))
     ((memq type '(babel-call inline-babel-call))
      (org-element-property :call context))
     ((car (org-element-property :results context)))
     ((let ((symbol (thing-at-point 'symbol)))
        (and symbol (member-ignore-case symbol names) symbol)))
     (t ""))))

(defun my/org-babel-goto-named-src-block ()
  "Jump to a named source block in the current buffer or any included file.
If point is on a noweb reference (<<name>>), jump to it directly."
  (interactive)
  (let* ((all-blocks (my/org-collect-named-src-blocks))
         (names (mapcar #'car all-blocks))
         (default (my/org-extract-block-name-at-point names))
         (noweb-p (and default (not (string= default "")))))
    (if noweb-p
        ;; Jump directly if on noweb or inferred symbol
        (let ((match (assoc default all-blocks)))
          (if match
              (let ((buf (nth 1 match))
                    (pos (nth 2 match)))
                (better-jumper-set-jump)
                (switch-to-buffer buf)
                ;; (org-mark-ring-push)
                (goto-char pos)
                (org-fold-show-context))
            (message "source-code block `%s` not found" default)))
      ;; Otherwise prompt
      (let* ((name (completing-read "source-block name: " names nil t))
             (match (assoc name all-blocks)))
        (if match
            (let ((buf (nth 1 match))
                  (pos (nth 2 match)))
              (better-jumper-set-jump)
              (switch-to-buffer buf)
              ;; (org-mark-ring-push)
              (goto-char pos)
              (org-fold-show-context))
          (message "source-code block `%s` not found" name))))))

;; map to g b in org mode
(map! :map org-mode-map
      :n "g b" #'my/org-babel-goto-named-src-block)

;;;;;;;;;;;;;;;;;;;;;;; special block facces

;; (defface org-example-block-face
;;   '((t (:inherit org-code)))
;;   "Face for content inside #+begin_example blocks.")
;; ;; #412F4F
;; (defun my/org-example-block-matcher (limit)
;;   "Font-lock matcher that highlights lines
;; between #+begin_example and #+end_example."
;;   (when (re-search-forward "^\\s-*#\\+begin_example\\b.*$" limit t)
;;     (let ((beg (line-beginning-position 2))) ;; start of next line
;;       (when (re-search-forward "^\\s-*#\\+end_example\\b.*$" limit t)
;;         (let ((end (line-beginning-position)))
;;           (when (< beg end)
;;             (set-match-data (list beg end))
;;             t))))))

;; (font-lock-add-keywords
;;  'org-mode
;;  '((my/org-example-block-matcher (0 'org-example-block-face prepend))))

(defface org-table-block-face
  '((t (:inherit org-block)))
  "Face for content inside #+begin_table blocks.")

(defun my/org-table-block-matcher (limit)
  "Font-lock matcher that highlights lines between #+begin_table and #+end_table."
  (when (re-search-forward "^\\s-*#\\+begin_table\\b.*$" limit t)
    (let ((beg (line-beginning-position 2))) ;; start of next line
      (when (re-search-forward "^\\s-*#\\+end_table\\b.*$" limit t)
        (let ((end (line-beginning-position)))
          (when (< beg end)
            (set-match-data (list beg end))
            t))))))

(font-lock-add-keywords
 'org-mode
 '((my/org-table-block-matcher (0 'org-table-block-face prepend))))

;;;;;;;;;;;;;;; open attachment 
(defun my/org--collect-heading-attachments ()
  "Collect attachments from the current heading. Returns a list of (display . file-path)."
  (let* ((attachments (org-entry-get (point) "ATTACHMENTS"))
         (attach-dir (ignore-errors (org-attach-dir t))))
    (when (and attachments attach-dir)
      (let (result)
        (with-temp-buffer
          (insert attachments)
          (goto-char (point-min))
          (while (re-search-forward
                  "\\[\\[\\(attachment:\\(.*?\\)\\)\\]\\[\\(.*?\\)\\]\\]"
                  nil t)
            (let ((file (match-string 2))
                  (display (match-string 3)))
              (push (cons display (expand-file-name file attach-dir)) result))))
        (nreverse result)))))

(defun my/org--collect-direct-children-attachments ()
  "Collect attachments from direct children of current heading."
  (let ((results nil)
        (parent-level (org-current-level)))
    (save-excursion
      (org-back-to-heading t)
      (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
        (forward-line 1)
        (while (and (< (point) subtree-end)
                    (re-search-forward org-heading-regexp subtree-end t))
          (when (= (org-current-level) (1+ parent-level))
            (let ((atts (my/org--collect-heading-attachments)))
              (when atts
                (setq results (append results atts))))))))
    results))

(defun my/org--collect-attachments-in-heading-and-direct-children ()
  "Collect attachments from this heading and its direct children."
  (let ((here-atts (my/org--collect-heading-attachments))
        (child-atts (my/org--collect-direct-children-attachments)))
    (append here-atts child-atts)))

(defun my/org-open-attachment-from-property-child-subtree ()
  "Open an attachment from ATTACHMENTS in this heading or its direct children."
  (interactive)
  (require 'org)
  (let* ((links (my/org--collect-attachments-in-heading-and-direct-children)))
    (if (not links)
        (user-error "No attachments found in this heading or its direct children")
      (let ((target
             (if (= (length links) 1)
                 (cdar links)
               (let ((choice (completing-read "Open attachment: " (mapcar #'car links) nil t)))
                 (cdr (assoc choice links))))))
        (when target
          (find-file target))))))

(map! :leader :desc "org open attachment from property" "m a h" #'my/org-open-attachment-from-property-child-subtree)


(defun my/jupyter-org--set-src-block-cache ()
  "Set the src-block cache.
If set successfully or if `point' is already inside the cached
source block, return non-nil. Otherwise, when `point' is not
inside a Jupyter src-block, return nil."
  (unless jupyter-org--src-block-cache
    (setq jupyter-org--src-block-cache (list 'invalid nil nil)))
 (if (org-in-src-block-p 'inside)
      (or (jupyter-org--at-cached-src-block-p)
          (when-let* ((ctx (org-element-context))
                      (el (and (eq (org-element-type ctx) 'src-block) ctx))
                      (info (and el
                                 (org-babel-jupyter-language-p
                                  (org-element-property :language el))
                                 (org-babel-get-src-block-info t el)))
                      (params (nth 2 info))
                      (beg (org-element-property :begin el))
                      (end (org-element-property :end el)))
            (setq jupyter-org--src-block-cache
                  (list params (copy-marker beg) (copy-marker end)))
            t))
    (pcase jupyter-org--src-block-cache
      ((and `(,x . ,_) (guard (not (eq x 'invalid))))
       (push 'invalid jupyter-org--src-block-cache)))
    nil))

(advice-add 'jupyter-org--set-src-block-cache
            :override #'my/jupyter-org--set-src-block-cache)

;; ;; (defun zz/org-download-clipboard-no-id (orig-fn &rest args)
;; ;;   "Run `org-download-clipboard' but skip the forced Org ID creation."
;; ;;   (cl-letf (((symbol-function 'org-id-get-create)
;; ;;              ;; Replace with a no-op during this call only
;; ;;              (lambda (&rest _ignore) nil)))
;; ;;     (apply orig-fn args)))

;; ;; (defvar org-download-clipboard-with-id t
;; ;;   "If non-nil, `org-download-clipboard' will
;; ;; create Org ID; if nil, skip ID creation.")

;; (defun zz/org-download-clipboard-no-id (orig-fn &rest args)
;;   "Run `org-download-clipboard'
;; but skip the forced Org ID creation."
;;   (if org-download-clipboard-with-id
;;       (apply orig-fn args)
;;     (cl-letf (((symbol-function 'org-id-get-create)
;;                (lambda (&rest _ignore) nil)))
;;       (apply orig-fn args))))

;; (advice-add 'org-download-clipboard :around #'zz/org-download-clipboard-no-id)

;;;###autoload
(defun my/org--resolve-attach-dir ()
  "Resolve attach directory at point, honoring :DIR: / :ATTACH_DIR: and org-attach."
  (require 'org) (require 'org-attach)
  (let* ((prop-dir (or (org-entry-get-with-inheritance "DIR")
                       (org-entry-get-with-inheritance "ATTACH_DIR")))
         (abs-dir
          (cond
           (prop-dir
            (if (file-name-absolute-p prop-dir)
                (expand-file-name prop-dir)
              (expand-file-name prop-dir
                                (or (and (buffer-file-name)
                                         (file-name-directory (buffer-file-name)))
                                    default-directory))))
           (t (or (org-attach-dir 'create)
                  (user-error "Could not resolve an attach directory"))))))
    (make-directory abs-dir 'parents)
    abs-dir))

;;;###autoload
(defun my/org-export-pdf-to-attach-dir (&optional subtreep)
  "Export current Org buffer (or SUBTREEP) to PDF into the attach dir at point.
PDF is named after the Org file. Respects :DIR:/ :ATTACH_DIR: and org-attach."
  (interactive "P")
  (require 'org) (require 'org-attach) (require 'cl-lib)

  (unless (buffer-file-name)
    (user-error "This buffer isn't visiting a file; can't determine base name"))

  (let* ((attach-dir (my/org--resolve-attach-dir))
         (base       (file-name-base (buffer-file-name)))
         ;; Targets we *want*:
         (tex-target (expand-file-name (concat base ".tex") attach-dir))
         (pdf-target (expand-file-name (concat base ".pdf") attach-dir)))

    (let ((default-directory attach-dir))
      (cl-letf (((symbol-function 'org-export-output-file-name)
                 (lambda (ext &optional _subtreep pub-dir)
                   (expand-file-name (concat base ext)
                                     (or pub-dir attach-dir)))))
        (let* ((outfile (org-latex-export-to-pdf nil subtreep))
               ;; Normalize the exporter’s return (can be relative):
               (src (when outfile
                      (if (file-name-absolute-p outfile)
                          (expand-file-name outfile)
                        (expand-file-name outfile default-directory))))
               (dst pdf-target))
          ;; Relocate only if needed and safe.
          (when (and src
                     (not (string-equal src dst))
                     (file-exists-p src))
            (condition-case err
                (progn
                  (when (file-exists-p dst) (delete-file dst))
                  (rename-file src dst t))
              (file-error
               ;; If rename fails (e.g., race or same file), fall back to copy.
               (when (file-exists-p src)
                 (copy-file src dst t t t)))))
          (message "Exported PDF → %s" dst)
          dst)))))
