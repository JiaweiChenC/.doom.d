;;; lisp/custom-functions.el -*- lexical-binding: t; -*-


(defun my/copy-image-to-clipboard ()
  "Copy the image at point or current image buffer to the clipboard in macOS.
Handles Org mode, Dired mode, and image buffers."
  (interactive)
  (cond
   ;; In Org mode, try to get the path and copy file
   ((derived-mode-p 'org-mode)
    (if-let ((image-path (org-element-property :path (org-element-context))))
        (let ((full-path (expand-file-name image-path)))
          (if (file-exists-p full-path)
              (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                     full-path
                                     "\") as JPEG picture)'"))
            (message "File does not exist: %s" full-path)))
      (message "No image file at point!")))
   ;; In Dired mode, copy the file at point
   ((derived-mode-p 'dired-mode)
    (let ((file-path (dired-get-file-for-visit)))
      (if (file-exists-p file-path)
          (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                 file-path
                                 "\") as JPEG picture)'"))
        (message "Selected file does not exist!"))))
   ;; In image mode, copy the image data directly
   ((eq major-mode 'image-mode)
    (let ((image-file (buffer-file-name)))
      (if image-file
          (shell-command (concat "osascript -e 'set the clipboard to (read (POSIX file \""
                                 image-file
                                 "\") as JPEG picture)'"))
        (message "No file associated with this buffer!"))))
   ;; Default message when not in applicable mode
   (t (message "Not in an Org, Dired, or Image buffer!"))))

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
(map! :n "C-;" #'send-scroll-up-to-other-frame)
(map! :n "C-'" #'send-scroll-down-to-other-frame)

(defun open-project-file-externally ()
  "Find a file in the current project and open it externally on macOS."
  (interactive)
  (require 'projectile)
  (projectile-ensure-project (projectile-project-root))
  (let ((file (projectile-completing-read "Find file: " (projectile-current-project-files))))
    (shell-command (concat "open " (shell-quote-argument (expand-file-name file (projectile-project-root)))))))

(map! :leader
      :desc "Open project file externally"
      "p E" #'open-project-file-externally)



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
