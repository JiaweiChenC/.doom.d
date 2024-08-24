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
            (t
             (scroll-up-command))))))  ; For all other modes, perform a normal scroll up

(defun send-scroll-down-to-other-frame ()
  (interactive)
  (let ((other-frame (next-frame)))  ; Get the next frame
    (with-selected-frame other-frame  ; Work within the context of the other frame
      (cond ((derived-mode-p 'pdf-view-mode)  ; Check if the frame is in pdf-view-mode
             (pdf-view-previous-line-or-previous-page 5))  ; If true, go to the next page in the PDF
            ((derived-mode-p 'image-mode)  ; Check if the frame is in image-mode
             (image-previous-file 1))  ; If true, go to the next image file
            (t
             (scroll-down-command))))))  ; For all other modes, perform a normal scroll up

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
(map! :n "C-;" #'send-scroll-up-to-other-window)
(map! :n "C-'" #'send-scroll-down-to-other-window)
