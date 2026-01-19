(after! org
  (require 'org-sliced-images)
  (org-sliced-images-mode 1)

  (defun jiawei/org-sliced-images-fix-startup ()
    (when (and org-startup-with-inline-images
               (bound-and-true-p org-sliced-images-mode))
      ;; Org 9.8+ may render startup images via org-link-preview, bypassing org-sliced-images advice.
      (when (fboundp 'org-link-preview)
        (ignore-errors (org-link-preview '(64)))) ; hide previews in accessible buffer
      (ignore-errors (org-display-inline-images)))) ; redraw through advised path

  (add-hook 'org-mode-hook #'jiawei/org-sliced-images-fix-startup 90))

(use-package! org-sliced-images
  :after org
  :demand t
  :config
  (org-sliced-images-mode 1))

;; Make org-sliced-images "fake newline" cells un-visitable by point
(after! org-sliced-images

  ;; Mark the fake-newline overlay as cursor-intangible
  (advice-add
   'org-sliced-images--make-inline-image-overlay
   :around
   (lambda (orig start end spec)
     (let ((ov (funcall orig start end spec)))
       (when (equal spec "\n")
         ;; This overlay is the dummy cell used to simulate slice line breaks
         (overlay-put ov 'cursor-intangible t))
       ov)))

  ;; Enable cursor-intangible-mode when sliced images are displayed
  (advice-add
   'org-sliced-images-display-inline-images
   :after
   (lambda (&rest _)
     (cursor-intangible-mode 1)))

  ;; Optional: disable mode when images are removed
  (advice-add
   'org-sliced-images-remove-inline-images
   :after
   (lambda (&rest _)
     (unless (seq-some (lambda (ov)
                         (overlay-get ov 'org-image-overlay))
                       (overlays-in (point-min) (point-max)))
       (cursor-intangible-mode -1)))))


(with-eval-after-load 'org
  (defun jw/osi--delete-blank-overlays (beg end)
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'jw-osi-blank)
        (delete-overlay ov))))

  (defun jw/osi--has-org-image-overlays-p (beg end)
    (catch 'yes
      (dolist (ov (overlays-in beg end) nil)
        (when (overlay-get ov 'org-image-overlay)
          (throw 'yes t)))))

  (advice-add
   '+org--toggle-inline-images-in-subtree
   :around
   (lambda (orig &optional beg end refresh)
     ;; Compute the same region defaults as Doom’s helper.
     (let* ((beg (or beg
                     (if (org-before-first-heading-p)
                         (save-excursion (point-min))
                       (save-excursion (org-back-to-heading) (point)))))
            (end (or end
                     (if (org-before-first-heading-p)
                         (save-excursion (org-next-visible-heading 1) (point))
                       (save-excursion (org-end-of-subtree) (point)))))
            ;; If images already exist, this RET is toggling OFF.
            (toggling-off (jw/osi--has-org-image-overlays-p beg end)))
       ;; Always clean stale blanks first (from previous state/bugs).
       (jw/osi--delete-blank-overlays beg end)

       ;; Run Doom’s original toggler (creates or removes org-image-overlay overlays).
       (prog1 (funcall orig beg end refresh)
         ;; Only remove blanks after if we toggled OFF.
         ;; If we toggled ON, we WANT blanks to remain to hide the long link text.
         (when toggling-off
           (jw/osi--delete-blank-overlays beg end)))))))
