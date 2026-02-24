(after! org
  (require 'org-sliced-images)
  (org-sliced-images-mode 1))

(use-package! org-sliced-images
  :after org
  :demand t
  :config
  (org-sliced-images-mode 1))

;; ;; Make org-sliced-images "fake newline" cells un-visitable by point
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


;; (with-eval-after-load 'org
;;   (defun jw/osi--delete-blank-overlays (beg end)
;;     (dolist (ov (overlays-in beg end))
;;       (when (overlay-get ov 'jw-osi-blank)
;;         (delete-overlay ov))))

;;   (defun jw/osi--has-org-image-overlays-p (beg end)
;;     (catch 'yes
;;       (dolist (ov (overlays-in beg end) nil)
;;         (when (overlay-get ov 'org-image-overlay)
;;           (throw 'yes t)))))

;;   (advice-add
;;    '+org--toggle-inline-images-in-subtree
;;    :around
;;    (lambda (orig &optional beg end refresh)
;;      ;; Compute the same region defaults as Doom’s helper.
;;      (let* ((beg (or beg
;;                      (if (org-before-first-heading-p)
;;                          (save-excursion (point-min))
;;                        (save-excursion (org-back-to-heading) (point)))))
;;             (end (or end
;;                      (if (org-before-first-heading-p)
;;                          (save-excursion (org-next-visible-heading 1) (point))
;;                        (save-excursion (org-end-of-subtree) (point)))))
;;             ;; If images already exist, this RET is toggling OFF.
;;             (toggling-off (jw/osi--has-org-image-overlays-p beg end)))
;;        ;; Always clean stale blanks first (from previous state/bugs).
;;        (jw/osi--delete-blank-overlays beg end)

;;        ;; Run Doom’s original toggler (creates or removes org-image-overlay overlays).
;;        (prog1 (funcall orig beg end refresh)
;;          ;; Only remove blanks after if we toggled OFF.
;;          ;; If we toggled ON, we WANT blanks to remain to hide the long link text.
;;          (when toggling-off
;;            (jw/osi--delete-blank-overlays beg end)))))))

;; Also fix SVG rendering: org--create-inline-image passes :scale 1 alongside
;; :max-width which can conflict for vector images.  Use :width directly.
(after! ol
  (defun +org--create-inline-image-svg-fix (orig-fn file width)
    (if (and (stringp file) (string-match-p "\\.svg\\'" file))
        (when (and (file-exists-p file) (display-graphic-p))
          (create-image file 'svg nil
                        :width (or width
                                   (when (integerp org-image-max-width)
                                     org-image-max-width))))
      (funcall orig-fn file width)))
  (advice-add 'org--create-inline-image :around #'+org--create-inline-image-svg-fix))
