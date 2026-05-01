;;; layout-buffer-consult.el -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-lib)

(define-derived-mode my/layout-buffer-mode fundamental-mode "LayoutBuf"
  "Major mode for virtual buffers that store window layouts.")

(defvar-local my/layout-buffer-p nil
  "Non-nil when this buffer stores a window layout.")

(defvar-local my/layout-buffer-state nil
  "Saved `window-state-get' form for this layout buffer.")

(defvar-local my/layout-buffer-members nil
  "Live buffers currently represented by this layout buffer.")

(defvar-local my/layout-buffer-return-state nil
  "Window state to restore when quitting this temporary buffer.")

(defvar-local my/layout-buffer-return-active nil
  "Layout buffer to reactivate when quitting this temporary buffer.")

(defvar my/layout-buffer-active-frame-param 'my/layout-buffer-active
  "Frame parameter used to track the active layout buffer.")

(defface my/layout-buffer-name-face
  '((t (:inherit font-lock-function-name-face :weight semibold)))
  "Face used for layout buffer names in completion UIs.")

(defvar my/layout-buffer--consult-preview-context nil
  "Internal context for consult preview while in a layout buffer.")

(defvar my/layout-buffer--consult-pane-switch nil
  "Non-nil means current `consult-buffer' switches only the selected pane.")

(defvar my/layout-buffer--consult-hide-layout-members nil
  "Non-nil means `consult-buffer' hides buffers captured by layout buffers.")

(defvar my/layout-buffer--consult-demote-layout nil
  "Layout buffer to demote in current `consult-buffer' invocation.")

(defvar my/layout-buffer--in-global-switch nil
  "Guard to avoid recursive layout switch advices.")

(defun my/layout-buffer--buffer-p (buffer)
  (and (buffer-live-p buffer)
       (buffer-local-value 'my/layout-buffer-p buffer)))

(defun my/layout-buffer--candidate-buffer (cand)
  (cond
   ((bufferp cand) cand)
   ((stringp cand) (get-buffer (substring-no-properties cand)))
   (t nil)))

(defun my/layout-buffer--owner-for-buffer (buffer)
  (let ((active (my/layout-buffer--active)))
    (or (and (my/layout-buffer--buffer-p active)
             (memq buffer (buffer-local-value 'my/layout-buffer-members active))
             active)
        (catch 'owner
          (dolist (buf (buffer-list))
            (when (and (my/layout-buffer--buffer-p buf)
                       (memq buffer (buffer-local-value 'my/layout-buffer-members buf)))
              (throw 'owner buf)))))))

(defun my/layout-buffer--focus-member-window (buffer)
  (when-let ((win (get-buffer-window buffer nil)))
    (select-window win)))

(defun my/layout-buffer--promote-in-buffer-list (buffer)
  (let ((bl (frame-parameter nil 'buffer-list))
        (bbl (frame-parameter nil 'buried-buffer-list)))
    (set-frame-parameter nil 'buffer-list (cons buffer (delq buffer bl)))
    (set-frame-parameter nil 'buried-buffer-list (delq buffer bbl))))

(defun my/layout-buffer--prepare-target (target)
  (let* ((buffer (my/layout-buffer--candidate-buffer target))
         (owner (and buffer
                     (not (my/layout-buffer--buffer-p buffer))
                     (my/layout-buffer--owner-for-buffer buffer))))
    (cond
     ((and buffer (my/layout-buffer--buffer-p buffer))
      (my/layout-buffer--maybe-save-active-state buffer)
      (my/layout-buffer--restore buffer t)
      :layout)
     (owner
      (my/layout-buffer--maybe-save-active-state owner)
      (my/layout-buffer--restore owner t)
      (my/layout-buffer--focus-member-window buffer)
      :layout)
     ((my/layout-buffer--buffer-p (my/layout-buffer--active))
      (my/layout-buffer--maybe-save-active-state)
      (my/layout-buffer--leave-active)
      :left-layout)
     (t nil))))

(defun my/layout-buffer--active ()
  (frame-parameter nil my/layout-buffer-active-frame-param))

(defun my/layout-buffer--set-active (buffer)
  (set-frame-parameter nil my/layout-buffer-active-frame-param buffer))

(defun my/layout-buffer--default-name ()
  (string-join
   (delete-dups
    (mapcar (lambda (win) (buffer-name (window-buffer win)))
            (window-list nil 'nomini)))
   " | "))

(defun my/layout-buffer--capture-state (buffer)
  (with-current-buffer buffer
    (setq-local my/layout-buffer-state
                (window-state-get (frame-root-window) t))
    (setq-local my/layout-buffer-members
                (cl-remove-if
                 #'my/layout-buffer--buffer-p
                 (delete-dups
                  (mapcar #'window-buffer (window-list nil 'nomini)))))))

(defun my/layout-buffer--hidden-members ()
  (let (members)
    (dolist (buf (buffer-list))
      (when (my/layout-buffer--buffer-p buf)
        (dolist (member (buffer-local-value 'my/layout-buffer-members buf))
          (when (and (buffer-live-p member)
                     (not (my/layout-buffer--buffer-p member)))
             (push member members)))))
    (delete-dups members)))

(defun my/layout-buffer--hidden-members-for-pane-switch ()
  (let ((active (my/layout-buffer--active))
        members)
    (dolist (buf (buffer-list))
      (when (and (my/layout-buffer--buffer-p buf)
                 (not (eq buf active)))
        (dolist (member (buffer-local-value 'my/layout-buffer-members buf))
          (when (and (buffer-live-p member)
                     (not (my/layout-buffer--buffer-p member)))
            (push member members)))))
    (delete-dups members)))

(defun my/layout-buffer--in-active-member-window-p (&optional window)
  (let* ((active (my/layout-buffer--active))
         (buffer (window-buffer (or window (selected-window)))))
    (and (my/layout-buffer--buffer-p active)
         (memq buffer (buffer-local-value 'my/layout-buffer-members active)))))

(defun my/layout-buffer--fallback-buffer (layout members)
  (or
   (catch 'candidate
     (dolist (buf (buffer-list))
       (when (and (buffer-live-p buf)
                  (not (eq buf layout))
                  (not (memq buf members))
                  (not (string-prefix-p " " (buffer-name buf))))
         (throw 'candidate buf))))
   (catch 'candidate
     (dolist (buf (buffer-list))
       (when (and (buffer-live-p buf)
                  (not (eq buf layout))
                  (not (memq buf members)))
         (throw 'candidate buf))))
   (get-buffer-create "*scratch*")))

(defun my/layout-buffer--save-active-state ()
  (let ((active (my/layout-buffer--active)))
    (when (my/layout-buffer--buffer-p active)
      (my/layout-buffer--capture-state active))))

(defun my/layout-buffer--maybe-save-active-state (&optional target)
  (let ((active (my/layout-buffer--active)))
    (when (and (my/layout-buffer--buffer-p active)
               (not (eq active target))
               (not (one-window-p t)))
      (my/layout-buffer--capture-state active))))

(defun my/layout-buffer--leave-active ()
  (when (my/layout-buffer--buffer-p (my/layout-buffer--active))
    (my/layout-buffer--set-active nil)
    (delete-other-windows)))

(defun my/layout-buffer--restore (buffer &optional _silent)
  (unless (my/layout-buffer--buffer-p buffer)
    (user-error "Not a layout buffer: %s" (buffer-name buffer)))
  (let ((state (buffer-local-value 'my/layout-buffer-state buffer)))
    (unless state
      (user-error "Layout buffer has no saved window state"))
    (window-state-put state (frame-root-window) 'safe)
    (my/layout-buffer--set-active buffer)
    (my/layout-buffer--promote-in-buffer-list buffer)))

(defun my/define-current-window-as-buffer (name)
  "Define current window layout as a virtual buffer named NAME."
  (interactive
   (list (read-string "Layout buffer name: " (my/layout-buffer--default-name))))
  (let* ((name (if (string-empty-p name)
                   (my/layout-buffer--default-name)
                 name))
         (existing (get-buffer name))
         (buffer (cond
                  ((null existing) (get-buffer-create name))
                  ((my/layout-buffer--buffer-p existing) existing)
                  (t (user-error "Buffer %s already exists and is not a layout buffer" name)))))
    (with-current-buffer buffer
      (my/layout-buffer-mode)
      (setq-local my/layout-buffer-p t)
      (setq-local doom-real-buffer-p t)
      (my/layout-buffer--capture-state buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Layout buffer: %s\n" (buffer-name buffer)))))
    (my/layout-buffer--set-active buffer)
    (my/layout-buffer--promote-in-buffer-list buffer)
    (message "Defined layout buffer: %s" (buffer-name buffer))))

(defun my/disassemble-current-window-buffer ()
  "Disassociate and remove the active virtual layout buffer."
  (interactive)
  (let ((active (my/layout-buffer--active)))
    (unless (my/layout-buffer--buffer-p active)
      (user-error "No active layout buffer in this frame"))
    (my/layout-buffer--set-active nil)
    (kill-buffer active)
    (message "Disassembled layout buffer")))

(defun my/layout-buffer--disassemble-and-kill (layout)
  "Disable LAYOUT mode and kill LAYOUT with all its member buffers."
  (when (and (buffer-live-p layout)
             (my/layout-buffer--buffer-p layout))
    (when (eq layout (my/layout-buffer--active))
      (my/layout-buffer--capture-state layout))
    (let* ((members (delete-dups
                     (cl-remove-if-not
                      #'buffer-live-p
                      (copy-sequence
                       (buffer-local-value 'my/layout-buffer-members layout)))))
           (current (current-buffer))
           (fallback (my/layout-buffer--fallback-buffer layout members)))
      (my/layout-buffer--set-active nil)
      (delete-other-windows)
      (when (or (eq current layout)
                (memq current members))
        (switch-to-buffer fallback 'norecord))
      (dolist (buf members)
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (when (buffer-live-p layout)
        (kill-buffer layout)))))

(defun my/layout-switch-buffer ()
  "Switch buffers, restoring layout buffers as full window states."
  (interactive)
  (my/layout-switch-to-buffer (read-buffer "Switch to buffer: " nil t)))

(defun my/layout-switch-current-window-to-buffer (target &optional norecord)
  "Switch the selected layout pane to TARGET without leaving layout mode."
  (interactive (list (read-buffer "Switch pane to buffer: " nil t)))
  (let ((active (my/layout-buffer--active))
        (buffer (my/layout-buffer--candidate-buffer target)))
    (unless (my/layout-buffer--in-active-member-window-p)
      (user-error "Not in an active layout window"))
    (unless buffer
      (user-error "No such buffer: %s" target))
    (when (my/layout-buffer--buffer-p buffer)
      (user-error "Use layout switching to restore layout buffer: %s"
                  (buffer-name buffer)))
    (let ((my/layout-buffer--in-global-switch t))
      (switch-to-buffer buffer norecord))
    (when (my/layout-buffer--buffer-p active)
      (my/layout-buffer--capture-state active)
      (my/layout-buffer--set-active active)
      (my/layout-buffer--promote-in-buffer-list active))))

(defun my/layout-consult-buffer ()
  "Run `consult-buffer', switching only the selected pane inside layouts."
  (interactive)
  (if (my/layout-buffer--in-active-member-window-p)
      (let ((my/layout-buffer--consult-pane-switch t))
        (consult-buffer))
    (consult-buffer)))

(defun my/layout-switch-to-buffer (target &optional norecord)
  "Switch to TARGET, restoring layout buffers as full window states.
TARGET may be a buffer object or a buffer name."
  (let* ((buffer (if (bufferp target) target (get-buffer target)))
         (owner (and buffer
                     (not (my/layout-buffer--buffer-p buffer))
                     (my/layout-buffer--owner-for-buffer buffer))))
    (unless buffer
      (user-error "No such buffer: %s" target))
    (my/layout-buffer--maybe-save-active-state (or owner buffer))
    (cond
     (owner
      (my/layout-buffer--restore owner)
      (my/layout-buffer--focus-member-window buffer))
     ((my/layout-buffer--buffer-p buffer)
        (my/layout-buffer--restore buffer)
      )
     (t
      (my/layout-buffer--leave-active)
      (switch-to-buffer buffer norecord)))))

(defun my/layout-buffer-switch-to-buffer-a (orig-fn buffer-or-name &rest args)
  "Make `switch-to-buffer' leave layout mode before normal switches."
  (if (or my/layout-buffer--in-global-switch
          my/layout-buffer--consult-pane-switch)
      (apply orig-fn buffer-or-name args)
    (let ((my/layout-buffer--in-global-switch t))
      (if (eq (my/layout-buffer--prepare-target buffer-or-name) :layout)
          (my/layout-buffer--candidate-buffer buffer-or-name)
        (apply orig-fn buffer-or-name args)))))

(defun my/layout-buffer-pop-to-buffer-a (orig-fn buffer-or-name &rest args)
  "Make `pop-to-buffer' switch whole frame while in layout mode."
  (if (or my/layout-buffer--in-global-switch
          my/layout-buffer--consult-pane-switch)
      (apply orig-fn buffer-or-name args)
    (let ((my/layout-buffer--in-global-switch t)
          (result (my/layout-buffer--prepare-target buffer-or-name)))
      (cond
       ((eq result :layout)
        (my/layout-buffer--candidate-buffer buffer-or-name))
       ((eq result :left-layout)
        (switch-to-buffer buffer-or-name (nth 1 args)))
       (t (apply orig-fn buffer-or-name args))))))

(defun my/layout-buffer-kill-buffer-a (orig-fn &optional buffer-or-name)
  "Kill BUFFER-OR-NAME without collapsing active layout state."
  (let* ((target (or (my/layout-buffer--candidate-buffer buffer-or-name)
                     (current-buffer)))
         (active (my/layout-buffer--active))
         (active-layout (my/layout-buffer--buffer-p active)))
    (if (and (my/layout-buffer--buffer-p target)
             (eq target active))
        (progn
          (my/layout-buffer--set-active nil)
          (funcall orig-fn target))
      (let ((my/layout-buffer--in-global-switch t)
            (result (funcall orig-fn target)))
        (when (and active-layout
                   (buffer-live-p active)
                   (not (eq target active))
                   (not (buffer-live-p target)))
          (with-current-buffer active
            (setq-local my/layout-buffer-members
                        (delq target my/layout-buffer-members)))
          (when (not (one-window-p t))
            (my/layout-buffer--capture-state active)))
        result))))

(defun my/layout-buffer-kill-current-buffer-a (orig-fn &rest args)
  "Kill the entire active layout when killing one of its buffers."
  (let* ((active (my/layout-buffer--active))
         (current (current-buffer)))
    (if (and (my/layout-buffer--buffer-p active)
             (or (eq current active)
                 (memq current (buffer-local-value 'my/layout-buffer-members active))))
        (my/layout-buffer--disassemble-and-kill active)
      (apply orig-fn args))))

(defun my/layout-buffer-dired-jump-a (orig-fn &rest args)
  "Remember layout context when jumping to Dired from a layout."
  (let* ((active (my/layout-buffer--active))
         (active-layout (my/layout-buffer--buffer-p active))
         (state (when active-layout
                  (my/layout-buffer--maybe-save-active-state)
                  (buffer-local-value 'my/layout-buffer-state active))))
    (let* ((result (apply orig-fn args))
           (buf (cond
                 ((derived-mode-p 'dired-mode) (current-buffer))
                 ((bufferp result) result)
                 (t nil))))
      (when (and active-layout state (buffer-live-p buf))
        (with-current-buffer buf
          (when (derived-mode-p 'dired-mode)
            (setq-local my/layout-buffer-return-state state)
            (setq-local my/layout-buffer-return-active active)
            (local-set-key (kbd "q") #'my/layout-buffer-dired-quit)
            (when (fboundp 'evil-local-set-key)
              (evil-local-set-key 'normal (kbd "q") #'my/layout-buffer-dired-quit)))))
      result)))

(defun my/layout-buffer-dired-quit ()
  "Quit Dired and restore the originating layout if available."
  (interactive)
  (if (not my/layout-buffer-return-state)
      (quit-window)
    (let ((state my/layout-buffer-return-state)
          (active my/layout-buffer-return-active)
          (buf (current-buffer)))
      (bury-buffer buf)
      (window-state-put state (frame-root-window) 'safe)
      (if (my/layout-buffer--buffer-p active)
          (my/layout-buffer--set-active active)
        (my/layout-buffer--set-active nil)))))

(defun my/layout-buffer-quit-window-restore-a (orig-fn &optional kill window)
  "Restore captured layout when quitting a tagged temporary buffer."
  (let* ((win (or window (selected-window)))
         (buf (and (window-live-p win) (window-buffer win)))
         (state (and (buffer-live-p buf)
                     (buffer-local-value 'my/layout-buffer-return-state buf)))
         (active (and (buffer-live-p buf)
                      (buffer-local-value 'my/layout-buffer-return-active buf))))
    (if (not state)
        (funcall orig-fn kill window)
      (if kill
          (kill-buffer buf)
        (bury-buffer buf))
      (window-state-put state (frame-root-window) 'safe)
      (if (my/layout-buffer--buffer-p active)
          (my/layout-buffer--set-active active)
        (my/layout-buffer--set-active nil)))))

(unless (advice-member-p #'my/layout-buffer-switch-to-buffer-a 'switch-to-buffer)
  (advice-add 'switch-to-buffer :around #'my/layout-buffer-switch-to-buffer-a))

(unless (advice-member-p #'my/layout-buffer-pop-to-buffer-a 'pop-to-buffer)
  (advice-add 'pop-to-buffer :around #'my/layout-buffer-pop-to-buffer-a))

(unless (advice-member-p #'my/layout-buffer-kill-buffer-a 'kill-buffer)
  (advice-add 'kill-buffer :around #'my/layout-buffer-kill-buffer-a))

(unless (advice-member-p #'my/layout-buffer-kill-current-buffer-a 'kill-current-buffer)
  (advice-add 'kill-current-buffer :around #'my/layout-buffer-kill-current-buffer-a))

(unless (advice-member-p #'my/layout-buffer-dired-jump-a 'dired-jump)
  (advice-add 'dired-jump :around #'my/layout-buffer-dired-jump-a))

(unless (advice-member-p #'my/layout-buffer-dired-jump-a 'dired-jump-other-window)
  (advice-add 'dired-jump-other-window :around #'my/layout-buffer-dired-jump-a))

(unless (advice-member-p #'my/layout-buffer-quit-window-restore-a 'quit-window)
  (advice-add 'quit-window :around #'my/layout-buffer-quit-window-restore-a))

(map! "C-x b" #'my/layout-consult-buffer
      :leader
      :desc "Define current windows as layout buffer" "w B" #'my/define-current-window-as-buffer
      :desc "Disassemble active layout buffer" "w D" #'my/disassemble-current-window-buffer)

(after! consult
  (defun my/consult-buffer-hide-layout-members-a (orig-fn &rest args)
    "Hide layout-member buffers only during `consult-buffer'."
    (let* ((active (my/layout-buffer--active))
           (my/layout-buffer--consult-hide-layout-members
            (not my/layout-buffer--consult-pane-switch))
           (my/layout-buffer--consult-demote-layout
            (and (not my/layout-buffer--consult-pane-switch)
                 (my/layout-buffer--buffer-p active)
                 active))
           (consult-buffer-list-function #'consult--frame-buffer-list))
      (apply orig-fn args)))

  (defun my/consult-buffer-query-hide-layout-members-a (orig-fn &rest args)
    "Filter out buffers represented by layout buffers in consult lists."
    (if (and (not my/layout-buffer--consult-hide-layout-members)
             (not my/layout-buffer--consult-pane-switch))
        (apply orig-fn args)
      (let* ((hidden (if my/layout-buffer--consult-pane-switch
                         (my/layout-buffer--hidden-members-for-pane-switch)
                       (my/layout-buffer--hidden-members)))
             (old-predicate (plist-get args :predicate))
             (predicate (lambda (buf)
                          (and (not (and my/layout-buffer--consult-pane-switch
                                         (my/layout-buffer--buffer-p buf)))
                               (not (memq buf hidden))
                                (if old-predicate (funcall old-predicate buf) t))))
             (args (plist-put args :predicate predicate)))
        (apply orig-fn args))))

  (defun my/consult-buffer-sort-visibility-layout-a (orig-fn buffers)
    "Demote the active layout buffer in visibility sorting."
    (let* ((sorted (funcall orig-fn buffers))
           (layout my/layout-buffer--consult-demote-layout))
      (if (and (buffer-live-p layout)
               (memq layout sorted))
          (append (delq layout sorted) (list layout))
        sorted)))

  (defun my/consult-buffer-preview-layout-a (orig-fn)
    "Preview regular buffers in full frame when a layout buffer is active."
    (let ((state (funcall orig-fn))
          (pane-switch my/layout-buffer--consult-pane-switch))
      (lambda (action cand)
        (if pane-switch
            (funcall state action cand)
          (pcase action
            ('setup
             (let ((active (my/layout-buffer--active)))
               (when (my/layout-buffer--buffer-p active)
                 (my/layout-buffer--maybe-save-active-state))
               (setq my/layout-buffer--consult-preview-context
                     (list :active active
                           :from-layout (my/layout-buffer--buffer-p active)
                           :custom nil
                           :window-state (window-state-get (frame-root-window) t)
                           :modified nil
                           :returned nil)))
             (funcall state action cand))
            ('preview
             (let* ((ctx my/layout-buffer--consult-preview-context)
                    (buffer (my/layout-buffer--candidate-buffer cand))
                    (layout-cand (my/layout-buffer--buffer-p buffer)))
               (if (not ctx)
                    (funcall state action cand)
                 (unless (plist-get ctx :custom)
                   (when (or (plist-get ctx :from-layout) layout-cand)
                     (setq ctx (plist-put ctx :custom t))
                     (setq my/layout-buffer--consult-preview-context ctx)))
                 (if (not (plist-get ctx :custom))
                     (funcall state action cand)
                   (if layout-cand
                       (progn
                         (my/layout-buffer--restore buffer t)
                         (setq my/layout-buffer--consult-preview-context
                               (plist-put ctx :modified t)))
                     (unless (one-window-p t)
                       (delete-other-windows))
                     (setq my/layout-buffer--consult-preview-context
                           (plist-put ctx :modified t))
                     (funcall state action cand))))))
            ('return
             (when my/layout-buffer--consult-preview-context
               (setq my/layout-buffer--consult-preview-context
                     (plist-put my/layout-buffer--consult-preview-context :returned t)))
             (funcall state action cand))
            ('exit
             (when-let* ((ctx my/layout-buffer--consult-preview-context)
                         ((plist-get ctx :modified))
                         ((not (plist-get ctx :returned)))
                         (window-state (plist-get ctx :window-state)))
               (window-state-put window-state (frame-root-window) 'safe)
               (my/layout-buffer--set-active (plist-get ctx :active)))
             (setq my/layout-buffer--consult-preview-context nil)
             (funcall state action cand))
            (_ (funcall state action cand)))))))

  (defun my/consult-buffer-pair-layout-face-a (orig-fn buffer)
    "Propertize layout buffer names in consult buffer candidates."
    (let ((pair (funcall orig-fn buffer)))
      (when (my/layout-buffer--buffer-p buffer)
        (let ((name (copy-sequence (car pair))))
          (add-face-text-property 0 (length name) 'my/layout-buffer-name-face 'append name)
          (setcar pair name)))
      pair))

  (defun my/consult-buffer-action-layout-a (orig-fn buffer &optional norecord)
    "Restore layout buffers when selected from consult."
    (if my/layout-buffer--consult-pane-switch
        (if (my/layout-buffer--buffer-p buffer)
            (my/layout-switch-to-buffer buffer norecord)
          (my/layout-switch-current-window-to-buffer buffer norecord))
      (if (my/layout-buffer--buffer-p buffer)
        (progn
          (when-let* ((ctx my/layout-buffer--consult-preview-context)
                       ((plist-get ctx :modified))
                       (state (plist-get ctx :window-state)))
            (window-state-put state (frame-root-window) 'safe)
            (my/layout-buffer--set-active (plist-get ctx :active)))
          (my/layout-switch-to-buffer buffer norecord))
        (my/layout-buffer--leave-active)
        (funcall orig-fn buffer norecord))))

  (unless (advice-member-p #'my/consult-buffer-preview-layout-a 'consult--buffer-preview)
    (advice-add 'consult--buffer-preview :around #'my/consult-buffer-preview-layout-a))

  (unless (advice-member-p #'my/consult-buffer-pair-layout-face-a 'consult--buffer-pair)
    (advice-add 'consult--buffer-pair :around #'my/consult-buffer-pair-layout-face-a))

  (unless (advice-member-p #'my/consult-buffer-action-layout-a 'consult--buffer-action)
    (advice-add 'consult--buffer-action :around #'my/consult-buffer-action-layout-a))

  (unless (advice-member-p #'my/consult-buffer-hide-layout-members-a 'consult-buffer)
    (advice-add 'consult-buffer :around #'my/consult-buffer-hide-layout-members-a))

  (unless (advice-member-p #'my/consult-buffer-query-hide-layout-members-a 'consult--buffer-query)
    (advice-add 'consult--buffer-query :around #'my/consult-buffer-query-hide-layout-members-a))

  (unless (advice-member-p #'my/consult-buffer-sort-visibility-layout-a 'consult--buffer-sort-visibility)
    (advice-add 'consult--buffer-sort-visibility :around #'my/consult-buffer-sort-visibility-layout-a)))
