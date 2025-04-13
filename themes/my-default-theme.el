;;; my-default-theme.el --- Default Emacs colors with custom dired faces -*- lexical-binding: t; no-byte-compile: t; -*-
(deftheme my-default
  "A theme based on the default Emacs theme, with custom dired faces.")

;;; commentary:
;;; This theme is a simple customization of the default Emacs theme,

(let ((class '((class color) (min-colors 89)))
      ;; Define fallback colors (customize these if needed)
      (c '((class color) (min-colors 89)))
      (fg-main "#000000")
      (bg-inactive "#f0f0f0")
      (accent-0 "#0066cc")
      (accent-1 "#228822")
      (accent-2 "#aa33aa")
      (accent-3 "#cc6600")
      (err "#cc0000")
      (warning "#cc8800")
      (docstring "#444444")
      (fg-link "#0000ee")
      (bg-link-symbolic "#e0f0ff")
      (fg-link-symbolic "#0044cc")
      (underline-link-symbolic t)
      (variable "#333399")
      (date-common "#777777"))

  (custom-theme-set-faces
   'my-default

   ;; Dired faces
   `(dired-broken-symlink ((,c :inherit button :foreground ,err)))
   `(dired-directory ((,c :foreground ,accent-0)))
   `(dired-flagged ((,c :foreground ,err :weight bold)))
   `(dired-header ((,c :weight bold)))
   `(dired-ignored ((,c :inherit shadow)))
   `(dired-mark ((,c :weight bold)))
   `(dired-marked ((,c :background ,bg-inactive :weight bold)))
   `(dired-perm-write ((,c :inherit shadow)))
   `(dired-symlink ((,c :inherit button :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
   `(dired-warning ((,c :inherit warning)))

   ;; Dired async
   `(dired-async-failures ((,c :inherit error)))
   `(dired-async-message ((,c :weight bold)))
   `(dired-async-mode-message ((,c :weight bold)))

   ;; dired-git
   `(dired-git-branch-else ((,c :weight bold :foreground ,accent-0)))
   `(dired-git-branch-master ((,c :weight bold :foreground ,accent-1)))

   ;; dired-git-info
   `(dgi-commit-message-face ((,c :foreground ,docstring)))

   ;; dired-narrow
   `(dired-narrow-blink ((,c :weight bold :foreground ,warning)))

   ;; dired-subtree (clear background to avoid conflicts)
   `(dired-subtree-depth-1-face (()))
   `(dired-subtree-depth-2-face (()))
   `(dired-subtree-depth-3-face (()))
   `(dired-subtree-depth-4-face (()))
   `(dired-subtree-depth-5-face (()))
   `(dired-subtree-depth-6-face (()))

   ;; diredfl
   `(diredfl-autofile-name ((,c :background ,bg-inactive)))
   `(diredfl-compressed-file-name ((,c :foreground ,warning)))
   `(diredfl-compressed-file-suffix ((,c :foreground ,err)))
   `(diredfl-date-time ((,c :foreground ,date-common)))
   `(diredfl-deletion ((,c :foreground ,err :weight bold)))
   `(diredfl-deletion-file-name ((,c :inherit diredfl-deletion)))
   `(diredfl-dir-heading ((,c :weight bold)))
   `(diredfl-dir-name ((,c :foreground ,accent-0)))
   `(diredfl-dir-priv ((,c :foreground ,accent-0)))
   `(diredfl-exec-priv ((,c :foreground ,accent-1)))
   `(diredfl-executable-tag ((,c :inherit diredfl-exec-priv)))
   `(diredfl-file-name ((,c :foreground ,fg-main)))
   `(diredfl-file-suffix ((,c :foreground ,variable)))
   `(diredfl-flag-mark ((,c :background ,bg-inactive :weight bold)))
   `(diredfl-flag-mark-line ((,c :inherit diredfl-flag-mark)))
   `(diredfl-ignored-file-name ((,c :inherit shadow)))
   `(diredfl-link-priv ((,c :foreground ,fg-link)))
   `(diredfl-no-priv ((,c :inherit shadow)))
   `(diredfl-number ((,c :inherit shadow)))
   `(diredfl-other-priv ((,c :foreground ,accent-2)))
   `(diredfl-rare-priv ((,c :foreground ,accent-3)))
   `(diredfl-read-priv ((,c :foreground ,fg-main)))
   `(diredfl-symlink ((,c :inherit dired-symlink)))
   `(diredfl-tagged-autofile-name ((,c :background ,bg-inactive :weight bold)))
   `(diredfl-write-priv ((,c :foreground ,accent-0)))))

(provide-theme 'my-default)
