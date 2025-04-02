;;; lisp/my-quickrun.el -*- lexical-binding: t; -*-

(setq quickrun-bookmark-file (expand-file-name "quickrun-bookmarks" doom-user-dir))

(defun add-to-quickrun-bookmarks ()
  (interactive)
  (let ((bookmark-default-file quickrun-bookmark-file))
    (call-interactively 'bookmark-set)
    (bookmark-save)))

(defun run-bookmarked-file-with-quickrun ()
  (interactive)
  (let ((bookmark-default-file quickrun-bookmark-file))
    (bookmark-load bookmark-default-file t)
    (let ((bookmark (bookmark-completing-read "Choose a quickrun bookmark" bookmark-alist)))
      (let ((filename (bookmark-get-filename bookmark)))
        (if (file-exists-p filename)
            (progn
              (find-file filename)
              (quickrun)
              (bookmark-save))
          (message "Bookmark points to a non-existent file!"))))))

(defun delete-quickrun-bookmark ()
  (interactive)
  (let ((bookmark-default-file quickrun-bookmark-file))
    (bookmark-load bookmark-default-file t)
    (let ((bookmark (bookmark-completing-read "Choose a quickrun bookmark to delete" bookmark-alist)))
      (bookmark-delete bookmark)
      (bookmark-save)
      (message "Deleted bookmark: %s" bookmark))))

(map! :leader
       :desc "Add to Quickrun Bookmarks" "r a" #'add-to-quickrun-bookmarks
       :desc "Run Quickrun Bookmark" "r b" #'run-bookmarked-file-with-quickrun
       :desc "Delete Quickrun Bookmark" "r d" #'delete-quickrun-bookmark)
