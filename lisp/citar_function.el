(require 'json)
  (require 'url)

  (defcustom zot-max-retries 3
    "Maximum number of retries for connecting to Zotero."
    :type 'integer
    :group 'zotero)

  (defcustom zot-retry-delay 3
    "Delay in seconds between retries."
    :type 'integer
    :group 'zotero)

  (defun zot--is-zotero-running ()
    "Check if Zotero is running by attempting to connect to its API."
    (condition-case nil
        (with-current-buffer
            (url-retrieve-synchronously "http://localhost:23119/better-bibtex" t t 1)
          t)
      (error nil)))

  (defun zot--start-zotero ()
    "Start Zotero application."
    (start-process "zotero" nil "open" "-a" "Zotero"))

  (defun zot-open-pdf (citekey)
    "Open PDF for given CITEKEY using Zotero Better BibTeX."
    (unless citekey
      (error "Citation key cannot be empty"))

    (unless (zot--is-zotero-running)
      (message "Zotero is not running. Starting Zotero...")
      (zot--start-zotero)
      (let ((retries 0))
        (while (and (< retries zot-max-retries)
                    (not (zot--is-zotero-running)))
          (message "Waiting for Zotero to start (attempt %d/%d)..."
                   (1+ retries) zot-max-retries)
          (sleep-for zot-retry-delay)
          (setq retries (1+ retries)))
        (unless (zot--is-zotero-running)
          (error "Could not connect to Zotero after %d attempts" zot-max-retries))))

    (let* ((url "http://localhost:23119/better-bibtex/json-rpc")
           (json-payload (json-encode
                          `(("jsonrpc" . "2.0")
                            ("method" . "item.attachments")
                            ("params" . [,citekey]))))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/json")
              ("Accept" . "application/json")))
           (url-request-data json-payload))

      (with-current-buffer
          (url-retrieve-synchronously url)
        (goto-char url-http-end-of-headers)
        (let* ((json-object-type 'alist)
               (json-response (json-read))
               (pdf-url (alist-get 'open
                                   (aref (alist-get 'result json-response) 0))))
          (when (getenv "DEBUG")
            (message "zot-open-pdf: URL for `%s` from better-bibtex = `%s`"
                     citekey pdf-url))
          (if pdf-url
              (start-process "open-pdf" nil "open" pdf-url)
            (error "Could not find PDF URL for citation key %s" citekey))))))
