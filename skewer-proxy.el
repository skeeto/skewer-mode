;;; skewer-proxy.el --- proxy a website so that it can be skewered

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Due to same origin policy, simply including the skewer JavaScript
;; in a page is not enough to skewer that website. The page must be
;; served by Emacs. To help accommodate this, Emacs can act as a
;; transparent proxy.

;; http://en.wikipedia.org/wiki/Same_origin_policy

;; Set `skewer-proxy-url' to the remote URL. Don't include the path,
;; so there's no trailing slash. Then enable the proxy with
;; `skewer-proxy-enable', which will overwrite the servlet currently
;; installed in `httpd/', probably the simple-httpd file server.
;; Finally, `skewer-proxy-disable' will restore the original `httpd/'
;; servlet.

;;; Code:

(require 'skewer-mode)

(defcustom skewer-proxy-url "http://example.com"
  "Remote server for the silent proxy."
  :type 'string
  :group 'skewer)

(defcustom skewer-inject t
  "When T, skewer will attempt to modify pages to insert its script."
  :type 'boolean
  :group 'skewer)

(defvar skewer--orig-httpd/ (symbol-function 'httpd/)
  "Original definition of the servlet at httpd/.")

(defvar skewer--enabled nil
  "Current status of skewer-proxy.")

(defun skewer-proxy-servlet (proc p &rest args)
  "Passes requests through to remote host at `httpd-proxy-url'."
  (with-current-buffer (url-retrieve-synchronously (concat skewer-proxy-url p))
    (let* ((header-string (car (split-string (buffer-string) "\n\r?\n\r?")))
           (header (httpd-parse header-string)))
      (if skewer-inject (skewer-inject))
      (process-send-region proc (point-min) (point-max))
      (if (equal "close" (cadr (assoc "Connection" header)))
          (process-send-eof proc)))))

;;;###autoload
(defun skewer-proxy-enable ()
  "Enable the skewer proxy, overwriting the httpd/ servlet."
  (interactive)
  (unless skewer--enabled
    (setq skewer--enabled t)
    (setq skewer--orig-httpd/ (symbol-function 'httpd/))
    (fset 'httpd/ (symbol-function 'skewer-proxy-servlet))))

(defun skewer-proxy-disable ()
  "Disable the skewer proxy, restoring the original httpd/ function."
  (interactive)
  (fset 'httpd/ skewer--orig-httpd/)
  (setq skewer--enabled nil))

;; Page manipulation

(defvar skewer-inject-string
  "<script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js\"></script><script src=\"/skewer\"></script>"
  "String to inject to skewer a page.")

(defun skewer--skeweredp ()
  "Determine if the current page has been skewered already."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\"/skewer\"" nil t)))

(defun skewer--content-type ()
  "Determine the content type of the current page."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "Content-Type: " nil t)
    (read (current-buffer))))

(defun skewer-inject ()
  "Inject the skewer script into the current page."
  (when (and (not (skewer--skeweredp)) (eq (skewer--content-type) 'text/html))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "</head>" nil t)
          (backward-char (length "</head>"))
          (insert skewer-inject-string)
          (goto-char (point-min))
          (when (re-search-forward "Content-Length: " nil t)
            (let* ((length (read (current-buffer)))
                   (fixed (+ length (length skewer-inject-string))))
              (backward-kill-word 1)
              (insert (number-to-string fixed)))))))))

(provide 'skewer-proxy)

;;; skewer-proxy.el ends here
