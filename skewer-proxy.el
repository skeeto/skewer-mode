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

(defvar skewer-proxy-url "http://example.com"
  "Remote server for the silent proxy.")

(defvar skewer--orig-httpd/ (symbol-function 'httpd/)
  "Original definition of the servlet at httpd/.")

(defvar skewer--enabled nil
  "Current status of skewer-proxy.")

(defun skewer-proxy-servlet (proc p &rest args)
  (with-current-buffer (url-retrieve-synchronously (concat skewer-proxy-url p))
    (let* ((header-string (car (split-string (buffer-string) "\n\r?\n\r?")))
           (header (httpd-parse header-string)))
      (process-send-region proc (point-min) (point-max))
      (if (equal "close" (cadr (assoc "Connection" header)))
          (process-send-eof proc)))))

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

(provide 'skewer-proxy)

;;; skewer-proxy.el ends here
