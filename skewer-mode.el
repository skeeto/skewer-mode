;;; skewer-mode.el --- live browser JavaScript interaction

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.0
;; Package-Requires: ((simple-httpd "1.2.3"))

;;; Commentary:

;;  1. Start the HTTP server (`httpd-start')
;;  2. Put your HTML document in the root (`httpd-root')
;;  3. Include jQuery and `/skewer` as scripts (see example.html)
;;  4. Visit the document from a browser (probably http://localhost:8080/)

;; With `skewer-mode' enabled in the buffer, these commands will
;; evaluate the JavaScript expression around the point, like the
;; various Lisp modes.

;;  * C-x C-e   --  `skewer-eval-last-expression'
;;  * C-M-x     --  `skewer-eval-defun'
;;  * C-c C-k   --  `skewer-load-buffer'

;; The result of the expression is echoed in the minibuffer.

;; There's also a REPL (`skewer-repl') provided in skewer-repl.el.

;; To work around the same origin policy, skewer can become a
;; transparent proxy to serve another website through Emacs, injecting
;; its own JavaScript. See skewer-proxy.el.

;;; Code:

(require 'cl)
(require 'simple-httpd)
(require 'js)
(require 'json)

(defvar skewer-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") 'skewer-eval-last-expression)
      (define-key map (kbd "C-M-x") 'skewer-eval-defun)
      (define-key map (kbd "C-c C-k") 'skewer-load-buffer)))
  "Keymap for skewer-mode.")

(defvar skewer-data-root (file-name-directory load-file-name)
  "Location of data files needed by impatient-mode.")

(defvar skewer-clients ()
  "Browsers awaiting JavaScript snippets.")

(defvar skewer-callbacks '(skewer-post-minibuffer)
  "A whitelist of valid callback functions. The browser hands
back the name of the callback function, which we can't
trust. These whitelisted functions are considered safe.")

(defvar skewer-queue ()
  "Queued messages for the browser.")

(defun skewer-process-queue ()
  "Send all queued messages to clients."
  (when (and skewer-queue skewer-clients)
    (let ((message (pop skewer-queue))
          (sent nil))
      (while skewer-clients
        (condition-case error-case
            (progn
              (with-httpd-buffer (pop skewer-clients) "text/plain"
                (insert (json-encode message)))
              (setq sent t))
          (error nil)))
      (if (not sent) (push message skewer-queue)))
    (skewer-process-queue)))

;; Servlets

(defservlet skewer text/javascript ()
  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root)))

(defun httpd/skewer/get (proc path &rest args)
  (push proc skewer-clients)
  (skewer-process-queue))

(defservlet skewer/post text/plain (path args req)
  (let* ((result (json-read-from-string (cadr (assoc "Content" req))))
         (callback (intern-soft (cdr (assoc 'callback result)))))
    (if (member callback skewer-callbacks)
        (funcall callback result)
      (message "warning: invalid callback: %s" callback))))

(defservlet skewer/example text/html ()
  (insert-file-contents (expand-file-name "example.html" skewer-data-root)))

;; Minibuffer display

(defun skewer-success-p (result)
  "Return T if result was a success."
  (equal "success" (cdr (assoc 'status result))))

(define-derived-mode skewer-error-mode special-mode "skewer-error"
  "Mode for displaying JavaScript errors returned by skewer-mode."
  (setq truncate-lines t))

(defun skewer-post-minibuffer (result)
  "Report results in the minibuffer or the error buffer."
  (if (skewer-success-p result)
      (message "%s" (cdr (assoc 'value result)))
    (with-current-buffer (pop-to-buffer (get-buffer-create "*skewer-error*"))
      (let ((inhibit-read-only t)
            (error (cdr (assoc 'error result))))
        (erase-buffer)
        (skewer-error-mode)
        (insert (cdr (assoc 'value result)) "\n")
        (insert (cdr (assoc 'stack error)) "\n")
        (beginning-of-buffer)))))

;; Evaluation functions

(defun skewer-eval (string callback)
  "Evaluate STRING in the waiting browsers, giving the result to
CALLBACK. The callback function must be listed in `skewer-callbacks'."
  (let ((request `((eval . ,string)
                   (callback . ,callback)
                   (id . ,(random most-positive-fixnum)))))
    (prog1 request
      (setq skewer-queue (append skewer-queue (list request)))
      (skewer-process-queue))))

(defun skewer-eval-last-expression ()
  "Evaluate the JavaScript expression before the point in the
waiting browser."
  (interactive)
  (save-excursion
    (re-search-backward "[^ \n\r;]+")
    (forward-char)
    (let ((p (point))
           (last (point-min)))
      (goto-char (point-min))
      (while (< (point) p)
        (setq last (point))
        (re-search-forward "[^ \n\r;]")
        (backward-char)
        (js--forward-expression))
      (skewer-eval (buffer-substring-no-properties last p)
                   'skewer-post-minibuffer))))

(defun skewer-eval-defun ()
  "Evaluate the JavaScript expression around the point in the
waiting browser."
  (interactive)
  (save-excursion
    (backward-paragraph)
    (let ((start (point)))
      (forward-paragraph)
      (skewer-eval (buffer-substring-no-properties start (point))
                   'skewer-post-minibuffer))))

;; Script loading

(defvar skewer-hosted-scripts (make-hash-table)
  "Map of hosted scripts to IDs.")

(defun skewer-host-script (string)
  (let ((id (random most-positive-fixnum)))
    (prog1 id
      (puthash id (cons (float-time) string) skewer-hosted-scripts)
      (maphash (lambda (k v)
                 (if (> (- (float-time) 3600) (car v))
                     (remhash k skewer-hosted-scripts)))
               skewer-hosted-scripts))))

(defun skewer-load-buffer ()
  "Load the current buffer into the browser."
  (interactive)
  (let ((id (skewer-host-script (buffer-string))))
    (skewer-eval (format "$.getScript('/skewer/script/%d')" id)
                 'skewer-post-minibuffer)))

(defservlet skewer/script text/javascript (path)
  (let ((id (string-to-number (file-name-nondirectory path))))
    (insert (cdr (gethash id skewer-hosted-scripts)))))

;; Define the minor mode

(define-minor-mode skewer-mode
  "Minor mode for interacting with a browser."
  :lighter " skewer"
  :keymap skewer-mode-map)

(add-hook 'js-mode-hook 'skewer-mode)

(provide 'skewer-mode)

;;; skewer-mode.el ends here
