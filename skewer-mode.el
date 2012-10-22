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

;; With `skewer-mode' enabled in a buffer, typing C-x C-e
;; (`skewer-eval-last-expression') or C-M-x (`skewer-eval-defun') will
;; evaluate the JavaScript expression before the point in the visiting
;; browser, like the various Lisp modes. The result of the expression
;; is echoed in the minibuffer.

;;; Code:

(require 'cl)
(require 'simple-httpd)
(require 'js)
(require 'json)

(defvar skewer-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") 'skewer-eval-last-expression)
      (define-key map (kbd "C-M-x") 'skewer-eval-defun)))
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

(define-minor-mode skewer-mode
  "Minor mode for interacting with a browser."
  :lighter " skewer"
  :keymap skewer-mode-map)

(add-hook 'js-mode-hook 'skewer-mode)

(provide 'skewer-mode)

;;; skewer-mode.el ends here
