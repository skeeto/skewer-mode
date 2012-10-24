;;; skewer-mode.el --- live browser JavaScript interaction

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.0
;; Package-Requires: ((simple-httpd "1.2.3") (js2-mode "20090723"))

;;; Commentary:

;;  1. Start the HTTP server (`httpd-start')
;;  2. Put your HTML document in the root (`httpd-root')
;;  3. Include jQuery and `/skewer` as scripts (see example.html)
;;  4. Visit the document from a browser

;; As a shortcut, the above can mostly be done with `run-skewer'. A
;; browser will be launched and connected to Emacs for use as a
;; JavaScript REPL.

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
(require 'js2-mode)
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
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-last-expression)
    (save-excursion
      (js2-backward-sws)
      (backward-char)
      (let ((node (js2-node-at-point)))
        (when (js2-ast-root-p node)
          (error "no expression found"))
        (let ((start (js2-node-abs-pos node))
              (end (js2-node-abs-end node)))
          (skewer-eval (buffer-substring-no-properties start end)
                       #'skewer-post-minibuffer))))))

(defun skewer--toplevel-start ()
  "Move point to the beginning of the current toplevel form returning point."
  (interactive)
  (js2-forward-sws)
  (if (= (point) (point-max))
      (js2-mode-forward-sexp -1)
    (let ((node (js2-node-at-point)))
      (when (js2-ast-root-p node)
        (error "cannot locate any toplevel form"))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-pos node))
      (js2-forward-sws)))
  (point))

(defun skewer--toplevel-end ()
  "Move point to the end of the current toplevel form returning point."
  (interactive)
  (js2-forward-sws)
  (let ((node (js2-node-at-point)))
    (unless (or (null node) (js2-ast-root-p node))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-end node)))
    (point)))

(defun skewer-eval-defun ()
  "Evaluate the JavaScript expression around the point in the
waiting browser."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-defun)
    (save-excursion
      (let ((start (skewer--toplevel-start))
            (end (skewer--toplevel-end)))
        (skewer-eval (buffer-substring-no-properties start end)
                     #'skewer-post-minibuffer)))))

;; Script loading

(defvar skewer-hosted-scripts (make-hash-table)
  "Map of hosted scripts to IDs.")

(defun skewer-host-script (string)
  "Host script STRING from the script servlet, returning the script ID."
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

;;;###autoload
(define-minor-mode skewer-mode
  "Minor mode for interacting with a browser."
  :lighter " skewer"
  :keymap skewer-mode-map)

;;;###autoload
(add-hook 'js2-mode-hook 'skewer-mode)

;;;###autoload
(defun run-skewer ()
  "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser."
  (interactive)
  (httpd-start)
  (browse-url (format "http://127.0.0.1:%d/skewer/example" httpd-port)))

(provide 'skewer-mode)

;;; skewer-mode.el ends here
