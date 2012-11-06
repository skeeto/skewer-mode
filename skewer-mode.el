;;; skewer-mode.el --- live browser JavaScript interaction

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.0
;; Package-Requires: ((simple-httpd "1.2.4") (js2-mode "20090723"))

;;; Commentary:

;;  1. Place dependencies in your `load-path' or load them directly
;;  2. Load skewer-mode.el
;;  3. M-x `run-skewer' to attach a browser to Emacs
;;  4. From a `js2-mode' buffer, send forms to the browser to evaluate

;; The keybindings for evaluating expressions in the browser are just
;; like the Lisp modes. These are provided by the minor mode
;; `skewer-mode'.

;;  * C-x C-e -- `skewer-eval-last-expression'
;;  * C-M-x   -- `skewer-eval-defun'
;;  * C-c C-k -- `skewer-load-buffer'

;; The result of the expression is echoed in the minibuffer.

;; Note: `run-skewer' uses `browse-url' to launch the browser. This
;; may require further setup depending on your operating system and
;; personal preferences.

;; Manual version:

;; To skewer your own document rather than the provided blank one,

;;  1. Load the dependencies
;;  2. Load skewer-mode.el
;;  3. Start the HTTP server (`httpd-start')
;;  4. Put your HTML document under the root (`httpd-root')
;;  5. Include jQuery and "/skewer" as scripts (see `example.html')
;;  6. Visit the document from your browser

;; If your document isn't a static page but is instead hosted by its
;; own server, you can still skewer the page. See the proxy below.

;; With skewer-repl.el loaded, a REPL into the browser can be created
;; with M-x `skewer-repl'. This should work just like a REPL in
;; console within the browser.

;; To work around the same origin policy, skewer can also be a proxy for
;; another site, where it will automatically inject it's own HTML. This
;; is experimental and a bit flaky right now. See skewer-proxy.el.

;;; Code:

(require 'cl)
(require 'simple-httpd)
(require 'js2-mode)
(require 'json)

(defgroup skewer nil
  "Live browser JavaScript interaction."
  :group 'languages)

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

(defstruct skewer-client
  "A client connection awaiting a response."
  proc agent)

(defun skewer-process-queue ()
  "Send all queued messages to clients."
  (when (and skewer-queue skewer-clients)
    (let ((message (pop skewer-queue))
          (sent nil))
      (while skewer-clients
        (condition-case error-case
            (progn
              (let ((proc (skewer-client-proc (pop skewer-clients))))
                (with-temp-buffer
                  (insert (json-encode message))
                  (httpd-send-header proc "text/plain" 200
                                     '("Cache-Control" . "no-cache"))
                  (httpd-send-buffer proc (current-buffer))))
              (setq sent t))
          (error nil)))
      (if (not sent) (push message skewer-queue)))
    (skewer-process-queue)))

(defun skewer-clients-tabulate ()
  "Prepare client list for tabulated-list-mode."
  (loop for client in skewer-clients collect
        (let ((proc (skewer-client-proc client))
              (agent (skewer-client-agent client)))
          (destructuring-bind (host port) (process-contact proc)
            `(,client [,host ,(format "%d" port) ,agent])))))

(define-derived-mode skewer-clients-mode tabulated-list-mode "skewer-clients"
  "Mode for listing browsers attached to Emacs for skewer-mode."
  (setq tabulated-list-format [("Host" 12 t)
                               ("Port" 5 t)
                               ("User Agent" 0 t)])
  (setq tabulated-list-entries #' skewer-clients-tabulate)
  (tabulated-list-init-header))

(defun list-skewer-clients ()
  "List the attached browsers in a buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*skewer-clients*"))
  (skewer-clients-mode)
  (tabulated-list-print))

;; Servlets

(defservlet skewer text/javascript ()
  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root)))

(defun httpd/skewer/get (proc path query req &rest args)
  (let ((agent (second (assoc "User-Agent" req))))
    (push (make-skewer-client :proc proc :agent agent) skewer-clients))
  (skewer-process-queue))

(defservlet skewer/post text/plain (path args req)
  (let* ((result (json-read-from-string (cadr (assoc "Content" req))))
         (callback (intern-soft (cdr (assoc 'callback result)))))
    (when (and callback (member callback skewer-callbacks))
      (funcall callback result))))

(defservlet skewer/demo text/html ()
  (insert-file-contents (expand-file-name "example.html" skewer-data-root)))

;; Minibuffer display

(defun skewer-success-p (result)
  "Return T if result was a success."
  (equal "success" (cdr (assoc 'status result))))

(define-derived-mode skewer-error-mode special-mode "skewer-error"
  :group 'skewer
  "Mode for displaying JavaScript errors returned by skewer-mode."
  (setq truncate-lines t))

(defface skewer-error-face
  '((((class color) (background light))
     :foreground "red" :underline t)
    (((class color) (background dark))
     :foreground "red" :underline t))
  "Face for JavaScript errors."
  :group 'skewer)

(defun skewer--error (string)
  (propertize string 'font-lock-face 'skewer-error-face))

(defun skewer-post-minibuffer (result)
  "Report results in the minibuffer or the error buffer."
  (if (skewer-success-p result)
      (message "%s" (cdr (assoc 'value result)))
    (with-current-buffer (pop-to-buffer (get-buffer-create "*skewer-error*"))
      (let ((inhibit-read-only t)
            (error (cdr (assoc 'error result))))
        (erase-buffer)
        (skewer-error-mode)
        (insert (skewer--error (cdr (assoc 'name error))) ": ")
        (insert (cdr (assoc 'message error)) "\n\n")
        (insert (or (cdr (assoc 'stack error)) "") "\n\n")
        (insert (format "Expression: %s\n\n"
                        (if (cdr (assoc 'strict result)) "(strict)" ""))
                (cdr (assoc 'eval error)))
        (goto-char (point-min))))))

;; Evaluation functions

(defun* skewer-eval (string &optional callback &key verbose strict)
  "Evaluate STRING in the waiting browsers, giving the result to
CALLBACK. VERBOSE controls the verbosity of the returned
string. The callback function must be listed in `skewer-callbacks'."
  (let ((request `((eval . ,string)
                   (callback . ,callback)
                   (id . ,(random most-positive-fixnum))
                   (verbose . ,verbose)
                   (strict . ,strict))))
    (if (or (not callback) (member callback skewer-callbacks))
        (prog1 request
          (setq skewer-queue (append skewer-queue (list request)))
          (skewer-process-queue))
      (error "Provided callback is not whitelisted in `skewer-callbacks'."))))

(defun skewer-mode-strict-p ()
  "Return T if buffer contents indicates strict mode."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (js2-forward-sws)
      (forward-char 1)
      (let* ((stricts '("\"use strict\"" "'use strict'"))
             (node (js2-node-at-point))
             (code (buffer-substring-no-properties (js2-node-abs-pos node)
                                                   (js2-node-abs-end node))))
        (and (member code stricts) t)))))

(defun skewer-eval-last-expression ()
  "Evaluate the JavaScript expression before the point in the
waiting browser."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-last-expression)
    (save-excursion
      (js2-backward-sws)
      (backward-char)
      (let ((node (js2-node-at-point nil t)))
        (when (eq js2-FUNCTION (js2-node-type (js2-node-parent node)))
          (setq node (js2-node-parent node)))
        (when (js2-ast-root-p node)
          (error "no expression found"))
        (let ((start (js2-node-abs-pos node))
              (end (js2-node-abs-end node)))
          (when (fboundp 'slime-flash-region)
            (slime-flash-region start end))
          (skewer-eval (buffer-substring-no-properties start end)
                       #'skewer-post-minibuffer))))))

(defun skewer-eval-defun ()
  "Evaluate the JavaScript expression before the point in the
waiting browser."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-last-expression)
    (save-excursion
      (js2-backward-sws)
      (backward-char)
      (let ((node (js2-node-at-point nil t)))
        (when (js2-ast-root-p node)
          (error "no expression found"))
        (while (and (js2-node-parent node)
                    (not (js2-ast-root-p (js2-node-parent node))))
          (setf node (js2-node-parent node)))
        (let ((start (js2-node-abs-pos node))
              (end (js2-node-abs-end node)))
          (when (fboundp 'slime-flash-region)
            (slime-flash-region start end))
          (skewer-eval (buffer-substring-no-properties start end)
                       #'skewer-post-minibuffer))))))

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
  :keymap skewer-mode-map
  :group 'skewer)

;;;###autoload
(add-hook 'js2-mode-hook 'skewer-mode)

;;;###autoload
(defun run-skewer ()
  "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser."
  (interactive)
  (httpd-start)
  (browse-url (format "http://127.0.0.1:%d/skewer/demo" httpd-port)))

(provide 'skewer-mode)

;;; skewer-mode.el ends here
