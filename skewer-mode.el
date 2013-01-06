;;; skewer-mode.el --- live browser JavaScript interaction

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.2
;; Package-Requires: ((simple-httpd "1.4.0") (js2-mode "20090723"))

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

;; Multiple browsers and browser tabs can be attached to Emacs at
;; once. JavaScript forms are sent to all attached clients
;; simultaneously, and each will echo back the result
;; individually. Use `list-skewer-clients' to see a list of all
;; currently attached clients.

;; Sometimes Skewer's long polls from the browser will timeout after a
;; number of hours of inactivity. If you find the browser disconnected
;; from Emacs for any reason, use the browser's console to call
;; skewer() to reconnect. This avoids a page reload, which would lose
;; any fragile browser state you might care about.

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

;;; History:

;; Version 1.2: features
;;   * Add a skewer-eval-print-last-expression
;;   * Display evaluation time when it's long
;;   * Flash the region on eval
;;   * Improve JS stringification
;; Version 1.1: features and fixes
;;   * Added `list-skewer-clients'
;;   * Reduce the number of HTTP requests needed
;;   * Fix stringification issues
;; Version 1.0: initial release

;;; Code:

(require 'cl)
(require 'json)
(require 'simple-httpd)
(require 'js2-mode)
(require 'cache-table)

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

(defvar skewer-timeout 3600
  "Maximum time to wait on the browser to respond, in seconds.")

(defvar skewer-clients ()
  "Browsers awaiting JavaScript snippets.")

(defvar skewer-callbacks (make-cache-table skewer-timeout :test 'equal)
  "Maps evaluation IDs to local callbacks.")

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
                                     :Cache-Control "no-cache")))
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

(defun skewer-queue-client (proc req)
  "Add a client to the queue, given the HTTP header."
  (let ((agent (second (assoc "User-Agent" req))))
    (push (make-skewer-client :proc proc :agent agent) skewer-clients))
  (skewer-process-queue))

;; Servlets

(defservlet skewer text/javascript ()
  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root)))

(defun httpd/skewer/get (proc path query req &rest args)
  (skewer-queue-client proc req))

(defun httpd/skewer/post (proc path query req &rest args)
  (let* ((result (json-read-from-string (cadr (assoc "Content" req))))
         (id (cdr (assoc 'id result)))
         (type (cdr (assoc 'type result)))
         (callback (get-cache-table id skewer-callbacks)))
    (when (and (member type '("log" "error"))
               (fboundp 'skewer-post-log))
      (skewer-post-log result))
    (when callback
      (funcall callback result))
    (skewer-queue-client proc req)))

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
  "Return STRING propertized as an error message."
  (propertize string 'font-lock-face 'skewer-error-face))

(defun skewer-post-minibuffer (result)
  "Report results in the minibuffer or the error buffer."
  (if (skewer-success-p result)
      (let ((value (cdr (assoc 'value result)))
            (time (cdr (assoc 'time result))))
        (if (> time 1.0)
            (message "%s (%.3f seconds)" value time)
          (message "%s" value)))
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

(defun* skewer-eval (string &optional callback &key verbose strict (type "eval"))
  "Evaluate STRING in the waiting browsers, giving the result to
CALLBACK. VERBOSE controls the verbosity of the returned string."
  (let* ((id (format "%x" (random most-positive-fixnum)))
         (request `((type . ,type)
                    (eval . ,string)
                    (id . ,id)
                    (verbose . ,verbose)
                    (strict . ,strict))))
    (prog1 request
      (setf (get-cache-table id skewer-callbacks) callback)
      (setq skewer-queue (append skewer-queue (list request)))
      (skewer-process-queue))))

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

(defun skewer-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun skewer-get-last-expression ()
  "Return the JavaScript expression before the point as a
list: (string start end)."
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
        (list (buffer-substring-no-properties start end) start end)))))

(defun skewer-eval-last-expression ()
  "Evaluate the JavaScript expression before the point in the
waiting browser."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-last-expression)
    (destructuring-bind (string start end) (skewer-get-last-expression)
      (skewer-flash-region start end)
      (skewer-eval string #'skewer-post-minibuffer))))

(defun skewer-get-defun ()
  "Return the toplevel JavaScript expression around the point as
a list: (string start end)."
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
        (list (buffer-substring-no-properties start end) start end)))))

(defun skewer-eval-defun ()
  "Evaluate the JavaScript expression before the point in the
waiting browser."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-last-expression)
    (destructuring-bind (string start end) (skewer-get-defun)
      (skewer-flash-region start end)
      (skewer-eval string #'skewer-post-minibuffer))))

;; Print last expression

(defvar skewer-eval-print-map (make-cache-table skewer-timeout :test 'equal)
  "A mapping of evaluation IDs to insertion points.")

(defun skewer-post-print (result)
  "Insert the result after its source expression."
  (if (not (skewer-success-p result))
      (skewer-post-minibuffer result)
    (let* ((id (cdr (assoc 'id result)))
           (pos (get-cache-table id skewer-eval-print-map)))
      (when pos
        (with-current-buffer (car pos)
          (goto-char (cdr pos))
          (insert (cdr (assoc 'value result)) "\n"))))))

(defun skewer-eval-print-last-expression ()
  "Evaluate the JavaScript expression before the point in the
waiting browser and insert the result in the buffer at point."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'skewer-eval-print-last-expression)
    (destructuring-bind (string start end) (skewer-get-defun)
      (skewer-flash-region start end)
      (insert "\n")
      (let* ((request (skewer-eval string #'skewer-post-print :verbose t))
             (id (cdr (assoc 'id request)))
             (pos (cons (current-buffer) (point))))
        (setf (get-cache-table id skewer-eval-print-map) pos)))))

;; Script loading

(defvar skewer-hosted-scripts (make-cache-table skewer-timeout)
  "Map of hosted scripts to IDs.")

(defun skewer-host-script (string)
  "Host script STRING from the script servlet, returning the script ID."
  (let ((id (random most-positive-fixnum)))
    (prog1 id
      (setf (get-cache-table id skewer-hosted-scripts) string))))

(defun skewer-load-buffer ()
  "Load the entire current buffer into the browser. A snapshot of
the buffer is hosted so that browsers visiting late won't see an
inconsistent buffer."
  (interactive)
  (let ((id (skewer-host-script (buffer-string))))
    (skewer-eval (format "$.getScript('/skewer/script/%d')" id)
                 'skewer-post-minibuffer)))

(defservlet skewer/script text/javascript (path)
  (let ((id (string-to-number (file-name-nondirectory path))))
    (insert (get-cache-table id skewer-hosted-scripts ""))))

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
