;;; skewer-mode.el --- live browser JavaScript, CSS, and HTML interaction -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/skewer-mode
;; Version: 1.7.0
;; Package-Requires: ((emacs "24") (simple-httpd "1.4.0") (js2-mode "20090723") (websocket "1.2"))

;;; Commentary:

;; Quick start (without package.el):

;;  1. Put this directory in your `load-path'
;;  2. Load skewer-mode.el
;;  3. M-x `run-skewer' to attach a browser to Emacs
;;  4. From a `js2-mode' buffer with `skewer-mode' minor mode enabled,
;;     send forms to the browser to evaluate

;; The function `skewer-setup' can be used to configure all of mode
;; hooks (previously this was the default). This can also be done
;; manually like so,

;;     (add-hook 'js2-mode-hook 'skewer-mode)
;;     (add-hook 'css-mode-hook 'skewer-css-mode)
;;     (add-hook 'html-mode-hook 'skewer-html-mode)

;; The keybindings for evaluating expressions in the browser are just
;; like the Lisp modes. These are provided by the minor mode
;; `skewer-mode'.

;;  * C-x C-e -- `skewer-eval-last-expression'
;;  * C-M-x   -- `skewer-eval-defun'
;;  * C-c C-k -- `skewer-load-buffer'

;; The result of the expression is echoed in the minibuffer.

;; Additionally, `css-mode' and `html-mode' get a similar set of
;; bindings for modifying the CSS rules and updating HTML on the
;; current page.

;; Note: `run-skewer' uses `browse-url' to launch the browser. This
;; may require further setup depending on your operating system and
;; personal preferences.

;; Multiple browsers and browser tabs can be attached to Emacs at
;; once. JavaScript forms are sent to all attached clients
;; simultaneously, and each will echo back the result
;; individually. Use `skewer-list-clients' to see a list of all
;; currently attached clients.

;; Sometimes Skewer's long polls from the browser will timeout after a
;; number of hours of inactivity. If you find the browser disconnected
;; from Emacs for any reason, use the browser's console to call
;; skewer() to reconnect. This avoids a page reload, which would lose
;; any fragile browser state you might care about.

;; To skewer your own document rather than the provided blank page,

;;  1. Load the dependencies
;;  2. Load skewer-mode.el
;;  3. Start the HTTP server (`httpd-start')
;;  4. Include "http://localhost:8080/skewer" as a script
;;     (see `example.html' and check your `httpd-port')
;;  5. Visit the document from your browser

;; Skewer fully supports CORS, so the document need not be hosted by
;; Emacs itself. A Greasemonkey userscript and a bookmarklet are
;; provided for injecting Skewer into any arbitrary page you're
;; visiting without needing to modify the page on the host.

;; With skewer-repl.el loaded, a REPL into the browser can be created
;; with M-x `skewer-repl', or C-c C-z. This should work like a console
;; within the browser. Messages can be logged to this REPL with
;; skewer.log() (just like console.log()).

;; Extending Skewer:

;; Skewer is flexible and open to extension. The REPL and the CSS and
;; HTML minor modes are a partial examples of this. You can extend
;; skewer.js with your own request handlers and talk to them from
;; Emacs using `skewer-eval' (or `skewer-eval-synchronously') with
;; your own custom :type. The :type string chooses the dispatch
;; function under the skewer.fn object. To inject your own JavaScript
;; into skewer.js, use `skewer-js-hook'.

;; You can also catch messages sent from the browser not in response
;; to an explicit request. Use `skewer-response-hook' to see all
;; incoming objects.

;;; History:

;; Version 1.6.0: fixes
;;   * Bring up to speed with Emacs 24.3
;;   * Switch to cl-lib from cl
;; Version 1.5.3: features
;;   * Add `skewer-run-phantomjs'
;; Version 1.5.2: small cleanup
;;   * Add `skewer-apply' and `skewer-funall'
;;   * Improved safeStringify
;; Version 1.5.1: features
;;   * No more automatic hook setup (see `skewer-setup')
;;   * Support for HTML interaction
;;   * Support for loading Bower packages
;;   * Drop jQuery dependency
;;   * Many small improvements
;; Version 1.4: features
;;   * Full CSS interaction
;;   * Greasemonkey userscript for injection
;;   * Full, working CORS support
;;   * Better browser presence detection
;; Version 1.3: features and fixes
;;   * Full offline support
;;   * No more callback registering
;;   * Fix 64-bit support
;;   * Two new hooks for improved extension support
;;   * More uniform keybindings with other interactive modes
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

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'url-util)
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

(defvar skewer-js-hook ()
  "Hook to run when skewer.js is being served to the browser.

When hook functions are called, the current buffer is the buffer
to be served to the client (a defservlet), with skewer.js script
already inserted. This is the chance for other packages to insert
their own JavaScript to extend skewer in the browser, such as
adding a new type handler.")

(defvar skewer-response-hook ()
  "Hook to run when a response arrives from the browser. Used for
catching messages from the browser with no associated
callback. The response object is passed to the hook function.")

(defvar skewer-timeout 3600
  "Maximum time to keep track of requests, in seconds.")

(defvar skewer-clients ()
  "Browsers awaiting JavaScript snippets.")

;; Skewer Client Class

(defclass skewer-client ()
  ((process :accessor skewer-process
            :initarg :process
            :documentation "Process used to communicate with this client.")
   (id :reader skewer-id
       :initarg :id
       :documentation "Unique identifier for this client.")
   (agent :accessor skewer-agent
          :initarg :agent
          :documentation "User agent string describing this client.")
   (last-seen :accessor skewer-last-seen
              :initform (float-time)
              :documentation "Last time this client was active."))
  (:documentation
   "A connection to a single browser page. Subclasses must
implement `skewer-close' and `skewer-request', and it should call
`skewer-response' with any client responses. Instances are
automatically added to the global client list.")
  :abstract t)

(defgeneric skewer-request (client request)
  "Pass REQUEST to CLIENT.")

(defgeneric skewer-close (client)
  "Close a connection to a specific client.")

(defmethod skewer-close :after ((client skewer-client))
  "Removes CLIENT from the global listing."
  (setf skewer-clients (cl-delete (skewer-id client) skewer-clients
                                  :key #'skewer-id :test #'string=)))

(defmethod skewer-live-p ((client skewer-client))
  "Return non-nil if CLIENT is still alive."
  (process-live-p (skewer-process client)))

;; Generic client handling

(defun skewer-id-create ()
  "Generate a random identifier string."
  (base64-encode-string (concat (cl-loop repeat 12 collect (random 256)))))

(defun skewer-get-client (client-id)
  "Return canonical client object for CLIENT-ID."
  (cl-find client-id skewer-clients :key #'skewer-id :test #'string=))

(defun skewer-clients-tabulate ()
  "Prepare client list for tabulated-list-mode."
  (cl-loop for client in skewer-clients collect
           (let ((proc (skewer-process client))
                 (agent (skewer-agent client))
                 (id (skewer-id client)))
             (cl-destructuring-bind (host port) (process-contact proc)
               `(,client [,host ,(format "%d" port) ,id ,agent])))))

(define-derived-mode skewer-clients-mode tabulated-list-mode "skewer-clients"
  "Mode for listing browsers attached to Emacs for skewer-mode."
  (setq tabulated-list-format [("Host" 12 t)
                               ("Port" 5 t)
                               ("ID" 8 t)
                               ("User Agent" 0 t)])
  (setq tabulated-list-entries #'skewer-clients-tabulate)
  (tabulated-list-init-header))

(define-key skewer-clients-mode-map (kbd "g")
  (lambda ()
    (interactive)
    (skewer-ping)
    (revert-buffer)))

(defun skewer-update-list-buffer ()
  "Revert the client list, due to an update."
  (save-window-excursion
    (let ((list-buffer (get-buffer "*skewer-clients*")))
      (when list-buffer
        (with-current-buffer list-buffer
          (revert-buffer))))))

;;;###autoload
(defun skewer-list-clients ()
  "List the attached browsers in a buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*skewer-clients*"))
  (skewer-clients-mode)
  (tabulated-list-print))

(define-obsolete-function-alias 'list-skewer-clients 'skewer-list-clients
  "1.7.0" "Obsoleted in favor of a cleaner namespace.")

(defmethod initialize-instance :after ((client skewer-client) &key)
  "Add a client to the global list."
  (let* ((id (skewer-id client))
         (position (cl-position id skewer-clients
                                :key #'skewer-id :test #'string=)))
    (prog1 client
      (if (null position)
          (push client skewer-clients)
        (setf (nth position skewer-client) client))
      (skewer-update-list-buffer))))

;; Servlets

(defservlet skewer text/javascript ()
  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root))
  (setf (point) (point-max))
  (run-hooks 'skewer-js-hook))

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
  (propertize (or string "<unknown>") 'font-lock-face 'skewer-error-face))

(defun skewer-post-minibuffer (result)
  "Report results in the minibuffer or the error buffer."
  (if (skewer-success-p result)
      (let ((value (cdr (assoc 'value result)))
            (time (cdr (assoc 'time result))))
        (if (and time (> time 1.0))
            (message "%s (%.3f seconds)" value time)
          (message "%s" value)))
    (with-current-buffer (pop-to-buffer (get-buffer-create "*skewer-error*"))
      (let ((inhibit-read-only t)
            (error (cdr (assoc 'error result))))
        (erase-buffer)
        (skewer-error-mode)
        (insert (skewer--error (cdr (assoc 'name error))) ": ")
        (insert (or (cdr (assoc 'message error)) "") "\n\n")
        (insert (or (cdr (assoc 'stack error)) "") "\n\n")
        (insert (format "Expression: %s\n\n"
                        (if (cdr (assoc 'strict result)) "(strict)" ""))
                (cdr (assoc 'eval error)))
        (goto-char (point-min))))))

;; Request Accessors

(defun skewer--slot-accessor (slot)
  "Return a function that accesses SLOT in an alist."
  (lambda (alist) (cdr (assoc slot alist))))

(defalias 'skewer-request-type (skewer--slot-accessor 'type))
(defalias 'skewer-request-eval (skewer--slot-accessor 'eval))
(defalias 'skewer-request-id (skewer--slot-accessor 'id))
(defalias 'skewer-request-verbose-p (skewer--slot-accessor 'verbose))
(defalias 'skewer-request-strict-p (skewer--slot-accessor 'strict))

;; Evaluation functions

(defvar skewer-callbacks (cache-table-create skewer-timeout :test 'equal)
  "Maps evaluation IDs to local callbacks.")

(defun skewer-register-callback (request-id callback)
  "Register CALLBACK to be called with request reponse."
  (push callback (cache-table-get request-id skewer-callbacks)))

(defun skewer-callback (request-id arg)
  "Call all the callbacks for REQUEST-ID with ARG."
  (let ((callbacks (cache-table-get request-id skewer-callbacks)))
    (dolist (callback callbacks)
      (when callback
        (funcall callback arg)))))

(defmethod skewer-response ((client skewer-client) response)
  "Handle RESPONSE from CLIENT."
  (setf (skewer-last-seen client) (float-time))
  (let* ((request-id (cdr (assoc 'id response))))
    (skewer-callback request-id response)
    (dolist (function skewer-response-hook)
      (funcall function response))))

(cl-defun skewer-eval (string &optional callback
                              &key verbose strict (type "eval") extra)
  "Evaluate STRING in the waiting browsers, giving the result to CALLBACK.

:VERBOSE -- if T, the return will try to be JSON encoded
:STRICT  -- if T, expression is evaluated with 'use strict'
:TYPE    -- chooses the JavaScript handler (default: eval)
:EXTRA   -- additional alist keys to append to the request object"
  ;; Unique request object for each client.
  (cl-loop for client in skewer-clients
           for id = (skewer-id-create)
           for request = `((type . ,type)
                           (eval . ,string)
                           (id . ,id)
                           (verbose . ,verbose)
                           (strict . ,strict)
                           ,@extra)
           when callback do (skewer-register-callback id callback)
           do (skewer-request client request)
           collect request))

(defun skewer-eval-synchronously (string &rest args)
  "Just like `skewer-eval' but synchronously, so don't provide a
callback. Use with caution."
  (let ((result nil))
    (apply #'skewer-eval string (lambda (v) (setq result v)) args)
    (cl-loop until result
             do (accept-process-output nil 0.01)
             finally (return result))))

(defun skewer-apply (function args)
  "Synchronously apply FUNCTION in the browser with the supplied
arguments, returning the result. All ARGS must be printable by
`json-encode'. For example,

    (skewer-apply \"Math.atan2\" '(1 -2)) ; => 2.677945044588987

Uncaught exceptions propagate to Emacs as an error."
  (let ((specials '(("undefined" . nil)
                    ("NaN" . 0.0e+NaN)
                    ("Infinity" . 1.0e+INF)
                    ("-Infinity" . -1.0e+INF))))
    (let* ((expr (concat function "(" (mapconcat #'json-encode args ", ") ")"))
           (result (skewer-eval-synchronously expr :verbose t))
           (value (cdr (assoc 'value result))))
      (if (skewer-success-p result)
          (if (assoc value specials)
              (cdr (assoc value specials))
            (condition-case _
                (json-read-from-string value)
              (json-readtable-error value)))
        (signal 'javascript
                (list (cdr (assoc 'message (cdr (assoc'error result))))))))))

(defun skewer-funcall (function &rest args)
  "Synchronously call FUNCTION with the supplied ARGS. All ARGS
must be printable by `json-read-from-string. For example,

    (skewer-funcall \"Math.sin\" 0.5)  ; => 0.479425538604203

Uncaught exceptions propagate to Emacs as an error."
  (skewer-apply function args))

(defun skewer-ping ()
  "Ping the browser to test that it's still alive."
  (unless (null skewer-clients) ; don't queue pings
    (skewer-eval (prin1-to-string (float-time)) nil :type "ping")))

(defun skewer-last-seen-seconds ()
  "Return the number of seconds since a browser was last seen, nil if never."
  (skewer-ping) ; make sure it's still alive next request
  (cl-loop with time = (float-time)
           for client in skewer-clients
           minimize (- (float-time) (skewer-last-seen client))))

;; Region Grabbing (js2-mode stuff)

(defun skewer-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun skewer--save-point (f &rest args)
  "Return a function that calls F with point at the current point."
  (let ((saved-point (point)))
    (lambda (&rest more)
      (save-excursion
        (goto-char saved-point)
        (apply f (append args more))))))

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

(defun skewer-eval-last-expression (&optional prefix)
  "Evaluate the JavaScript expression before the point in the
waiting browser. If invoked with a prefix argument, insert the
result into the current buffer."
  (interactive "P")
  (if prefix
      (skewer-eval-print-last-expression)
    (if js2-mode-buffer-dirty-p
        (js2-mode-wait-for-parse
         (skewer--save-point #'skewer-eval-last-expression))
      (cl-destructuring-bind (string start end) (skewer-get-last-expression)
        (skewer-flash-region start end)
        (skewer-eval string #'skewer-post-minibuffer)))))

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
      (js2-mode-wait-for-parse (skewer--save-point #'skewer-eval-defun))
    (cl-destructuring-bind (string start end) (skewer-get-defun)
      (skewer-flash-region start end)
      (skewer-eval string #'skewer-post-minibuffer))))

;; Print last expression

(defvar skewer-eval-print-map (cache-table-create skewer-timeout :test 'equal)
  "A mapping of evaluation IDs to insertion points.")

(defun skewer-post-print (result)
  "Insert the result after its source expression."
  (if (not (skewer-success-p result))
      (skewer-post-minibuffer result)
    (let* ((id (skewer-request-id result))
           (pos (cache-table-get id skewer-eval-print-map)))
      (when pos
        (with-current-buffer (car pos)
          (setf (point) (cdr pos))
          (insert (cdr (assoc 'value result)) "\n"))))))

(defun skewer-eval-print-last-expression ()
  "Evaluate the JavaScript expression before the point in the
waiting browser and insert the result in the buffer at point."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse
       (skewer--save-point #'skewer-eval-print-last-expression))
    (cl-destructuring-bind (string start end) (skewer-get-defun)
      (skewer-flash-region start end)
      (let* ((pos (cons (current-buffer) (point)))
             (requests (skewer-eval string #'skewer-post-print :verbose t)))
        (cl-loop for request in requests
                 for id = (skewer-request-id request)
                 do (setf (cache-table-get id skewer-eval-print-map) pos))))))

;; Script loading

(defvar skewer-hosted-scripts (cache-table-create skewer-timeout)
  "Map of hosted scripts to IDs.")

(defun skewer-host-script (string)
  "Host script STRING from the script servlet, returning the script ID."
  (let ((id (random most-positive-fixnum)))
    (prog1 id
      (setf (cache-table-get id skewer-hosted-scripts) string))))

(defun skewer-load-buffer ()
  "Load the entire current buffer into the browser. A snapshot of
the buffer is hosted so that browsers visiting late won't see an
inconsistent buffer."
  (interactive)
  (let ((id (skewer-host-script (buffer-string)))
        (buffer-name (buffer-name)))
    (skewer-eval (format "/skewer/script/%d/%s"
                         id (url-hexify-string buffer-name))
                 (lambda (_) (message "%s loaded" buffer-name))
                 :type "script")))

(defservlet skewer/script text/javascript (path)
  (let ((id (string-to-number (nth 3 (split-string path "/")))))
    (insert (cache-table-get id skewer-hosted-scripts ""))))

;; Define the minor mode

;;;###autoload
(define-minor-mode skewer-mode
  "Minor mode for interacting with a browser."
  :lighter " skewer"
  :keymap skewer-mode-map
  :group 'skewer)

;;;###autoload
(defun run-skewer ()
  "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser."
  (interactive)
  (httpd-start)
  (browse-url (format "http://127.0.0.1:%d/skewer/demo" httpd-port)))

;; PhantomJS

(defvar phantomjs-program-name "/usr/bin/phantomjs"
  "Path to the phantomjs executable.")

(defvar skewer-phantomjs-processes ()
  "List of phantomjs processes connected to Skewer.")

(defun skewer-phantomjs-sentinel (proc event)
  "Cleanup after phantomjs exits."
  (when (cl-some (lambda (s) (string-match-p s event))
                 '("finished" "abnormal" "killed"))
    (delete-file (process-get proc 'tempfile))))

;;;###autoload
(defun skewer-run-phantomjs ()
  "Connect an inferior PhantomJS process to Skewer, returning the process."
  (interactive)
  (httpd-start)
  (let ((script (make-temp-file "phantomjs-"))
        (url (format "http://0:%d/skewer/demo" httpd-port)))
    (with-temp-buffer
      (insert (format "require('webpage').create().open('%s')" url))
      (write-region nil nil script nil 0)
      (let ((proc (start-process "phantomjs" nil
                                 phantomjs-program-name script)))
        (prog1 proc
          (push proc skewer-phantomjs-processes)
          (process-put proc 'tempfile script)
          (set-process-sentinel proc 'skewer-phantomjs-sentinel))))))

(defun skewer-phantomjs-kill ()
  "Kill all inferior phantomjs processes connected to Skewer."
  (interactive)
  (mapc #'kill-process skewer-phantomjs-processes))

(provide 'skewer-mode)

(require 'skewer-bosh)

;;; skewer-mode.el ends here
