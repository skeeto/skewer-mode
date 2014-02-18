;;; skewer-websocket.el --- websocket client -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eieio)
(require 'websocket)
(require 'skewer-mode)

(defvar skewer-websocket-port 9999
  "Port for serving websocket connections.")

(defvar skewer-websocket-server nil
  "WebSocket server connection.")

(defclass skewer-ws-client (skewer-client)
  ((websocket :accessor skewer-websocket
              :initarg :websocket
              :documentation "Struct from the websocket library.")))

(defun skewer-ws-to-client (websocket)
  "Return the skewer-client belonging to WEBSOCKET."
  (skewer-get-client (process-get (websocket-conn websocket) :skewer-id)))

(defun skewer-ws-on-open (websocket)
  "The server's WebSocket onopen event."
  (let ((id (skewer-id-create))
        (process (websocket-conn websocket)))
    (process-put process :skewer-id id)
    (make-instance 'skewer-ws-client
                   :process process
                   :id id
                   :agent "websocket"
                   :websocket websocket)))

(defun skewer-ws-on-message (websocket frame)
  "The server's WebSocket onmessage event."
  (let ((client (skewer-ws-to-client websocket)))
    (mapc (apply-partially #'skewer-response client)
          (json-read-from-string (websocket-frame-payload frame)))))

(defun skewer-ws-on-close (websocket)
  "The server's WebSocket onclose event."
  (skewer-close (skewer-ws-to-client websocket)))

(defun skewer-websocket-start ()
  "Start the Skewer WebSocket server."
  (interactive)
  (unless skewer-websocket-server
    (setf skewer-websocket-server
          (websocket-server skewer-websocket-port
                            :on-open #'skewer-ws-on-open
                            :on-message #'skewer-ws-on-message
                            :on-close #'skewer-ws-on-close))))

(defun skewer-websocket-stop ()
  "Stop the Skewer WebSocket server."
  (interactive)
  (when skewer-websocket-server
    (websocket-server-close skewer-websocket-server)
    (setf skewer-websocket-server nil)))

(defmethod skewer-close ((client skewer-ws-client))
  (let ((process (skewer-process client)))
    (when (process-live-p process)
      (websocket-close process))))

(defmethod skewer-request ((client skewer-ws-client) request)
  (websocket-send-text (skewer-websocket client) (json-encode request)))

(defun skewer-websocket-first-start ()
  "One-shot hook function to start the websocket server."
  (skewer-websocket-start)
  (remove-hook 'skewer-js-hook #'skewer-websocket-first-start))

(add-hook 'skewer-js-hook #'skewer-websocket-first-start)

(defservlet skewer/websocket/port "text/plain" ()
  "Whisper the websocket port to the browser."
  (if skewer-websocket-server
      (insert (json-encode skewer-websocket-port))
    (insert (json-encode json-null))))

(provide 'skewer-websocket)

;;; skewer-websocket.el ends here
