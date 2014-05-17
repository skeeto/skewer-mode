;;; skewer-bosh.el --- BOSH fallback for Skewer -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eieio)
(require 'skewer-mode)

(defclass skewer-bosh-client (skewer-client)
  ((queue :accessor skewer-queue
          :documentation "List of requests queued for this client.")
   (ready-p :accessor skewer-ready-p
            :initform t
            :documentation "Non-nil if client is ready for another request."))
  (:documentation "A client connection that relies on long-polling."))

(defmacro skewer--with-response-buffer (process status &rest body)
  "Respond to PROCESS with CORS headers."
  (declare (indent 2))
  `(with-temp-buffer
     (let ((standard-output (current-buffer)))
       ,@body
       (httpd-send-header ,process "text/plain" ,status
                          :Cache-Control "no-cache"
                          :Keep-Alive "timeout=300, max=2"
                          :Access-Control-Allow-Origin "*"))))

(defmethod skewer-close ((client skewer-bosh-client))
  (let ((process (skewer-process client)))
    (when (process-live-p process)
      (ignore-errors
        (skewer--with-response-buffer process 200)))))

(defmethod skewer-next ((client skewer-bosh-client))
  "Handle the next request in CLIENT's queue."
  (if (skewer-live-p client)
      (when (and (skewer-ready-p client) (skewer-queue client))
        (let ((request (pop (skewer-queue client))))
          (skewer--with-response-buffer (skewer-process client) 200
            (insert (json-encode request)))
          (setf (skewer-ready-p client) nil)))
    (skewer-close client)))

(defmethod skewer-request ((client skewer-bosh-client) request)
  (setf (skewer-queue client) (nconc (skewer-queue client) (list request)))
  (skewer-next client))

(defservlet skewer/channel "text/plain" (_path _query request)
  (let* ((method (car (car request)))
         (client-id (cadr (assoc "X-Skewer-Connection-Key" request)))
         (client (skewer-get-client client-id))
         (process (httpd-resolve-proc t)))
    (when (null client)
      (setf client
            (make-instance 'skewer-bosh-client
                           :process process
                           :id client-id
                           :agent (cadr (assoc "User-Agent" request)))))
    (cond
     ;; Respond to CORS preflight check
     ((string= method "OPTIONS")
      (ignore-errors
        (httpd-send-header
         t "text/plain" 200
         :Cache-Control "no-cache"
         :Keep-Alive "timeout=300, max=2"
         :Access-Control-Allow-Methods "POST, GET, OPTIONS"
         :Access-Control-Allow-Headers "X-Skewer-Connection-Key, Content-Type"
         :Access-Control-Allow-Origin "*"
         :Access-Control-Max-Age "3600")))
     ;; Process responses from the client
     ((string= method "POST")
      (let ((messages (json-read-from-string (cadr (assoc "Content" request)))))
        (mapc (apply-partially #'skewer-response client) messages)))
     ;; Mark client as ready
     ((progn
        (httpd-discard-buffer)
        (setf (skewer-process client) process
              (skewer-ready-p client) t)
        (skewer-next client))))))

(provide 'skewer-bosh)

;;; skewer-bosh.el ends here
