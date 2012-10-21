(require 'cl)

(defvar skewer-clients ())

(defun httpd/skewer/get (proc path &rest args)
  (push proc skewer-clients))

(defservlet skewer/post text/plain (path args req)
  (message "%s" (cadr (assoc "Content" req))))

(defun skewer-eval (string)
  (while skewer-clients
    (condition-case error-case
        (with-httpd-buffer (pop skewer-clients) "text/javascript"
          (insert string))
      (error nil))))

(skewer-eval "Math.pow(35, 5)")
(skewer-eval "$('h1').html('Foobar');")
