(require 'cl)
(require 'js)

(defvar skewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") 'skewer-eval-last-expression)
    map)
  "Keymap for skewer-mode.")

(defvar skewer-clients ()
  "Browsers awaiting JavaScript snippets.")

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
      (skewer-eval (buffer-substring-no-properties last p)))))

(define-minor-mode skewer-mode
  "Minor mode for interacting with a browser."
  :lighter " skewer"
  :keymap skewer-mode-map)

(add-hook 'js-mode-hook 'skewer-mode)
