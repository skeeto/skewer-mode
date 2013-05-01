;;; skewer-html.el --- skewer support for live-interaction HTML

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This minor mode provides functionality for HTML like plain Skewer
;; does for JavaScript.

;; * C-M-x   -- `skewer-html-eval-tag'
;; * C-c C-g -- `skewer-html-fetch-selector-into-buffer'

;; HTML is by default, appended to the "body" selector, but this can
;; be overriden by prefixing (C-u) the above interactions.

;;; Code:

(require 'cl)
(require 'sgml-mode)
(require 'skewer-mode)

;; Selector computation

(defun skewer-html--cleanup (name)
  "Cleanup tag names provided by sgml-mode."
  (replace-regexp-in-string "/$" "" name))

(defun skewer-html--tag-after-point ()
  "Return the tag struct for the tag immediately following point."
  (save-excursion
    (forward-char 1)
    (sgml-parse-tag-backward)))

(defun skewer-html-compute-tag-nth ()
  "Compute the position of this tag within its parent."
  (save-excursion
    (let ((tag (car (last (sgml-get-context)))))
      (if (null tag)
          1
        (loop with start = (sgml-tag-name tag)
              with stop = (save-excursion (sgml-get-context) (point))
              with n = 1
              do (sgml-skip-tag-backward 1)
              while (> (point) stop)
              when (equal start (sgml-tag-name (skewer-html--tag-after-point)))
              do (incf n)
              finally (return n))))))

(defun skewer-html-compute-tag-ancestry ()
  "Compute the ancestry chain at point."
  (save-excursion
    (nreverse
     (loop for nth = (skewer-html-compute-tag-nth)
           for tag = (car (last (sgml-get-context)))
           while tag
           for name = (skewer-html--cleanup (sgml-tag-name tag))
           for type = (sgml-tag-type tag)
           when (not (or (string= name "html")
                         (eq type 'close)))
           collect (list name nth)))))

(defun skewer-html-compute-selector ()
  "Compute the selector for exactly the tag around point."
  (let ((ancestry (skewer-html-compute-tag-ancestry)))
    (mapconcat (lambda (tag)
                 (format "%s:nth-of-type(%d)" (first tag) (second tag)))
               ancestry " > ")))

;; Fetching

(defun skewer-html-fetch-selector (selector)
  "Fetch the innerHTML of a selector."
  (let ((result (skewer-eval-synchronously selector :type "fetchselector")))
    (if (skewer-success-p result)
        (cdr (assoc 'value result))
      "")))

(defun skewer-html-fetch-selector-into-buffer (selector)
  "Fetch the innerHTML of a selector and insert it into the active buffer."
  (interactive "P")
  (let ((sel (if selector
                 (read-string "Selector: ")
               skewer-html-options-selector)))
    (insert (skewer-html-fetch-selector sel))))

;; Evaluation

(defun skewer-html-eval (string selector &optional append)
  "Load HTML into a selector, optionally appending."
  (skewer-eval string nil :type "html" :extra `((selector . ,selector)
                                                (append   . ,append))))

(defun skewer-html-eval-tag (&optional prefix)
  "Load HTML from the surrounding tag. When prefixed, prompt for options."
  (interactive "P")
  (save-excursion
    (let* ((selector (skewer-html-compute-selector))
           (beg (progn (sgml-skip-tag-backward 1) (point)))
           (end (progn (sgml-skip-tag-forward 1) (point)))
           (region (buffer-substring-no-properties beg end)))
      (skewer-flash-region beg end)
      (skewer-html-eval region selector nil))))

;; Minor mode definition

(defvar skewer-html-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-M-x") 'skewer-html-eval-tag)
      (define-key map (kbd "C-c C-g") 'skewer-html-fetch-selector-into-buffer)))
  "Keymap for skewer-html-mode")

;;;###autoload
(define-minor-mode skewer-html-mode
  "Minor mode for interactively loading new HTML."
  :lighter " skewer-html"
  :keymap skewer-html-mode-map
  :group 'skewer)

;;;###autoload
(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'skewer-html)

;;; skewer-html.el ends here
