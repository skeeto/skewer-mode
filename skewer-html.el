;;; skewer-html.el --- skewer support for live-interaction HTML

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This minor mode provides functionality for HTML like plain Skewer
;; does for JavaScript.

;; * C-M-x   -- `skewer-html-eval-tag'
;; * C-c C-k -- `skewer-html-eval-region'
;; * C-c C-g -- `skewer-html-fetch-selector-into-buffer'

;; HTML is by default, appended to the "body" selector, but this can
;; be overriden by prefixing (C-u) the above interactions.

;;; Code:

(require 'cl)
(require 'sgml-mode)
(require 'skewer-mode)

(defvar skewer-html-options-selector "body"
  "Default CSS selector to be used when evaluating HTML.")

(defvar skewer-html-options-append t
  "True if skewer-html should append HTML by default.")

;; Selector computation

(defun skewer-html--tag-name-cleanup (name)
  "Cleanup tag names provided by sgml-mode."
  (replace-regexp-in-string "/$" "" name))

(defun skewer-html-compute-tag-ancestry ()
  "Compute the ancestry chain at point."
  (save-excursion
    (nreverse
     (loop for next = (sgml-get-context)
           while next
           collect (skewer-html--tag-name-cleanup
                    (sgml-tag-name (car (last next))))))))

(defun skewer-html--tag-after-point ()
  "Return the tag struct for the tag immediately following point."
  (save-excursion
    (forward-char 1)
    (sgml-parse-tag-backward)))

(defun skewer-html-compute-tag-nth ()
  "Compute the position of this tag within its parent."
  (save-excursion
    (let ((start (sgml-tag-name (car (last (sgml-get-context)))))
          (stop (save-excursion (sgml-get-context) (point))))
      (loop with n = 1
            do (sgml-skip-tag-backward 1)
            while (> (point) stop)
            when (equal start (sgml-tag-name (skewer-html--tag-after-point)))
            do (incf n)
            finally (return n)))))

(defun skewer-html-compute-selector ()
  "Compute the selector for exactly the tag around point."
  (let ((ancestry (skewer-html-compute-tag-ancestry))
        (nth (skewer-html-compute-tag-nth)))
    (format "%s:nth-of-type(%d)" (mapconcat 'identity ancestry " > ") nth)))

;; Helpers

(defun skewer-html-get-eval-options (&optional prefix)
  "Return options for eval. Maybe prompt a user, if prefixed is true."
  (values
   (if prefix
       (read-string "Selector: ")
     skewer-html-options-selector)
   (if prefix
       (y-or-n-p "Append?")
     skewer-html-options-append)))

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

(defun skewer-html-eval (string selector append)
  "Load HTML into a selector, optionally appending."
  (skewer-eval string nil :type "html" :extra `((selector . ,selector)
                                                (append   . ,append))))

(defun skewer-html-eval-region (&optional prefix)
  "Load HTML from region or buffer. When prefixed, prompt for options."
  (interactive "P")
  (multiple-value-bind (selector append) (skewer-html-get-eval-options prefix)
    (let* ((region-active (region-active-p))
           (beg (if region-active (region-beginning) (point-min)))
           (end (if region-active (region-end) (point-max)))
           (region (buffer-substring-no-properties beg end)))
      (skewer-flash-region beg end) ; check region
      (skewer-html-eval region selector append))))

(defun skewer-html-eval-tag (&optional prefix)
  "Load HTML from the surrounding tag. When prefixed, prompt for options."
  (interactive "P")
  (save-excursion
    (multiple-value-bind (selector append) (skewer-html-get-eval-options prefix)
      (let* ((beg (progn (sgml-skip-tag-backward 1) (point)))
             (end (progn (sgml-skip-tag-forward 1) (point)))
             (region (buffer-substring-no-properties beg end)))
        (skewer-flash-region beg end)
        (skewer-html-eval region selector append)))))

;; Minor mode definition

(defvar skewer-html-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-M-x") 'skewer-html-eval-tag)
      (define-key map (kbd "C-c C-k") 'skewer-html-eval-region)
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
