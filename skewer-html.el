;;; skewer-html.el --- skewer support for live-interaction HTML -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This minor mode provides functionality for HTML like plain Skewer
;; does for JavaScript. There's no clean way to replace the body and
;; head elements of a live document, so "evaluating" these elements is
;; not supported.

;; * C-M-x   -- `skewer-html-eval-tag'

;; See also `skewer-html-fetch-selector-into-buffer' for grabbing the
;; page as it current exists.

;;; Code:

(require 'cl-lib)
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
        (cl-loop with start = (sgml-tag-name tag)
                 with stop = (save-excursion (sgml-get-context) (point))
                 with n = 1
                 do (sgml-skip-tag-backward 1)
                 while (> (point) stop)
                 when (equal start (sgml-tag-name
                                    (skewer-html--tag-after-point)))
                 do (cl-incf n)
                 finally (return n))))))

(defun skewer-html-compute-tag-ancestry ()
  "Compute the ancestry chain at point."
  (save-excursion
    (nreverse
     (cl-loop for nth = (skewer-html-compute-tag-nth)
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
                 (format "%s:nth-of-type(%d)" (cl-first tag) (cl-second tag)))
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
  (interactive "sSelector: ")
  (insert (skewer-html-fetch-selector selector)))

;; Evaluation

(defun skewer-html-eval (string ancestry &optional append)
  "Load HTML into a selector, optionally appending."
  (let ((ancestry* (cl-coerce ancestry 'vector)))  ; for JSON
    (skewer-eval string nil :type "html" :extra `((ancestry . ,ancestry*)
                                                  (append   . ,append)))))

(defun skewer-html-eval-tag ()
  "Load HTML from the immediately surrounding tag."
  (interactive)
  (let ((ancestry (skewer-html-compute-tag-ancestry)))
    (save-excursion
      ;; Move to beginning of opening tag
      (cl-loop for tag = (car (last (sgml-get-context)))
               while (and tag (eq 'close (sgml-tag-type tag))))
      (let* ((beg (progn (point)))
             (end (progn (sgml-skip-tag-forward 1) (point)))
             (region (buffer-substring-no-properties beg end)))
        (skewer-flash-region beg end)
        (if (= (length ancestry) 1)
            (error "Error: cannot eval body and head tags.")
          (skewer-html-eval region ancestry nil))))))

;; Minor mode definition

(defvar skewer-html-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-M-x") 'skewer-html-eval-tag)))
  "Keymap for skewer-html-mode")

;;;###autoload
(define-minor-mode skewer-html-mode
  "Minor mode for interactively loading new HTML."
  :lighter " skewer-html"
  :keymap skewer-html-mode-map
  :group 'skewer)

(provide 'skewer-html)

;;; skewer-html.el ends here
