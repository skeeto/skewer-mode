;;; skewer-repl.el --- create a REPL in a visiting browser

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This is largely based on of IELM's code. Run `skewer-repl' to
;; switch to the REPL buffer and evaluate code.

;;; Code:

(require 'comint)
(require 'skewer-mode)

(defvar skewer-repl-prompt "js> "
  "Prompt string for JavaScript REPL.")

(defun skewer-repl-process ()
  "Return the process for the skewer REPL."
  (get-buffer-process (current-buffer)))

(defface skewer-repl-log-face
  '((((class color) (background light))
     :foreground "#77F")
    (((class color) (background dark))
     :foreground "#77F"))
  "Face for skewer.log() messages.")

(define-derived-mode skewer-repl-mode comint-mode "js-REPL"
  "Provide a REPL into the visiting browser."
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote skewer-repl-prompt)))
  (setq comint-input-sender 'skewer-input-sender)
  (add-to-list 'skewer-callbacks 'skewer-post-repl)
  (add-to-list 'skewer-callbacks 'skewer-post-log)
  (unless (comint-check-proc (current-buffer))
    (start-process "ielm" (current-buffer) "hexl")
    (set-process-query-on-exit-flag (skewer-repl-process) nil)
    (end-of-buffer)
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (skewer-repl-process) skewer-repl-prompt)
    (set-process-filter (skewer-repl-process) 'comint-output-filter)))

(defun skewer-input-sender (proc input)
  "REPL comint handler."
  (skewer-eval input 'skewer-post-repl t))

(defun skewer-post-repl (result)
  "Callback for reporting results in the REPL."
  (let ((buffer (get-buffer "*skewer-repl*"))
        (output (cdr (assoc 'value result))))
    (when buffer
      (with-current-buffer buffer
        (comint-output-filter (skewer-repl-process)
                              (concat output "\n" skewer-repl-prompt))))))

(defun skewer-post-log (log)
  "Callback for logging messages to the REPL."
  (let ((buffer (get-buffer "*skewer-repl*"))
        (output (propertize (cdr (assoc 'value log))
                            'font-lock-face 'skewer-repl-log-face)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (forward-line 0)
          (backward-char)
          (insert (concat "\n" output "")))))))

;;;###autoload
(defun skewer-repl ()
  "Start a JavaScript REPL to be evaluated in the visiting browser."
  (interactive)
  (when (not (get-buffer "*skewer-repl*"))
    (with-current-buffer (get-buffer-create "*skewer-repl*")
      (skewer-repl-mode)))
  (switch-to-buffer (get-buffer "*skewer-repl*")))

(provide 'skewer-repl)

;;; skewer-repl.el ends here
