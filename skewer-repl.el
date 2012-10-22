(require 'skewer-mode)

(defvar skewer-repl-prompt "js> ")

(defun skewer-repl-process ()
  (get-buffer-process (current-buffer)))

(define-derived-mode skewer-repl-mode comint-mode "js-REPL"
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote skewer-repl-prompt)))
  (setq comint-input-sender 'skewer-input-sender)
  (add-hook 'skewer-post-hook 'skewer-post-repl)
  (unless (comint-check-proc (current-buffer))
    (start-process "ielm" (current-buffer) "hexl")
    (set-process-query-on-exit-flag (skewer-repl-process) nil)
    (end-of-buffer)
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (skewer-repl-process) skewer-repl-prompt)
    (set-process-filter (skewer-repl-process) 'comint-output-filter)))

(defun skewer-input-sender (proc input)
  (skewer-eval input))

(defun skewer-post-repl (result)
  (let ((buffer (get-buffer "*skewer-repl*"))
        (output (cdr (assoc 'value result))))
    (when buffer
      (with-current-buffer buffer
        (comint-output-filter (skewer-repl-process)
                              (concat output "\n" skewer-repl-prompt))))))

(defun skewer-repl ()
  (interactive)
  (when (not (get-buffer "*skewer-repl*"))
    (with-current-buffer (get-buffer-create "*skewer-repl*")
      (skewer-repl-mode)))
  (switch-to-buffer (get-buffer "*skewer-repl*")))

(provide 'skewer-repl)

;;; skewer-repl.el ends here
