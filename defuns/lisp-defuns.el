;; Lisp specific defuns

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-unedebug-defun ()
  "I can't believe emacs doesn't give you a way to do this!!"
  (interactive t)
  (eval-expression (edebug-read-top-level-form)))
