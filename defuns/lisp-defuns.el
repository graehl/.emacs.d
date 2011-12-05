;; Lisp specific defuns

;; return first result, perform rest no matter if first fails
(defmacro safe-wrap-cleanup (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defmacro safe-wrap (&rest body)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

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
