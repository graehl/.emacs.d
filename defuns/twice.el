(defun try-twice (fn)
  (unless (ignore-errors (or (funcall fn) t))
    (funcall fn)))

(provide 'twice)
