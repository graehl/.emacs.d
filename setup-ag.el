(require 'ag)
(defun gr-ag-cd (pattern &optional directory)
  (interactive "sGrep Literal String: ")
  (ag/search pattern (or directory default-directory))
  )


(provide 'setup-ag)
