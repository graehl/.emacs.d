(require 'ag)
(setq gr-have-ag t)
(defun gr-ag-cd (pattern &optional directory)
  (interactive "sGrep Literal String: ")
    (message "shell-file-name %s " shell-file-name)
    (let ((null-device "/dev/null"))
    (ag/search pattern (or directory default-directory)))
  )

(setq ag-highlight-search t)

(provide 'setup-ag)
