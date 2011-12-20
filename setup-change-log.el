(require 'hooks-defuns)
(require 'buffer-defuns)
(provide 'setup-change-log)
(defun cleanup-change-log ()
  (interactive)
  (with-whole-buffer
   (replace-string "../r" "racerx")))
;(add-hook HOOK FUNCTION &optional APPEND LOCAL) - so make-local-hook removed in emacs 24.
(defun setup-change-log-hook ()
;  (make-local-hook 'before-save-hook)
  (install-hook 'before-save-hook 'cleanup-change-log nil t))
(install-hook 'change-log-mode-hook 'setup-change-log-hook)
