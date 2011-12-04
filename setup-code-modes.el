
(defun my-code-mode-hook ()
  (show-paren-mode t)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(control return)] 'newline)
  (local-set-key [( control ?\( )] 'my-matching-paren)
  (make-local-variable 'dabbrev-case-fold-search)
  (setq dabbrev-case-fold-search nil)
  )

(add-hook 'coding-hooks 'my-code-mode-hook)

(provide 'setup-code-modes)
