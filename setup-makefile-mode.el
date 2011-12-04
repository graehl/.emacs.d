(defun my-makefile-mode-hook ()
  (font-lock-mode t)
  (show-paren-mode t)
  (setq indent-tabs-mode t)  ; Makefiles actually _need_ tabs :(
  (local-set-key [( control ?\( )] 'my-matching-paren)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(control return)] 'newline)
  )

(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

(provide 'setup-makefile-mode)
