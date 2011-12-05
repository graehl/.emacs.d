(defun my-makefile-mode-hook ()
  (setq indent-tabs-mode t)  ; Makefiles actually _need_ tabs :(
  ;;  (local-set-key [( control ?\( )] 'my-matching-paren)
  (local-set-key [return] 'newline-and-indent)
  ;;(local-set-key [(control return)] 'newline)
  )

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(require 'setup-code-modes)
(install-hooks make-modes-hook 'my-makefile-mode-hook)
(provide 'setup-makefile-mode)
