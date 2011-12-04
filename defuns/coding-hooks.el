(setq coding-hooks '())
;(defun run-coding-hooks () (map 'funcall coding-hooks))
(defun run-coding-hooks () (loop for h in coding-hooks do (funcall h)))
;(add-hook 'coding-hooks '(lambda () (message "hi")))

(setq c-modes-hook '(c-mode-hook idl-mode-hook c++-mode-hook cc-mode-hook java-mode-hook c-mode-common-hook))
(setq lisp-modes-hook '(lisp-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook))
(setq make-modes-hook '(makefile-mode-hook))
(setq interp-modes-hook '(perl-mode-hook python-mode-hook sh-mode-hook))
(setq all-code-modes-hook (append c-modes-hook lisp-modes-hook make-modes-hook interp-modes-hook '(LaTeX-mode)))
(loop for h in all-code-modes-hook
      do (add-hook h 'run-coding-hooks))

