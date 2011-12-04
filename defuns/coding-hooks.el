(setq coding-hooks '())
                                        ;(defun run-coding-hooks () (map 'funcall coding-hooks))
(defun run-coding-hooks () (loop for h in coding-hooks do (funcall h)))
                                        ;(add-hook 'coding-hooks '(lambda () (message "hi")))

(setq c-modes-hook '(js-mode-hook js2-mode-hook c-mode-hook idl-mode-hook c++-mode-hook cc-mode-hook java-mode-hook c-mode-common-hook go-mode-hook))
(setq lisp-modes-hook '(scheme-mode-hook lisp-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook))
(setq jvm-modes-hook '(java-mode-hook scala-mode-hook clojure-mode-hook tuareg-mode-hook))
(setq ml-modes-hook '(ocaml-mode-hook ml-mode-hook haskell-mode-hook tuareg-mode-hook))
(setq make-modes-hook '(make-mode-hook cmake-mode-hook makefile-mode-hook jam-mode-hook))
(setq doc-modes-hook '(LaTeX-mode-hook html-mode-hook))
(setq script-modes-hook '(perl-mode-hook ruby-mode-hook python-mode-hook sh-mode-hook))
(setq all-code-modes-hook (append ml-modes-hook jvm-modes-hook script-modes-hook c-modes-hook lisp-modes-hook make-modes-hook script-modes-hook doc-modes-hook))
(loop for h in all-code-modes-hook do (add-hook h 'run-coding-hooks))
