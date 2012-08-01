(require 'coding-hooks)

(defun my-code-mode-hook ()
  (show-paren-mode t)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (local-set-key (kbd "C-<return>") 'newline)
  (local-set-key (kbd "C-(") 'my-matching-paren)
  (make-local-variable 'dabbrev-case-fold-search)
  (setq dabbrev-case-fold-search nil)
  )

(install-hook 'coding-hooks 'my-code-mode-hook)

;;sets e.g. c-modes c-modes-hook
;; suggestion: either (for all) (install-hook 'coding-hooks X) or (install-hooks lisp-mode-hooks X)
(set-coding-modes 'c js js2 c idl c++ cc java go)
(set-coding-modes 'lisp scheme lisp emacs-lisp lisp-interaction)
(set-coding-modes 'jvm java scala clojure tuareg)
(set-coding-modes 'ml ocaml ml haskell tuareg)
(set-coding-modes 'make make cmake makefile makefile-gmake jam fundamental conf conf-unix change-log)
(set-coding-modes 'doc LaTeX html)

(require 'python)
(install-coding-hooks)
(provide 'setup-code-modes)
