;;(mapcar (lambda (x) (add-hook x 'turn-on-paredit)) lisp-modes)
(defun turn-on-paredit () (interactive) (paredit-mode 1))
(require 'paredit)
(provide 'setup-paredit)
