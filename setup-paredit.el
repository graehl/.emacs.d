;;(mapcar (lambda (x) (add-hook x 'turn-on-paredit)) lisp-modes)
(defun turn-on-paredit () (interactive) (paredit-mode 1))
(require 'paredit)
(require 'diminish)
(require 'elisp-slime-nav)
;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(provide 'setup-paredit)
