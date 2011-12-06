;; Autopair parens

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
(setq autopair-blink nil) ;; no no no! NO BLINKING! NOOO!

(defun autopair-dont ()
  (interactive)
  (setq autopair-dont-activate t))

;; Don't autopair lisp
(add-hook 'emacs-lisp-mode-hook 'autopair-dont)
(add-hook 'lisp-interaction-mode 'autopair-dont)

(require 'setup-code-modes)
(install-hooks lisp-modes-hook 'autopair-dont)

(set-default 'autopair-dont-activate #'(lambda ()
                                         (eq major-mode 'term-mode)))
(provide 'setup-autopair)
