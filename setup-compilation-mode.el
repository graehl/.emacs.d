(require 'cmake-mode)
(require 'compile)
(require 'compile-defuns)

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil)
  (add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
)
                                        ; Don't truncate lines in the compilation window
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(add-to-list 'compilation-error-regexp-alist '("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2))
(setq debug-on-error t)

(provide 'setup-compilation-mode)
