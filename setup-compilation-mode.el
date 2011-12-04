(defun my-compilation-mode-hook ()
  (setq truncate-lines nil))
                                        ; Don't truncate lines in the compilation window
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(provide 'setup-compilation-mode)
