
(defun my-sh-mode-hook ()
  (interactive "*")
                                        ;  (my-code-mode-hook)
                                        ;  (auto-fill-mode t)
                                        ;  (local-set-key [return] 'my-sh-newline-and-indent)
                                        ;  (local-set-key "{" 'my-sh-electric-open-brace)
                                        ;  (local-set-key [\S-\M-{] 'my-sh-electric-braces)
                                        ;  (setq fill-paragraph-function 'my-sh-fill-paragraph)
  ;; (local-set-key "}" 'my-sh-electric-close-brace)
  )

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

(provide 'setup-sh-mode)
