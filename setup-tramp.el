(custom-set-variables
           '(tramp-default-method "ssh")
           '(tramp-default-user "graehl")
           '(tramp-default-host "c-graehl")
           '(vc-handled-backends '(Git))
           '(tramp-completion-reread-directory-timeout nil)
           '(tramp-persistency-file-name .tramp-persistency)
           )
;;(setq vc-handled-backends '(Git))
;;(setq tramp-completion-reread-directory-timeout nil)
;;tramp-persistency-file-name
(provide 'setup-tramp)
