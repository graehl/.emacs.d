(setq ispell-process-directory (expand-file-name "~/"))
;;(setq ispell-program-name "hunspell")
;;(setq ispell-dictionary "english")
;;(setq ispell-dictionary "en_US")
(add-to-list 'ispell-dictionary-alist '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
  ("-B")
  nil utf-8))
(provide 'setup-spell)
