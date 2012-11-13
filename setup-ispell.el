(if (file-exists-p "/usr/local/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(setq ispell-program-name "aspell")
(ispell-change-dictionary "english")
(provide 'setup-ispell)
