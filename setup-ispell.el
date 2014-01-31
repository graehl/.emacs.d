(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-program-name "aspell")
(when nil (if (file-exists-p "/usr/local/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))
)

(setq ispell-program-name "aspell")
(ispell-change-dictionary "english")
(provide 'setup-ispell)
