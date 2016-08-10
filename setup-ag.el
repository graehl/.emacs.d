(require 'ag)
(setq gr-have-ag t)
(defun gr-ag-cd (pattern &optional directory)
  (interactive "sGrep Literal String: ")
    (message "shell-file-name %s " shell-file-name)
    (let ((null-device "/dev/null"))
    (ag/search pattern (or directory default-directory)))
  )

(defun gr-ag-search-case (string directory &optional regexp)
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments (if regexp
                       ag-arguments
                     (cons "--literal" (cons "--case-sensitive" ag-arguments)))))
    (if ag-highlight-search
        (setq arguments (append '("--color" "--color-match" "'30;43'") arguments))
      (setq arguments (append '("--nocolor") arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (compilation-start
     (ag/s-join " "
                (append '("ag") arguments (list "--agignore" "~/.agignore" (ag/shell-quote string))))
     'ag-mode)))

(defun gr-ag-cd-case (pattern &optional directory)
  (interactive "sGrep (case-sensitive) Literal String: ")
    (message "shell-file-name %s " shell-file-name)
    (let ((null-device "/dev/null"))
    (gr-ag-search-case pattern (or directory default-directory))))

(setq ag-highlight-search t)

(provide 'setup-ag)
