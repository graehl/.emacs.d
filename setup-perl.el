(defvar perl-lint--script
  (concat (if load-file-name
              (file-name-directory load-file-name)
            default-directory) "perl-lint.pl"))

(defun perl-lint ()
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "This buffer is not related to file."))
   (let ((cmd (format "perl %s %s" perl-lint--script (file-name-nondirectory file))))
     (compile cmd))))

(provide 'setup-perl)
