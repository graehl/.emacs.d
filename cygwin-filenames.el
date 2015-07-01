;; When in cygwin, allow C:\whatever to turn into /c/whatever
(defun gr-cygpath (p)
  (replace-regexp-in-string "\\\\" "/"
                            (replace-regexp-in-string "^\\([a-zA-Z]\\):" "/\\1" p t) t))

(defun cygwin-name-hook (operation &rest args)
  "Turn Windows filenames into Cygwin filenames."
  ;; Handle all operations the same
  (let ((first (car args))
        (inhibit-file-name-handlers
         (cons 'cygwin-name-hook
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation (cons (gr-cygpath first) (cdr args)))))

(add-to-list 'file-name-handler-alist '("^[a-zA-Z]:" . cygwin-name-hook))
