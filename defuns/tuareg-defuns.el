(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(defun tuareg-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "\\`\\(.*\\)\\.fs\\(i\\)?\\'" name)
        (find-file (concat (tuareg-match-string 1 name)
                           (if (match-beginning 2) ".fs" ".fsi"))))))

(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)


(defun tuareg-fsc () "fast scala compiler"
  (set (make-local-variable 'compile-command)
                  (concat "fsc \""
                          (file-name-nondirectory buffer-file-name)
                          "\"")))

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-hook 'tuareg-mode-hook 'tuareg-fsc)
