(require 'cl)
(require 'bytecomp)

;; byte-recompile-file:
;;
;;  - in Emacs23 will not recompile a file when the source is newer than the
;;    bytecode (.elc)
;;
;;  - in Emacs24 has another different and unhelpful behavior:
;;
;;    If the `.elc' file does not exist, normally this function *does not*
;;    compile FILENAME. If ARG is 0, that means compile the file even if it
;;    has never been compiled before.
;;
;; so we just define our own
(defun make-byte-compile-file (el)
  "Byte compile the EL file, and skips unnecessary compilation.
Specifically, if the compiled elc file already exists and is
newer, then compilation is skipped."
  (interactive "s")
  (let ((elc (concat (file-name-sans-extension el) ".elc"))
	;; Byte-compile runs emacs-lisp-mode-hook; disable it
	emacs-lisp-mode-hook byte-compile-warnings)
    (when (or (not (file-exists-p elc))
	      (file-newer-than-file-p el elc))
      (condition-case err
	  (byte-compile-file el)
	((debug error) ;; catch-all, allow for debugging
	 (message "%S" (error-message-string err)))))))

(defun make-byte-compile-file-or-directory (file)
  "Byte-compile FILE or all files within it if it is a directory."
  (interactive "s")
  (let ((byte-compile-warnings nil)
        ;; Byte-compile runs emacs-lisp-mode-hook; disable it
        emacs-lisp-mode-hook)
    (if (file-directory-p file)
        (make-byte-compile-file-or-directory file 0)
      (make-byte-compile-file file))))

(provide 'make-byte-compile)
