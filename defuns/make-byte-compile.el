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
  (interactive "sElisp file to compile: ")
  (let ((elc (concat (file-name-sans-extension el) ".elc"))
	;; Byte-compile runs emacs-lisp-mode-hook; disable it
	emacs-lisp-mode-hook byte-compile-warnings)
    (when (or (not (file-exists-p elc))
	      (file-newer-than-file-p el elc))
      (condition-case err
	  (byte-compile-file el)
	((debug error) ;; catch-all, allow for debugging
	 (message "%S" (error-message-string err)))))))

(defun walk-path-nosymlink (dir action &optional symlink)
  "walk DIR executing (action dir path) - may fall prey to link-cycles if symlink is t.
 action is called on dirs and files;
 action return t if you want to recurse.
 action is called on root if it's a file, but not called on root dir
"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t)) fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (or symlink (not (file-symlink-p fullname)))
                         (walk-path-nosymlink fullname action symlink)))))))
        (t
         (funcall action
                  (file-name-wdirectory dir)
                  (file-name-nondirectory dir)))))

(defun string-suffix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (let ((l1 (length str1)) (l2 (length str2)))
    (and (>= l2 l1)
         (eq t (compare-strings str1 nil nil
                                str2 (- l2 l1) nil ignore-case)))))

(if (not (fboundp 'file-not-autosave))
    (defun file-not-autosave (path)
      (and
       (not (string-match "#$" path))
       (not (string-match ".#" path))
       )))

(defun make-byte-compile-ext (el &optional extension)
  (interactive "f")
  (if (eq nil extension) (setq extension ".el"))
  (when (and (file-not-autosave el) (string-suffix-p extension el))
    (message (format "make-byte-compile %s ext=%s" el extension))
    (make-byte-compile-file el)))

(defun make-byte-compile-el-visitor (dir file)
  (make-byte-compile-ext (concat dir file) ".el"))

(defun make-byte-compile-file-or-directory (file &optional symlink)
  "Byte-compile FILE or all files within it if it is a directory."
  (interactive "sElisp directory or file to byte-compile: ")
  (if (file-regular file)
      (make-byte-compile-el file)
    (walk-path-nosymlink file 'make-byte-compile-el symlink)))

(defun make-byte-compile-directory (dir &optional symlink)
  (interactive "sElisp directory to byte-compile: ")
  (walk-path-nosymlink dir 'make-byte-compile-el-visitor symlink))

(provide 'make-byte-compile)
