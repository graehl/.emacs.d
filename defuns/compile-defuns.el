(defmacro compilation-recenter-end-with-selected-window (window &rest body)
  (if (eval-when-compile (fboundp 'with-selected-window))
      `(with-selected-window ,window ,@body)
    `(save-selected-window
       (select-window ,window)
       ,@body)))
(put 'compilation-recenter-end-with-selected-window 'lisp-indent-function 1)

(defun compilation-recenter-end-at-finish (buffer string)
  (dolist (window (get-buffer-window-list buffer))
    (compilation-recenter-end-with-selected-window window
      (when (<= (count-lines (point) (point-max)) 2)
        (save-excursion
          (goto-char (point-max))
          (recenter -1))))))
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)

(defun my-end-of-current-compilation-buffer()
  (if (equal (buffer-name) "*compilation*")
      (bufend)))

(defun my-compile(&optional command)
  (interactive)
  (if (interactive-p)
      (call-interactively 'compile)
    (compile command))
  (save-excursion
    (pop-to-buffer "*compilation*")
    (bufend))
  ;; force scrolling despite save-excursion
  (my-end-of-current-compilation-buffer))

(defun my-buffer-exists (buffer)
  "Return t if the buffer exists.
buffer is either a buffer object or a buffer name"
  (bufferp (get-buffer buffer)))

(defun my-recompile ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (if (and (my-buffer-exists "*compilation*")
           compile-command)
      (save-excursion
        ;; switching to the compilation buffer here causes the compile command to be
        ;; executed from the same directory it originated from.
        (pop-to-buffer "*compilation*")
        (recompile)
        (pop-to-buffer "*compilation*")
        (bufend))
    ;; else
    (call-interactively 'my-compile))
  ;; force scrolling despite save-excursion
  (my-end-of-current-compilation-buffer))

(defun compilation-always-restart ()
  (interactive)
  (defadvice yes-or-no-p (around compilation-always-kill activate)
    "Minor mode for `compile' to always kill existing compilation."
    (if (and (boundp 'compilation-always-kill-mode) ;; in case `unload-feature'
             compilation-always-kill-mode
             (string-match "A compilation process is running; kill it\\?"
                           prompt))
        (setq ad-return-value t)
      ad-do-it)))

(defun c-default-compile ()
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -c -o %s.o %s %s %s"
                   (or (getenv "CC") "g++")
                   (file-name-sans-extension file)
                   (or (getenv "CPPFLAGS") "-DDEBUG=9")
                   (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                   file)))))
;;(add-hook 'c-mode-hook 'c-default-compile)


;; not used for anything. not tested. could put a flag in modeline. one-file no-makefile scripts usually compile pretty fast, though.
(defun my-buffer-compiled-since-modified-p (buffer)
  "Is file modification for BUFFER newer than last compilation time? (not perfect at midnight etc)"
  (interactive (list (current-buffer)))
  (unless (bufferp (get-buffer "*compilation*"))
    (error "No file associated with buffer"))
  (let ((mtime (nth 5 (file-attributes (buffer-file-name buffer))))
        (compile-start
         (save-excursion
           (with-current-buffer (get-buffer "*compilation*")
             (goto-char (point-min))
             (search-forward "Compilation started at "
                             nil 'noerror)
             (apply 'encode-time
                    ;; Anything missing in the string,
                    (mapcar
                     (lambda (a)
                       (if (null (car a)) (cadr a) (car a)))
                     ((lambda (&rest args)
                        "Transpose list of ARGS into one list."
                        (mapcar
                         (lambda (n)
                           (delq nil
                                 (mapcar
                                  (lambda (arg) (nth n arg))
                                  args)))
                         (let ((len (apply 'max
                                           (mapcar 'length args))))
                           (number-sequence 0 (1- len)))))
                      (parse-time-string
                       (buffer-substring-no-properties
                        (point)
                        (progn
                          (forward-line 1)
                          (forward-char -1)
                          (point))))
                      ;; fold in from current time.
                      (decode-time))))))))
    (if (time-less-p compile-start mtime)
        (prog1 nil
          (if (buffer-modified-p)
              (message "Not compiled since last save (modified)")
            (message "Not compiled since last save")))
      (prog1 t
        (if (buffer-modified-p)
            (message "Compiled after last save (modified)")
          (message "Compiled after last save"))))))

(provide 'compile-defuns)
