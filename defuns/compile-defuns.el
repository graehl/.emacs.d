
(defun my-recompile-point-end ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (if (and (my-buffer-exists "*compilation*")
           compile-command)
      (save-excursion
        ;; switching to the compilation buffer here causes the compile command to be
        ;; executed from the same directory it originated from.
        (pop-to-buffer "*compilation*")
        (enlarge-window 20)
        (flet ((yes-or-no-p (&rest args) t)
               (y-or-n-p (&rest args) t))
          (recompile))
        ;;(font-lock-mode -1)
        ;;(setq truncate-lines t)
        ;;(toggle-word-wrap -1)
        (toggle-word-wrap 1)
        (pop-to-buffer "*compilation*")
        (bufend)
        (other-window 1)
        )
    ;; else
    (call-interactively 'my-compile))
  ;; force scrolling despite save-excursion
  ;; testing turning this off:
  (my-end-of-current-compilation-buffer))

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


(defconst gr-compile-buffername "*compilation*")

(defun my-end-of-current-compilation-buffer()
  (if (equal (buffer-name) gr-compile-buffername)
      (bufend)))

(defun my-compile(&optional command)
  (interactive)
  (save-last-compile)
  (let ((frame (selected-frame)))
    (if (interactive-p)
        (call-interactively 'compile)
      (compile command))
    (with-current-buffer gr-compile-buffername
      (bufend)
      (pop-to-buffer gr-compile-buffername)
      (bufend)
      (my-end-of-current-compilation-buffer)
      ;; force scrolling despite save-excursion
      (my-end-of-current-compilation-buffer)
      )
    (x-focus-frame frame)
    )
  )

(defun my-buffer-exists (buffer)
  "Return t if the buffer exists.
buffer is either a buffer object or a buffer name"
  (bufferp (get-buffer buffer)))

(defun my-really-recompile()
  (interactive)
  (let ((frame (selected-frame)))
    (with-current-buffer gr-compile-buffername
      (save-excursion
        ;; switching to the compilation buffer here causes the compile command to be
        ;; executed from the same directory it originated from.
        (pop-to-buffer gr-compile-buffername)
        (flet ((yes-or-no-p (&rest args) t)
               (y-or-n-p (&rest args) t))
          (recompile))
        (pop-to-buffer gr-compile-buffername)
        (bufend)
        (my-end-of-current-compilation-buffer)
        )
      )
    (x-focus-frame frame)
    ))

(defun my-recompile ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (save-last-compile)
  (if (and (my-buffer-exists gr-compile-buffername) compile-command)
      (my-really-recompile)
    ;; else
    (call-interactively 'my-compile))
  )

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
  (unless (bufferp (get-buffer gr-compile-buffername))
    (error "No file associated with buffer"))
  (let ((mtime (nth 5 (file-attributes (buffer-file-name buffer))))
        (compile-start
         (save-excursion
           (with-current-buffer (get-buffer gr-compile-buffername)
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

;;; makes < > into ( ) (syntactically) for parsing templates

(require 'cl)

(defun find-dedicated-frames (buf)
  (let (result)
    (dolist (window (get-buffer-window-list buf t) result)
      (let ((frame (window-frame window)))
        (when (frame-parameter frame 'unsplittable)
          (push frame result))))))

(defun qtmstr-setup-compile-mode ()
  ;; Support C++ better
  (modify-syntax-entry ?< "(")
  (modify-syntax-entry ?> ")")

  (dolist (frame (find-dedicated-frames (current-buffer)))
    (let ((orig (frame-parameter frame 'orig-background))
          (orig-fg (frame-parameter frame 'orig-foreground)))
      (when orig
        (modify-frame-parameters
         frame (list (cons 'background-color orig))))
      (when orig-fg
        (modify-frame-parameters
         frame (list (cons 'foreground-color orig-fg))))
      )))


(defun my-yes-or-mumble-p (prompt)
  "PROMPT user with a yes-or-no question, but only test for yes."
  (if (string= "yes"
               (downcase
                (read-from-minibuffer
                 (concat prompt "(yes or no) "))))
      t
    nil))


(defconst qtmstr-finished-color "#042414")
(defconst qtmstr-fail-color "#200015")

(defvar gr-compile-dedicated-frame nil)
(defun qtmstr-compile-finish (buf status)
  (with-current-buffer buf
    (message (format "qtmstr-compile-finish %s %s" buf status))
    (let* ((color (if (string-match "finished" status) ;;^finished\\b
                      qtmstr-finished-color
                    qtmstr-fail-color))
           found)
      (frame-parameter nil 'background-clor)
      (when gr-compile-dedicated-frame
        (dolist (frame (find-dedicated-frames buf))
          (setq found t)
          (modify-frame-parameters
           frame
           (list
            (cons 'orig-background (frame-parameter frame 'background-color))
            (cons 'orig-foreground (frame-parameter frame 'foreground-color))
            (cons 'background-color color)
            ))))

      (unless found
        (let ((overlay)
              (overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face (list :background color))
          (overlay-put overlay 'evaporate t))))))


;;; for compile-dwim
(defun my-c-mode-compile-dwim ()
  (setq get-buffer-compile-command
        (lambda (file)
          (cons (format "gcc -Wall -o %s %s && ./%s"
                        (file-name-sans-extension file)
                        file
                        (file-name-sans-extension file))
                11))))

(defun my-c++-mode-compile-dwim ()
  (setq get-buffer-compile-command
        (lambda (file)
          (cons (format "g++ -Wall -o %s %s && ./%s"
                        (file-name-sans-extension file)
                        file
                        (file-name-sans-extension file))
                11))))

(defun my-latex-mode-compile-dwim ()
  (setq get-buffer-compile-command
        (lambda (file) (format "pdflatex %s" file))))

(defun save-last-compile ()
  (interactive)
  (when (my-buffer-exists gr-compile-buffername)
    (with-current-buffer gr-compile-buffername
      (let ((text (buffer-substring (point-min) (point-max)))
            (inhibit-read-only t))
        (with-current-buffer (find-file-noselect "~/tmp/last.comp")
          (delete-region (point-min) (point-max))
          (insert text)
          (save-buffer))
        ))))


(defun end-of-line-nomark ()
  (interactive)
  (end-of-line)
)

(defun beginning-of-line-mark ()
  (interactive)
  (beginning-of-line)
  (mark)
)

(defmacro gr-save-focus (&rest body)
  `(let ((frame (selected-frame))
         (val (progn ,@body)))
     (x-focus-frame frame)
     val))

(defun my-next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (gr-save-focus
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    ))

(defun my-previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (gr-save-focus
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    ))

(provide 'compile-defuns)
