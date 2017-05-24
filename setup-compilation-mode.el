;; compile- + - one of these is killing my ctrl-o binding in global map.
;;(require 'compile-)
(require 'compile)
;;(require 'compile+)
(require 'compile-defuns)
(require 'cmake-mode)

(defun my-compilation-mode-hook ()
  (define-key compilation-mode-map (kbd "C-o") 'other-window)
  (when (eq major-mode 'compilation-mode) ; exclude grep/ag
  ;;  (setq truncate-lines nil)
  ;;  (add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
  ;; (global-set-key (kbd "C-o") 'other-window)
  ;;(setq bidi-paragraph-direction 'left-to-right)
  ))
                                        ; Don't truncate lines in the compilation window
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;;(add-to-list 'compilation-error-regexp-alist '("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2))
;;(add-to-list 'compilation-error-regexp-alist '("^ + [0-9]+>\\([^(]+\\)(\\([0-9]+\\)): [^ ]" 1 2))

;; '(compilation-error-regexp-alist (quote (("^ + [0-9]+>\\([^(>]+\\)(\\([0-9]+\\)): [^ ]" 1 2) ("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2) absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint)))

(require 'compile-defuns)

(add-hook 'c-mode-hook 'my-c-mode-compile-dwim)
(add-hook 'c++-mode-hook 'my-c++-mode-compile-dwim)
(add-hook 'latex-mode-hook 'my-latex-mode-compile-dwim)

;;(add-hook 'compilation-finish-functions #'qtmstr-compile-finish)
;;(add-hook 'compilation-mode-hook #'qtmstr-setup-compile-mode)

 ;; i don't like this - it forces a dedicated frame
(remove-hook 'compilation-finish-functions 'fit-1-window-frames-on)

(setq compilation-skip-threshold 1)
;;(setq compilation-skip-threshold 2)
;;(setq next-error-recenter (quote (4)))

(setq gr-dedicated-compilation-frame nil)
(setq compilation-frame-spec '("*compilation*" (minibuffer . nil) (unsplittable . t) (menu-bar-lines . 0)))

(setq fit-frame-max-width-percent 40)
(setq fit-frame-max-height-percent 75)
(if gr-on-24-3 (progn
                 (setq display-buffer-alist nil)
                 (if gr-dedicated-compilation-frame
                     (pushnew compilation-frame-spec display-buffer-alist)
                   (setq special-display-buffer-names (remove compilation-frame-spec display-buffer-alist))))
  (if gr-dedicated-compilation-frame
      (pushnew compilation-frame-spec special-display-buffer-names)
    (setq special-display-buffer-names (remove compilation-frame-spec special-display-buffer-names)))
  )

(setq growl-enabled nil)

(defun growl (title message)
  (when (and growl-enabled gr-on-mac)
    (start-process "growl" " growl" "growlnotify" title "-a" "Emacs")
    (process-send-string " growl" message)
    (process-send-string " growl" "\n")
    (process-send-eof " growl")))

(defun default-local-compile (command &optional comint)
  (let ((old-ddir default-directory))
    (setq default-directory "~/x")
    (compile command comint)
  (setq default-directory old-ddir)
    ))

(setq local-compile-command compile-command)

(defun local-compile (command &optional comint)
  (interactive
   (list
    (let ((command (eval local-compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    (consp current-prefix-arg)))
  (unless (equal command (eval local-compile-command))
    (setq local-compile-command command))
(if (and (called-interactively-p 'any)
         (file-remote-p default-directory))
         (default-local-compile command comint)
         (compile command comint))
  )

(defun growl-compilation-result(buffer msg)
  (when (string-match "^*com" (buffer-name buffer))
    (if (string-match "^finished" msg)
        (progn
          (growl "Emacs compilation" "Compilation Successful :-)"))
      (growl "Emacs compilation" "Compilation Failed :-("))))

(add-hook 'compilation-finish-functions 'growl-compilation-result)

;;; copy from compile.el to avoid hooks
(defun compile-no-hooks (command &optional comint)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

If optional second arg COMINT is t the buffer will be in Comint mode with
`compilation-shell-minor-mode'.

Interactively, prompts for the command if the variable
`compilation-read-command' is non-nil; otherwise uses `compile-command'.
With prefix arg, always prompts.
Additionally, with universal prefix arg, compilation buffer will be in
comint mode, i.e. interactive.

To run more than one compilation at once, start one then rename
the `*compilation*' buffer to some other name with
\\[rename-buffer].  Then _switch buffers_ and start the new compilation.
It will create a new `*compilation*' buffer.

On most systems, termination of the main compilation process
kills its subprocesses.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint))

(provide 'setup-compilation-mode)
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
