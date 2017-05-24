(require 'shell)

;;(setq ansi-color-for-comint-mode nil)

(add-hook 'shell-mode-hook 'my-on-shell)

;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

(require 'ansi-color)
(setq ansi-color-for-comint-mode 'filter)

(defun my-comint ()
  (interactive)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-prompt-read-only t)
  (setq comint-get-old-input (lambda () ""))
  )

(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun my-after-prompt-p () (interactive)
  (and (eq major-mode 'shell-mode) (comint-after-pmark-p)))

(defun shell-mode-up () (interactive)
  (if (my-after-prompt-p)
      (comint-previous-input 1)
    (prev-line 1)))

(defun shell-mode-down () (interactive)
  (if (my-after-prompt-p)
      (comint-next-input 1)
    (forward-line 1)))

(defun my-on-shell () (interactive)
                                        ; (define-key shell-mode-map [up] 'ewd-comint-up)
                                        ; (define-key shell-mode-map [down] 'ewd-comint-down)
  (define-key shell-mode-map [(control k)] 'comint-kill-input)
  (local-set-key [C-a] 'comint-bol)
                                        ; (local-set-key [home] 'comint-bol) ; move to beginning of line, after prompt

                                        ; use history if at prompt


  (local-set-key [up] 'shell-mode-up)
  (local-set-key [down] 'shell-mode-down)
  (my-comint)
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-to-bottom-on-output nil)
  (setq comint-scroll-show-maximum-output t)
  ;;(setq comint-prompt-read-only nil)
  (setq comint-get-old-input (lambda () ""))
  )
;; note: can still use ctrl-n ctrl-p for regular movement


;; (require 'multi-term)
;; (setq multi-term-program "/bin/bash")

(defun open-localhost ()
  (interactive)
  (ansi-term "bash" "localhost"))

(defun my-change-tmp-to-nfs (buffer &optional stat)
  "change tmp to nfs"
  (interactive "b")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "/tmp/trunk.graehl/trunk/" nil t)
        (replace-match "~/t/")))))

(defvar my-local-shells
  '("*shell*" "*shell1*" "*shell2*" "*shell0*"))
(defvar my-remote-shells
  '())
(defvar my-shells (append my-local-shells my-remote-shells))

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(when nil
;; this is nice but something is breaking my eol keybind when i type too fast
(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))
)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
;; (when (< (emacs-version-major) 24) (load "comint.el.gz"))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun default-local-shell ()
  (interactive)
  (let ((old-ddir default-directory))
  (setq default-directory "~")
  (shell-no-hooks "*local-shell*")
  (setq default-directory old-ddir)
  ))

(defun local-shell ()
  (interactive)
  (if (file-remote-p default-directory)
      (default-local-shell-mode)
    (shell)))

;;; copy of shell.el shell fn to avoid hooks
(defun shell-no-hooks (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (prog1
	     (read-buffer "Shell buffer: "
			  ;; If the current buffer is an inactive
			  ;; shell buffer, use it as the default.
			  (if (and (eq major-mode 'shell-mode)
				   (null (get-buffer-process (current-buffer))))
			      (buffer-name)
			    (generate-new-buffer-name "*shell*")))
	   (if (file-remote-p default-directory)
	       ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
	       (setq default-directory
		     (expand-file-name
		      (read-directory-name
		       "Default directory: " default-directory default-directory
		       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
	   (file-remote-p default-directory)
	   (null explicit-shell-file-name)
	   (null (getenv "ESHELL")))
      (with-current-buffer buffer
	(set (make-local-variable 'explicit-shell-file-name)
	     (file-remote-p
	      (expand-file-name
	       (read-file-name
		"Remote shell path: " default-directory shell-file-name
		t shell-file-name))
	      'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (pop-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
		     (getenv "ESHELL") shell-file-name))
	   (name (file-name-nondirectory prog))
	   (startfile (concat "~/.emacs_" name))
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
	(setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
	     (if (file-exists-p startfile) startfile)
	     (if (and xargs-name (boundp xargs-name))
		 (symbol-value xargs-name)
	       '("-i")))
      (shell-mode)))
  buffer)

(provide 'setup-shell-mode)
