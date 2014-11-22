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


(require 'multi-term)
(setq multi-term-program "/bin/bash")
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

(provide 'setup-shell-mode)
