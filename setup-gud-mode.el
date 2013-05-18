;;(require 'gud)
(require 'setup-shell-mode)

(defun my-gud-run-to-cursor ()
  (gud-tbreak)
  (gud-cont))

;; Debugging
(defun my-gud-mode-hook ()
  (local-set-key [home]        ; move to beginning of line, after prompt
                 'comint-bol)
  (local-set-key [up]          ; cycle backward through command history
                 '(lambda () (interactive)
                    (if (comint-after-pmark-p)
                        (comint-previous-input 1)
                      (prev-line 1))))
  (local-set-key [down]        ; cycle forward through command history
                 '(lambda () (interactive)
                    (if (comint-after-pmark-p)
                        (comint-next-input 1)
                      (forward-line 1))))
  (local-set-key [f2] 'gud-cont)
  (local-set-key [f12] 'gud-step)
  (local-set-key [f10] 'gud-next)
  (local-set-key [f11] 'gud-finish)
  (local-set-key [(control f10)] 'my-gud-run-to-cursor)
  (local-set-key [f9] 'gud-break)
  (local-set-key [(shift f9)] 'gud-remove)
(setq comint-scroll-to-bottom-on-output 'this)
(my-comint)
)

;;(add-hook 'gud-mode-hook 'my-gud-mode-hook)

(require 'gdb-mi)

(setq gdb-non-stop-setting t)
(setq gdb-create-source-file-list nil)

(defun my-gdb-stopped-hook (record)
  (with-current-buffer (get-buffer-create "*debug-stop-log*")
    (insert
     (format "%s stopped in %s (%s)\n"
             (gdb-get-field record 'thread-id)
             (gdb-get-field record 'frame 'func)
             (gdb-get-field record 'reason)))))

(add-to-list 'gdb-stopped-hooks #'my-gdb-stopped-hook)

(provide 'setup-gud-mode)
