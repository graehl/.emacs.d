(require 'shell)
(add-hook 'shell-mode-hook 'my-on-shell)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun my-on-shell () (interactive)
                                        ;            (define-key shell-mode-map [up] 'ewd-comint-up)
                                        ;            (define-key shell-mode-map [down] 'ewd-comint-down)
  (define-key shell-mode-map [(control k)] 'comint-kill-input)
  (local-set-key [C-a] 'comint-bol)
                                        ;            (local-set-key [home] 'comint-bol)       ; move to beginning of line, after prompt
(local-set-key [up]          ; cycle backward through command history
               '(lambda () (interactive)
                  (if (comint-after-pmark-p)
                      (comint-previous-input 1)
                    (prev-line 1))))
(local-set-key [down]        ; cycle forward through command history
               '(lambda () (interactive)
                  (if (comint-after-pmark-p)
                      (comint-next-input 1)
                    (forward-line 1)))))


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

(require 'multi-term)
(setq multi-term-program "/bin/bash")
(defun open-localhost ()
  (interactive)
  (ansi-term "bash" "localhost"))

(require 'term)
(defun term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
        )
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l"))
      )))

(provide 'setup-shell-mode)
