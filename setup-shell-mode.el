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

(provide 'setup-shell-mode)
