(require 'shell)
(add-hook 'shell-mode-hook 'my-on-shell)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun shell-mode-up () (interactive)
  (if (comint-after-pmark-p)
      (comint-previous-input 1)
    (prev-line 1)))

(defun shell-mode-down () (interactive)
  (if (comint-after-pmark-p)
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
  )
;; note: can still use ctrl-n ctrl-p for regular movement

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

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
    (let ((buffer-read-only nil))
      (while (re-search-forward "/tmp/trunk.graehl/trunk/" nil t)
        (replace-match "~/t/")))))

(provide 'setup-shell-mode)
