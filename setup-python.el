(require 'python-mode)

(setq py-keys (list
               (cons (kbd "S-<f10>") 'py-pychecker-run)
               (cons (kbd "<M-left>") 'py-shift-left)
               (cons (kbd "<M-right>") 'py-shift-right)
               ))
(defun define-py-keys ()
  (mapcar (lambda (c) (list 'py-mode-map (car c) (cdr c))) py-keys))
(define-py-keys)

(defun gr-python ()

  (interactive)
  (switch-to-buffer-other-window
   (apply 'make-comint py-which-bufname py-which-shell nil py-which-args))
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'font-lock-defaults)
  (setq comint-prompt-regexp "^python% \\|^> \\|^(pdb) "
        font-lock-defaults '(python-shell-font-lock-keywords t))
  (add-hook 'comint-output-filter-functions 'py-comint-output-filter-function)
  (set-syntax-table py-mode-syntax-table)
  (use-local-map py-shell-map)
  (local-set-key "\C-a" 'comint-bol)
  (local-set-key "\C-c\C-a" 'beginning-of-line)
  (python-mode)
  (font-lock-mode))


(defun my-python-send-region (beg end)
  (interactive "r")
  (if (eq beg end)
      (python-send-region (point-at-bol) (point-at-eol))
    (python-send-region beg end)))

(defun my-python-send-region2 (&optional beg end)
  (interactive)
  (let ((beg (cond (beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (python-send-region beg end)))

(provide 'setup-python)
