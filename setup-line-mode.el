                                        ;Users may configure this behavior via the (new) `line-move-visual' customization variable. Set it to t for unconditional visual movement of all keys, and nil for unconditional logical (buffer) movement of all keys.

                                        ;(setq word-wrap t)
(global-visual-line-mode t)
(defvar global-line-move-visual line-move-visual)
(defun regular-line-move () (setq line-move-visual global-line-move-visual))
(regular-line-move)
(add-hook 'iswitchb-minibuffer-setup-hook
          (lambda ()
            (setq line-move-visual nil)))
(add-hook 'minibuffer-exit-hook 'regular-line-move)

                                        ;(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators t)

(provide 'setup-line-mode)

(defun srb-adaptive-indent (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))
        (put-text-property lbp lep 'wrap-prefix (fill-context-prefix lbp lep))
        (search-forward "\n" end t))))

(defvar srb-adaptive-wrap-no-fringe nil)

(define-minor-mode srb-adaptive-wrap-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  (save-excursion
    (save-restriction
      (widen)
      (let ((buffer-undo-list t)
            (inhibit-read-only t)
            (mod (buffer-modified-p)))
        (if srb-adaptive-wrap-mode
            (progn
              (when srb-adaptive-wrap-no-fringe
                (setq word-wrap t)
                (unless (member '(continuation) fringe-indicator-alist)
                  (push '(continuation) fringe-indicator-alist)))
              (jit-lock-register 'srb-adaptive-indent))
          (jit-lock-unregister 'srb-adaptive-indent)
          (remove-text-properties (point-min) (point-max) '(wrap-prefix pref))
          (when srb-adaptive-wrap-no-fringe
            (setq fringe-indicator-alist
                  (delete '(continuation) fringe-indicator-alist))
            (setq word-wrap nil)))
        (restore-buffer-modified-p mod)))))

(srb-adaptive-wrap-mode t)
