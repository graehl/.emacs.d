                                        ;Users may configure this behavior via the (new) `line-move-visual' customization variable. Set it to t for unconditional visual movement of all keys, and nil for unconditional logical (buffer) movement of all keys.

                                        ;(setq word-wrap t)
(when (fboundp 'global-visual-line-mode)
(global-visual-line-mode t)
(defvar global-line-move-visual t)
(defun regular-line-move () (setq line-move-visual global-line-move-visual))
(regular-line-move)
(add-hook 'iswitchb-minibuffer-setup-hook
          (lambda ()
            (setq line-move-visual nil)))
(add-hook 'minibuffer-exit-hook 'regular-line-move)

                                        ;(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators t)

)
(provide 'setup-line-mode)

(require 'gr-adaptive-wrap)
(gr-adaptive-wrap-global-mode)
