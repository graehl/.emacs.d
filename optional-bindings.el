(require 'setup-magit)

(global-set-key (kbd "M-c") 'magit-status)
(define-key magit-status-mode-map (kbd "q")
  '(lambda ()
     (interactive)
     (jump-to-register magit-status-fullscreen-window-configuration-register)))
(require 'smex)(global-set-key (kbd "M-x") 'smex)
(require 'scratch-back)(global-set-key (kbd "M-<f2>") 'scratch-back)
;;(require 'hobo)(global-set-key (kbd "C-x F") 'hobo-find-file)
(global-set-key (kbd "M-c") 'magit-status)

(provide 'optional-bindings)
