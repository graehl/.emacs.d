(require 'dash)
(require 'magit)

;;(setq magit-last-seen-setup-instructions "1.4.0")

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(defvar magit-status-fullscreen-window-configuration-register
  ?b
  "The register to store the current window configuration in when
entering fullscreen magit-status.")

(defvar magit-status-fullscreen-register
  ?g
  "The register to store the fullscreen magit-status
window configuration in.")

(defun magit-status-fullscreen ()
  "Save the current window configuration, run magit-status
and delete other windows, providing a fullscreen git mode.
The previous window configuration is stored in the register
specified by the magit-status-fullscreen-window-configuration-register
variable. The fullscreen magit status configuration is stored
in register specified by the magit-status-register variable."
  (interactive)
  (window-configuration-to-register magit-status-fullscreen-window-configuration-register)
  (magit-status (magit-get-top-dir default-directory))
  (delete-other-windows)
  (window-configuration-to-register magit-status-fullscreen-register))


(add-hook 'magit-log-edit-mode-hook '(lambda () (setq fill-column 70) (auto-fill-mode t)))

(setq magit-commit-all-when-nothing-staged t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 70)))

(define-key magit-status-mode-map (kbd "q")
  '(lambda ()
     (interactive)
     (jump-to-register magit-status-fullscreen-window-configuration-register)))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'setup-magit)

;;(setq git-show/sha-command "log -g --pretty=format:'%H %an %d %ar %s'")
(setq git-show/sha-command "log --pretty=format:'%H %an %d %ar %s'")
;;(setq visual-line-fringe-indicators nil)

(require 'git-timemachine)
