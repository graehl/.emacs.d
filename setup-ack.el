(require 'ack)
(setq ack-command "ack --nogroup --nocolor ")
(if (string= system-name "K")
    (setq ack-command (concat "perl " dotfiles-dir "/ack-standalone --nogroup --nocolor "))
  (setq ack-command "ag --nogroup --nocolor "))
;;no-heading
(require 'grep)
;;(grep-apply-setting 'grep-find-command ack-command)

(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)
(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive) ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ;; Close STDIN to keep ack from going into filter mode
  (let ((null-device "") ;(format "< %s" "/dev/null")) ;; null-device
        (grep-command ack-command)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))

(provide 'setup-ack)
