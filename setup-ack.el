(if gr-have-ag
    (progn
      (require 'ag)
      (defun gr-ag-cd (pattern &optional directory)
        (interactive "sGrep Literal String: ")
        (ag/search pattern (or directory default-directory))
        )
      )
  (progn
    (require 'ack-and-a-half)
    (setq ack-command (concat "perl " dotfiles-dir "/ack-standalone --nogroup --nocolor "))
    (setq ack-and-a-half-executable ack-command)
    (add-to-list 'ack-and-a-half-project-root-file-patterns "CMakeLists.txt")

    ))


(when nil
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
  )

(provide 'setup-ack)
