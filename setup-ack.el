(setq ack-command (concat "perl " dotfiles-dir "ack-standalone --no-heading --no-color -a"))
(require 'grep)
(grep-apply-setting 'grep-find-command ack-command)
(provide 'setup-ack)
