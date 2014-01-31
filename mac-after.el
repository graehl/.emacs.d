(setq gr-on-term (eq system-uses-terminfo t))
(when gr-on-term
  (safe-wrap (load-file (expand-file-name "xterm256.el" dotfiles-dir)))
;;  (safe-wrap (load-file (expand-file-name "sco-termkeys.el" dotfiles-dir)))
  )
(load-dotfile "gr-config.el")
(load-dotfile "appearance.el")
(load-dotfile "mac.el")
(load-dotfile "key-bindings.el")
;;(global-unset-key (kbd "M-["))
(load-dotfile "setup-spell.el")
(provide 'mac-after)