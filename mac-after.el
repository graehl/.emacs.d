(setq gr-on-term (eq system-uses-terminfo t))
(when gr-on-term
  (safe-wrap (load-file (expand-file-name "xterm256.el" dotfiles-dir)))
;;  (safe-wrap (load-file (expand-file-name "sco-termkeys.el" dotfiles-dir)))
  )
(load-file "gr-config.el")
(load-file "appearance.el")
(load-file "mac.el")
(load-file "key-bindings.el")
;;(global-unset-key (kbd "M-["))
(load-file "setup-spell.el")
(provide 'mac-after)
