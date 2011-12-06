(push "/usr/local/bin" exec-path) ;brew

(require 'font)
;; change command to meta, and ignore option to use weird norwegian keyboard
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

;; mac friendly font
;;(mac-font "Monaco-12")
(when nil
  ;;(mac-font "Lucida Console-14")
  (mac-font "Andale Mono-14")
  )
(mac-font "Consolas-14")
;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

(setq inferior-octave-program "/Applications/Octave.app/Contents/MacOS/Octave")

(add-hook 'shell-mode-hook 'my-magic-prompt)
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")
(setq shell-prompt-pattern "^|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]? *")
(setq shell-prompt-pattern "^\\(|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]?\\|[^#$%>\n]*[#$%>]\\) *")

(enable-magic-dirtrack)
(default-size-frame 200 60)
(provide 'mac)
