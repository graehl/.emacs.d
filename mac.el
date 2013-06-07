;;(push "/usr/local/bin" exec-path) ;brew

(require 'font)
;; change command to meta, and ignore option to use weird norwegian keyboard
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

(setq mac-size-default 14)
(setq mac-weight-default 'light)
(setq mac-font-default "Monaco")
(setq mac-font-default "Andale Mono")
(setq mac-font-default "PragmataPro")
;;(setq mac-font-default "Consolas")
;;(mac-font mac-font-default mac-size-default mac-weight-default)
;;(mac-font "PragmataPro")
;; mac friendly font
;;(mac-font "Monaco-12")
(set-face-attribute 'default nil :font mac-font-default)
(set-face-attribute 'default nil :height (* 10 mac-size-default))
(when nil
  (mac-font "Consolas" 14)
  (mac-font "Andale Mono" 14)
  (mac-font "DejaVu Sans Mono" 14 'light)
  (mac-font "Pragmata" 14 'light)
  (mac-font "PragmataPro" 14 'light)
  )


                                        ;(custom-set-faces '(default ((t (:height 100 :family "Consolas" :embolden nil)))))

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(setq ns-use-native-fullscreen nil)
;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) 'toggle-frame-fullscreen)

(setq on-mac-port nil)
(defun toggle-frame-fullscreen-mac-port ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(when on-mac-port (global-set-key (quote [M-f10]) 'toggle-frame-fullscreen-mac-port))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;;(setq inferior-octave-program "/Applications/Octave.app/Contents/MacOS/Octave")
(setq inferior-octave-program "octave")
;;(add-to-list 'exec-path "/Applications/Octave.app/Contents/Resources/bin")
;;(autoload 'octave-mode "octave-mod" nil t)
;;(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
;;(add-hook 'octave-mode-hook (lambda () (abbrev-mode 1) (auto-fill-mode 1) (if (eq window-system 'x) (font-lock-mode 1))))
(autoload 'run-octave "octave-inf" nil t)

;;(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")
;;(setq shell-prompt-pattern "^|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]? *")
(setq shell-prompt-pattern "^\\(|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]?\\|[^#$%>\n]*[#$%>]\\) *")
(require 'setup-shell-prompt)

(default-size-frame 200 60)

(defun top-mode-mac-generate-top-command (user)
  (if (not user)
      "top -l 1"
    (format "top -l 1 -user %s" user)))
(setq top-mode-generate-top-command-function
      'top-mode-mac-generate-top-command)
(setq top-mode-strace-command "/usr/sbin/dtrace")

(menu-bar-mode t)
(provide 'mac)
