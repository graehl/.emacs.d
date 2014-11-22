;; https://github.com/Golevka/emacs-clang-complete-async

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "/ac-dicts"))
(setq ac-comphist-file (concat dotfiles-dir "/ac-comphist.dat"))
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(define-key ac-menu-map (kbd "M-/") 'auto-complete)
(define-key ac-menu-map (kbd "<down>") nil)
(define-key ac-menu-map (kbd "<up>") nil)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-complete-mode-map "\M-/" 'nil)
;;(setq ac-auto-start 4)
(setq ac-auto-start nil)
(global-set-key "\M-/" 'dabbrev-expand)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;(define-key ac-complete-mode-map "\M-/" 'ac-stop)
(setq ac-dwim t)

(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun ac-common-setup ()
  ())

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

(provide 'setup-clang-complete)
;;(ac-clang-syntax-check)
