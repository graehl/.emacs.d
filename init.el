;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (concat (expand-file-name "site-lisp" dotfiles-dir) "/"))
;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)


;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(require 'setup-ido)
(require 'setup-yasnippet)
(require 'setup-dired)
(require 'setup-magit)
(require 'setup-hippie)
(require 'setup-autopair)
(require 'setup-c-mode)
(require 'setup-compilation-mode)
(require 'setup-gud-mode)
(require 'setup-html-mode)
(require 'setup-sh-mode)
(require 'setup-js-mode)
(require 'setup-ack)
(require 'setup-shell-mode)
(require 'setup-sourcepair)
(require 'setup-iswitchb)
(require 'setup-scala-mode)
(require 'setup-clojure-mode)
(require 'setup-js-mode)
;; Map files to modes
(require 'mode-mappings)

(require 'recall-position)
(require 'expand-region)
(require 'mark-more-like-this)
(require 'inline-string-rectangle)
(require 'iy-go-to-char)
(require 'ace-jump-mode)

;; Setup key mappings
(require 'key-chords) ; to enable: key-chord-mode
(require 'key-bindings)


;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; below use defuns.

;; Misc
(require 'appearance)
(require 'misc)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (safe-wrap (server-start)))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'grep-buffers)
(require 'scratch-back)
(require 'wrap-region)
;(iswitchb-mode 1)
;(icomplete-mode 1)
(require 'buffer-init)

(autoload 'live-mode "live-mode" "live mode" t)
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(require 'aliases)
(require 'smex) ;M-x
(smex-initialize)
(require 'ace-jump-mode)

(require 'paredit)
(defun turn-on-paredit () (interactive) (paredit-mode 1))

(when (equal system-type 'darwin) (require 'mac))
(when (equal system-type 'windows-nt) (require 'win))
(safe-wrap (load-file (expand-file-name "local.el" dotfiles-dir)))
