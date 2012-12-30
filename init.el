;;(require 'ffap) ; find files/urls at point ; (ffap-bindings)

(defun emacs-version-get-component (component)
  (let ((old-match-data (match-data))
	(version 0)
	(regexp (cond
		 ((eq 'major component) "^\\([0-9]+\\)")
		 ((eq 'minor component) "^[0-9]+\\.\\([0-9]+\\)")
		 ((eq 'build component) "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"))))
    (unwind-protect
	(and (string-match regexp emacs-version)
	     (setq version
		   (string-to-int (substring emacs-version
					     (match-beginning 1)
					     (match-end 1)))))
      (store-match-data old-match-data))
    version))

(defun emacs-version-major ()
  "Returns (as an integer) the major version number."
  (interactive)
  (emacs-version-get-component 'major))

;; Set path to .emacs.d
(setq dotfiles-dir "~/.emacs.d")
;(file-name-directory                    (or (buffer-file-name) "~/.emacs.d")))

;; Set path to dependencies
(setq site-lisp-dir (concat (expand-file-name "site-lisp" dotfiles-dir) "/"))
;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

(defun file-not-autosave (path)
  (not (string-match "#$" path)))

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (and (file-not-autosave project) (file-directory-p project))
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))


;; Save point position between sessions
(setq save-place-file (expand-file-name ".places" dotfiles-dir))
(require 'saveplace)
(setq-default save-place t)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(add-to-list 'load-path defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-not-autosave file) (file-regular-p file))
    (load file)))


(require 'key-bindings)

;; Setup extensions
(require 'setup-ido)
(require 'setup-yasnippet)
(require 'setup-dired)
;;(require 'setup-magit)
(require 'setup-hippie)
;(require 'setup-autopair)
(require 'setup-c-mode)
(require 'setup-compilation-mode)
(require 'setup-gud-mode)
(require 'setup-html-mode)
(require 'setup-sh-mode)
;(require 'setup-js-mode)
(require 'setup-ack)
(require 'setup-shell-mode)
(require 'setup-sourcepair)
(require 'setup-iswitchb)
;(require 'setup-scala-mode)
;(require 'setup-clojure-mode)
(require 'setup-line-mode)
;(require 'setup-paredit)
(require 'setup-ispell)

;; Map files to modes
(require 'mode-mappings)

(require 'recall-position)
(require 'expand-region)
(require 'mark-more-like-this)
(require 'inline-string-rectangle)
(require 'iy-go-to-char)
;;(require 'ace-jump-mode)

;; Setup key mappings
;;(require 'key-chords) ; to enable: key-chord-mode
(require 'setup-python)
(require 'setup-term)
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

(when (equal system-type 'darwin) (require 'mac))
(when (equal system-type 'windows-nt) (require 'win))
(safe-wrap (load-file (expand-file-name "local.el" dotfiles-dir)))

(require 'setup-code-modes)
(install-coding-hooks)

(when t ; redundant with autopair
  (require 'wrap-region)
  (wrap-region-global-mode t))

(require 'gr-cleanup-save)
(setq gr-cleanup-save-excessive-spaces 1)
(gr-cleanup-save-global-mode)
(require 'setup-isearch)
(require 'setup-ediff)
(require 'make-byte-compile)
(defun emacs-d-recompile () (interactive) (make-byte-compile-directory dotfiles-dir))
(defun site-lisp-recompile () (interactive) (make-byte-compile-directory site-lisp-dir))
(delete-selection-mode nil)
(require 'setup-change-log)
(require 're-builder+)
(require 'pandoc-mode)
(require 'setup-spell)
(require 'optional-bindings)
