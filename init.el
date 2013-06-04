(setq gr-packages
  '(ack-and-a-half ace-jump-mode
                   paredit
                   scala-mode
                   flycheck gist gitconfig-mode gitignore-mode
                   helm-projectile ido-ubiquitous
                   solarized-theme zenburn-theme rainbow-mode))

(defun emacs-version-matches (substr)
  (string-match substr (emacs-version)))

(defvar emacs-mac-port (emacs-version-matches "Carbon"))


(if emacs-mac-port
    (progn
(unless (fboundp 'advice-add) (defun advice-add (&rest args) t))
(unless (fboundp 'advice--add-function) (defun advice--add-function (&rest args) t))
(unless (fboundp 'advice--p) (defun advice--p (&rest args) nil))
(unless (fboundp 'advice-remove) (defun advice-remove (&rest args) t))

(provide 'advice)
)
 (require 'advice)
)

(require 'cl)

;; Set path to .emacs.d
(setq dotfiles-dir "~/.emacs.d")

;; Set path to dependencies
(setq site-lisp-dir (concat (expand-file-name "site-lisp" dotfiles-dir) "/"))
;; Set up load path
(when emacs-mac-port (add-to-list 'load-path "/usr/local/emacs-mac-port/share/emacs"))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun gr-packages-installed-p ()
  "Check if all packages in `gr-packages' are installed."
  (every #'package-installed-p gr-packages))
(defun gr-install-packages ()
  "Install all packages listed in `gr-packages'."
  (interactive)
  (unless (gr-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
     (remove-if #'package-installed-p gr-packages))))

(defun gr-init-packages ()
  "Install all packages listed in `gr-packages'."
  (interactive)
  (gr-install-packages)
  (package-initialize))

(defmacro gr-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar gr-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (gr-auto-install extension package mode))))
 gr-auto-install-alist)

(defun gr-ensure-module-deps (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'package-install (remove-if #'package-installed-p packages)))


;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun file-not-autosave (path)
  (not (or (string-match "#$" path)
           (string-match "~$" path))))

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (and (file-not-autosave project) (file-directory-p project))
    (add-to-list 'load-path project)))


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
		   (string-to-number (substring emacs-version
					     (match-beginning 1)
					     (match-end 1)))))
      (store-match-data old-match-data))
    version))

(defun emacs-version-major ()
  "Returns (as an integer) the major version number."
  (interactive)
  (emacs-version-get-component 'major))


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
;;(require 'setup-autopair)
(require 'setup-c-mode)
(require 'setup-compilation-mode)
(require 'setup-gud-mode)
(require 'setup-html-mode)
(require 'setup-sh-mode)
(require 'setup-ack)
(require 'setup-shell-mode)
(require 'setup-sourcepair)
(require 'setup-iswitchb)
;;(require 'setup-scala-mode)
(require 'setup-line-mode)
;;(require 'setup-paredit)
(require 'setup-ispell)

;; Map files to modes
(require 'mode-mappings)

(require 'recall-position)
(require 'expand-region)
;;(require 'inline-string-rectangle)
(require 'iy-go-to-char)

;; Setup key mappings
(require 'setup-python)
(require 'setup-term)
;; below use defuns.

(when (equal system-type 'darwin) (require 'mac))

;; Misc

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
(defun gr-byte-recompile () (interactive) (emacs-d-recompile) (site-lisp-recompile))
(delete-selection-mode nil)
(require 'setup-change-log)
(require 're-builder+)
(require 'pandoc-mode)
(require 'setup-spell)
(require 'optional-bindings)
(require 'setup-helm)
;;(add-to-list 'load-path (concat site-lisp-dir "/" multiple-cursors.el "/"))
;;(require 'ffap) ; find files/urls at point ; (ffap-bindings)
(gr-init-packages)
(require 'appearance)
