
(setq gr-packages
      '(ansi-color
        auto-complete
        diminish
        ag
        autopair
        ack
        ace-jump-mode
        csv-mode
        paredit
        scala-mode
        gradle-mode
        git-gutter+
        ibuffer
        magit
        ;;gitconfig-mode gitignore-mode
        ;;helm
        wrap-region
        helm-projectile
        ido-ubiquitous
        pandoc-mode
        rainbow-delimiters
;;        smart-operator
        python-mode
        ;;python-mode-expansions
        ;;solarized-theme
        ;;color-theme-solarized
        zenburn-theme
        yasnippet
        expand-region
        gradle-mode
        rainbow-mode))

(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-starts-with message "Your `load-path' seems to contain\nyour `.emacs.d' directory"))
    ad-do-it))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; Set path to .emacs.d
(setq dotfiles-dir (expand-file-name "~/.emacs.d"))

(defun load-dotfile (file)
  (load-file (expand-file-name file dotfiles-dir)))
;; Set path to dependencies
(setq site-lisp-dir (concat (expand-file-name "site-lisp" dotfiles-dir) "/"))
;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path (concat site-lisp-dir "emacs-clang-complete-async"))
(add-to-list 'load-path (concat dotfiles-dir "plugins"))
(add-to-list 'load-path dotfiles-dir)
(require 'color-theme)

(require 'cl)

(require 'gr-config)
(setq package-user-dir "~/.elpa")
;;(load-file (expand-file-name "gr-config.el" dotfiles-dir))
(if (< (emacs-version-major) 24)
    (load-file (expand-file-name "package-23.el" dotfiles-dir))
  (require 'package))

(defun all-to-list (list all)
  (mapc (lambda (x) (add-to-list list x)) all))
(if gr-on-26
    (progn
      (defun define-fringe-bitmap (a b) t)
      (defun set-fringe-mode (a) t)
      )
  (progn
    (defun define-fringe-bitmap (a b c d e) t)
    (defun set-fringe-mode (a) t)
    ))
(if gr-on-24
    (all-to-list 'gr-packages '(smex gist flyspell pcache logito js2-mode gh flycheck artist))
  (add-to-list 'gr-packages 'cl-lib))

;; packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defun gr-elpa-ensure-package (name)
  "Make sure that a particular package is installed; if not then
  automatically download, compile and install it.

  This is primarily used by gr-elpa-require to allow deployment of
  the configuration to a new machine - packages will therefore be
  downloaded on that fresh machine (following installation they are
  automatically kept up to date by the package manager).

  Use this as follows:
  (gr-elpa-ensure-package 'org)"
  (when (not (package-installed-p name))
      (package-install name)))

(defun gr-elpa-require (name)
  "A replacement for the standard Emacs 'require'
  function. This uses gr-elpa-require to download and install a
  package if necessary prior to using the standard 'require'
  function to import it. This is useful to allow the configuration
  to just 'gr-elpa-require' a package and not have to bother
  checking whether it has been installed yet."
  (gr-elpa-ensure-package name)
  (require name))

(defun gr-packages-installed-p ()
  "Check if all packages in `gr-packages' are installed."
  (every #'package-installed-p gr-packages))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")))
(setq package-check-signature nil)
(setq linum-format "%d ")
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
  (package-initialize)
  (gr-install-packages)
  (package-initialize)
  ;; markdown-mode doesn't have autoloads for the auto-mode-alist
  ;; so we add them manually if it's already installed
  (when (package-installed-p 'markdown-mode)
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))
  )

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
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

(defun gr-auto-install-install ()
  "build auto-install mappings"
  (interactive)
  (mapc
   (lambda (entry)
     (let ((extension (car entry))
           (package (cadr entry))
           (mode (cadr (cdr entry))))
       (unless (package-installed-p package)
         (gr-auto-install extension package mode))))
   gr-auto-install-alist))

(add-hook 'after-init-hook 'gr-auto-install-install)

(defun gr-ensure-module-deps (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'package-install (remove-if #'package-installed-p packages)))

(defun file-not-autosave (path)
  (not (or (string-match "#$" path)
           (string-match "~$" path))))

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (and (file-not-autosave project) (file-directory-p project))
    (add-to-list 'load-path project)))

(gr-init-packages)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "/backups")))))

(setq delete-auto-save-files nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 9
  kept-old-versions 5
  version-control t)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(add-to-list 'load-path defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-not-autosave file) (file-regular-p file))
    (load file)))


;; Setup extensions
(require 'setup-ido)
(require 'setup-perl)
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
(require 'setup-shell-mode)
(require 'setup-sourcepair)
(require 'setup-iswitchb)
;;(require 'setup-scala-mode)
(require 'setup-line-mode)
;;(require 'setup-paredit)
(require 'setup-ispell)
(require 'setup-spell)
(require 'setup-git-gutter)
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
;;(iswitchb-mode 1)
;;(icomplete-mode 1)
(require 'buffer-init)

;;(autoload 'live-mode "live-mode" "live mode" t)
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(require 'aliases)
(when (and nil gr-on-24)
  (require 'smex) ;M-x
  (smex-initialize))

(when (equal system-type 'windows-nt) (require 'win))
(safe-wrap (load-file (expand-file-name "local.el" dotfiles-dir)))

(require 'setup-code-modes)
(install-coding-hooks)

(when t ; redundant with autopair
  (require 'wrap-region)
  (wrap-region-global-mode t))

(defvar gr-on-term nil)
(setq gr-on-term (eq window-system nil))

(require 'gr-cleanup-save)
(setq gr-cleanup-save-excessive-spaces 1)
(gr-cleanup-save-global-mode)
(require 'setup-isearch)
(require 'setup-ediff)
(require 'make-byte-compile)
;;(require 'setup-lua)
(defun emacs-d-recompile () (interactive) (make-byte-compile-directory dotfiles-dir))
(defun site-lisp-recompile () (interactive) (make-byte-compile-directory site-lisp-dir))
(defun gr-byte-recompile () (interactive) (emacs-d-recompile) (site-lisp-recompile))
(delete-selection-mode nil)
(require 'setup-change-log)
;;(require 're-builder+)
;;(require 'pandoc-mode)
(require 'setup-compilation-mode)

;;(require 'optional-bindings)
;;(require 'setup-helm)
;; emacs-clang-complete-async doesn't seem to work at all (tried google) - maybe try https://truongtx.me/2013/03/10/ecb-emacs-code-browser/ ?

;;(add-to-list 'load-path (concat site-lisp-dir "emacs-clang-complete-async"))
;;(require 'setup-clang-complete)
;;(add-to-list 'load-path (concat site-lisp-dir "/" multiple-cursors.el "/"))
;;(require 'ffap) ; find files/urls at point ; (ffap-bindings)
(defun gr-load-appearance ()
  (interactive)
  (load-file (expand-file-name "appearance.el" dotfiles-dir))
  (when gr-on-mac
    (require 'mac)
    (require 'mac-after)
    ))
;;(require 'appearance)
;;(add-hook 'after-init-hook 'gr-load-appearance)
(defun gr-ag-after-init ()
  (gr-auto-install-install)
  (load-file (expand-file-name "setup-ag.el" dotfiles-dir))
  (require 'key-bindings)
  (gr-load-appearance)
  (if gr-have-ag
      (require 'setup-ag)
    (require 'setup-ack))
  (load-file (expand-file-name "key-bindings.el" dotfiles-dir))
  (load-file (expand-file-name "setup-compilation-mode.el" dotfiles-dir))
  (load-file (expand-file-name "setup-ac-yasnippet.el" dotfiles-dir))
)

(add-hook 'after-init-hook 'gr-ag-after-init)
(gr-ag-after-init)
(when nil (not (boundp 'gr-init-once-t))
  (setq gr-init-once-t t)
  (load-file (expand-file-name "init.el" dotfiles-dir)))

(fmakunbound 'ed)
(setq compilation-ask-about-save nil)

;; first argument is the package name, the second is the mode in question, and the third is the new lighter for the mode.

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

(require 'recentf)
;; Save a list of recent files visited.
(recentf-mode 1)

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(require 'setup-projectile)

(require 'setup-tramp)
