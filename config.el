(defun emacs-version-matches (substr)
  (string-match substr (emacs-version)))

(setq gr-mac-port (emacs-version-matches "Carbon"))

(setq gr-on-mac (eq system-type 'darwin))
(setq gr-on-linux (eq system-type 'gnu/linux))
(setq gr-on-win (not (or gr-on-mac gr-onlinux)))

(defvar gr-have-ag (not gr-on-win))

(defun emacs-version-major ()
  "Returns (as an integer) the major version number."
  (interactive)
  (emacs-version-get-component 'major))
