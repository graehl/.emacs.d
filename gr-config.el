(defun emacs-version-matches (substr)
  (string-match substr (emacs-version)))

(setq gr-mac-port (emacs-version-matches "Carbon"))

(setq gr-on-mac (eq system-type 'darwin))
(setq gr-on-linux (eq system-type 'gnu/linux))
(setq gr-on-win (not (or gr-on-mac gr-on-linux)))

(defvar gr-have-ag (not gr-on-win))

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

(setq gr-on-24 (>= (emacs-version-major) 24))

(provide 'gr-config)
