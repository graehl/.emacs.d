(require 'dirtrack)
(defun gr-system (string) (interactive) (string= system-name string))
(defun dirtrack-on () (not (gr-system "K")))

(defun dirtrack-filter-out-pwd-prompt (string)
  "dirtrack-mode doesn't remove the PWD match from the prompt.  This does."
  ;; TODO: support dirtrack-mode's multiline regexp.
  (if (and (stringp string) (string-match (first dirtrack-list) string))
      (replace-match "" t t string 0)
    string))

(defun gr-magic-prompt-good ()
  (shell-dirtrack-mode nil)
  (dirtrack-mode t)
  (add-hook 'comint-preoutput-filter-functions
            'dirtrack-filter-out-pwd-prompt t t)
  )

(defun gr-magic-prompt () "" (interactive)
  (when (gr-dirtrack-on)
    (setq dirtrack-list '("^|PrOmPt|\\([^|]*\\)|" 1 nil))
    (gr-magic-prompt-good)))

                                        ; enable this only if you set your PS1 for gr-magic-prompt:
;; if [[ $INSIDE_EMACS ]] ; then export PS1='|PrOmPt|\w|\w $ '; fi

(defun enable-magic-dirtrack () "" (interactive)
  (shell-dirtrack-mode t)
  (add-hook 'shell-mode-hook 'gr-magic-prompt))

(provide 'shell-defuns)
