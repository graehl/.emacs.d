(require 'dirtrack)
(defun dirtrack-filter-out-pwd-prompt (string)
  "dirtrack-mode doesn't remove the PWD match from the prompt.  This does."
  ;; TODO: support dirtrack-mode's multiline regexp.
  (if (and (stringp string) (string-match (first dirtrack-list) string))
      (replace-match "" t t string 0)
    string))

(defun my-magic-prompt-good ()
  (shell-dirtrack-mode nil)
  (dirtrack-mode t)
  (add-hook 'comint-preoutput-filter-functions
            'dirtrack-filter-out-pwd-prompt t t)
  )

(defun my-magic-prompt () "" (interactive)
  (setq dirtrack-list '("^|PrOmPt|\\([^|]*\\)|" 1 nil))
  (my-magic-prompt-good))

; enable this only if you set your PS1 for my-magic-prompt
(defun enable-magic-dirtrack () "" (interactive)
  (shell-dirtrack-mode t)
  (add-hook 'shell-mode-hook 'my-magic-prompt))

