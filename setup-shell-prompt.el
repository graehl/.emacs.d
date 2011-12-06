;; only use this if you have in your bash profile: if [[ $INSIDE_EMACS ]] ; then ; PS1='|PrOmPt|\w|\w $ ' fi
(require 'shell-defuns)
(setq shell-prompt-pattern "^\\(|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]?\\|[^#$%>\n]*[#$%>]\\) *")
(enable-magic-dirtrack)
(provide 'setup-shell-prompt)
