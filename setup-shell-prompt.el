(require 'shell-defuns)
(setq shell-prompt-pattern "^\\(|PrOmPt|[^|\n]*|[^:\n]+:[^ \n]+ *[#$%>\n]?\\|[^#$%>\n]*[#$%>]\\) *")
(enable-magic-dirtrack)
(provide 'setup-shell-prompt)
