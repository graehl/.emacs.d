(require 'js)
(defun my-js-mode-hook ()
  (key-chord-define js-mode-map ";;" "\C-e")
)
(add-hook 'js-mode-hook 'my-js-mode-hook)
(provide 'setup-js-mode)