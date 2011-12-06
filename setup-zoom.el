(require 'zoom-frm)
(defun toggle-zoom-full ()
  (interactive)
  (recenter)
  (toggle-zoom-frame)
  (fullscreen) ; toggle twice to get back to where you were
  (fullscreen)
  (recenter))
(setq frame-zoom-font-difference 4) ;;customize
(provide 'setup-zoom)
