(defun gnu-font-sz (font sz) (interactive)
  (set-frame-font (concat font "-" sz)
                  (safe-wrap
                   (let ((fontpre (concat "-*-" font "-"))
                         (fontpost (concat "-*-*-" sz "-*-*-*-c-*-*-ansi-")))
                     (set-default-font (concat (concat fontpre "normal-r") fontpost))
                     (set-face-font 'italic (concat (concat fontpre "normal-i") fontpost))
                     (set-face-font 'bold-italic (concat (concat fontpre "bold-i") fontpost))))))

(defun gnu-font (font) (interactive)
  (gnu-font-sz font "11"))

(defvar mac-font-default "Monaco-12")
(defvar mac-size-default 12)
(defvar mac-weight-default 'light)
(defun mac-font (&optional font size weight)
  (interactive "sFont name (Monaco, Andale Mono, Consolas, DejaVuSans, Inconsolata, Pragmata): \nipoints (height in 1/72 in): ")
  (if (eq nil weight) (setq weight mac-weight-default))
  (if (eq nil size) (setq size mac-size-default))
  (if (eq nil font) (setq font mac-font-default))
  (set-face-attribute 'default nil :font font :height (* 10 size) :weight weight))

(provide 'font)
