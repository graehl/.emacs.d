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
