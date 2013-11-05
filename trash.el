
(provide 'compile-) ;; included by compile+ but i don't want the fit-1-window-frames-on
;; Use nil, not `underline', to turn off underlining.
;;;###autoload
(defcustom compilation-message-face nil
  "*Face name to use for whole messages.
Faces `compilation-error-face', `compilation-warning-face',
`compilation-info-face', `compilation-line-face' and
`compilation-column-face' get prepended to this, when applicable."
  :type 'face :group 'compilation :version "22.1")

;; Instead of `highlight', which is hard-coded in `compile.el'.
;;;###autoload
(defface compilation-mouseover '((t (:underline t)))
  "*Face used to highlight text the mouse is over."
  :group 'compilation :group 'font-lock-highlighting-faces)

(unless (facep 'next-error)
  (defface next-error '((t (:background "SkyBlue")))
    "Face used to highlight next error locus."
    :group 'next-error))
