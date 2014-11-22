;; Make dired less verbose
(require 'dired)
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Always reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create)
  (revert-buffer))
(ad-activate 'dired-create-directory)

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))


(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(provide 'setup-dired)
