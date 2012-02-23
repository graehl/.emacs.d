;; get rid of minibuffer recursion annoyances

(provide 'minibuffer-defuns)

(defun stop-using-minibuffer ()
  (interactive)
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun leave-minibuffer ()
  (interactive)
  (if (> (minibuffer-depth) 0) (exit-minibuffer)))

(defun quit-minibuffer ()
  (interactive)
  (dotimes (i (minibuffer-depth)) (exit-minibuffer)))

(defun keyboard-really-quit ()
  (interactive)
  (stop-using-minibuffer)
  (quit-minibuffer)
  (keyboard-quit))
