(defun install-hook (h f &optional append local)
  (remove-hook h f local)
  (add-hook h f append local))

(defun install-hooks (hs f &optional append local)
  (loop for h in hs do (install-hook h f append local)))

(defun remove-hooks (hs f &optional append local)
  (loop for h in hs do (remove-hook h f append local)))

(defun run-hook (h) (loop for f in h do (funcall f)))

(provide 'hooks-defuns)
