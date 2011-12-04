
(defun my-path-elts ()
  (subseq (my-split-path (buffer-file-name)) 0 -1))


(defun my-split-filename (filename)
  "split a FILENAME string into a (basename . extension) pair"
  (let* ((fname filename)

         (prefix
          (if (string-match "\\(.*\\)\\(\\..*\\)" fname)
              (substring fname 0 (match-end 1))
            fname))
         (extension
          (if (match-beginning 2)
              (substring fname (+ 1 (match-beginning 2)))
            nil)))
    (cons prefix extension)
    ))

(defun my-split-current-filename ()
  (my-split-filename (file-name-nondirectory (buffer-file-name))))

(defun my-split-path (path)
  (let ((result nil) (elt nil))
    (while (and path
                (not (equal (setq elt (file-name-nondirectory path)) "")))
      (setq result (cons elt result))
      (setq path (file-name-directory path))
      (setq path (and path (directory-file-name path))))
    result))
