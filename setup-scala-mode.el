;;(require 'scala-mode nil 'noerror)

(defun my-scala-eval-line ()
  "eval current line"
  (interactive)
  (scala-eval-region (line-beginning-position) (line-end-position))
  )
(defun my-scala-eval ()
  "Send the current 'definition' to the Scala interpreter.
   Cursor is on a line that doesn't start w/ ws or }: eval current line
   or else: find prev and next such line that starts w/ non ws { }.  That's the start/end of a defn.
"
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (while and (not (= (point) (point-max))) (looking-at "\\s") (next-line 1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat (lambda (x) x)
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that start with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (scala-eval-region (point) end))))
;;(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "M-;") 'my-scala-eval-line) (local-set-key (kbd "M-'") 'scala-eval-region)))
;(require 'scala-mode)

(require 'scala-mode2)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(provide 'setup-scala-mode)
