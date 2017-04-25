
(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'ntr 'narrow-to-region)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'sl 'sort-lines)
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
(defalias 'uniq 'uniquify-all-lines-buffer)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'lcd 'list-colors-display)
(defalias 'rb 'revert-buffer)
(defalias 'rs 'replace-string)
(defalias 'rr 'reverse-region)
(defalias 'lf 'load-file)
(defalias 'man 'woman)

(defalias 'sh 'shell)
(defalias 'ps 'powershell)
;;(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)

(defalias 'rof 'recentf-open-files)

                                        ; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)

                                        ; modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'vbm 'visual-basic-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'cc 'calc)
(defalias 'dsm 'desktop-save-mode)

(defalias 'acm 'auto-complete-mode)
(defalias 'rb 'revbufs)
(defalias 'rbk 'revbufs-kill)

(provide 'aliases)
