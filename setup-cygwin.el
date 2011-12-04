(defcustom use-cygwin-root "c:/cygwin" "use cygwin if it exists at this path" ':type 'string)

(defun setup-cygwin () "" (interactive)
  (setq cygwin-mount-cygwin-bin-directory (concat use-cygwin-root "/bin"))
  (when (and (file-executable-p use-cygwin-root) (file-executable-p cygwin-mount-cygwin-bin-directory))
    (require 'cygwin-mount)
    (cygwin-mount-activate)
    (require 'win)
    (add-PATH cygwin-mount-cygwin-bin-directory)
    (add-PATH (concat use-cygwin-root "/usr/local/bin"))

    (defun follow-cygwin-symlink ()
      "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
      (save-excursion
        (goto-char 0)
        (if (looking-at
             "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
            (progn
              (re-search-forward
               "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
              (find-alternate-file (match-string 1)))
          (if (looking-at "!<symlink>")
              (progn
                (re-search-forward "!<symlink>\\(.*\\)\0")
                (find-alternate-file (match-string 1))))
          )))
    (when nil
      (add-hook 'find-file-hooks 'follow-cygwin-symlink)
      (add-hook 'find-file-not-found-hooks 'follow-cygwin-symlink)
      )
                                        ;(require 'cygwin-mount)
                                        ;(cygwin-mount-activate)
                                        ;(add-hook 'find-file-hooks 'follow-cygwin-symlink)
                                        ;(add-hook 'find-file-not-found-hooks 'follow-cygwin-symlink)

    (setq explicit-shell-file-name "bash.exe")
    (setq shell-file-name explicit-shell-file-name)
    ))

(setup-cygwin)

(provide 'setup-cygwin)
