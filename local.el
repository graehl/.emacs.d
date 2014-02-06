(defun with-sh (c) (concat "bash -c '. ~/.e; " c "'"))
(defun with-sh (c) (concat ". ~/.e; " c))
(setq compile-command (with-sh "buildgraehl forest-em"))
(setq compile-command (with-sh "HYPERGRAPH_DBG=6 LAZYK_DBG=10 test=Hypergraph/Best raccm Debug"))
(setq compile-command (with-sh "ARGS='--b.xs 1 2 --a.xs 1 asdf --a.str hi' g1 configure_program_options.hpp -DGRAEHL_CONFIGURE_SAMPLE_MAIN=1"))
(setq compile-command (with-sh "makerun Utf8Normalize --help"))
(setq compile-command (with-sh "test=Hypergraph/Best racm Debug"))

;;(global-set-key (kbd "<home>") (lambda () (interactive )(switch-to-buffer "*Inferior Octave*")))
(setq transient-mark-mode t)

(setq gud-latest "~/bin/egdbFsTokenize --nbest=1 --tokenizer /Users/graehl/x/RegressionTests/FsTokenize/morenums-train.stdout-expected /Users/graehl/x/RegressionTests/FsTokenize/nums.untok --prune-to-nbest 0")

(setq gud-latest "~/bin/egdbHgBest --nbest=2 /Users/graehl/x/bugs/xyz/fsaw")
(setq compilation-skip-threshold 1)
(setq compile-command (with-sh "kjen"))
(setq compile-command (with-sh "cjen Debug"))
(setq explicit-shell-file-name
      )
(setq shell-file-name explicit-shell-file-name)
;;(add-to-list 'exec-path "C:/chocolatey/bin")

(defun set-shell-cmd (cmd)
  (interactive)
  (setq shell-file-name cmd)
  (setq explicit-shell-file-name shell-file-name))

(setq gr-cmd-exe "C:/bin/ntemacs24/bin/cmdproxy.exe")
(setq gr-bash-exe "C:/msys/bin/bash.exe")
(defun set-cmd-exe ()
  (interactive)
  (setq shell-file-name gr-cmd-exe)
  (setq explicit-shell-file-name shell-file-name))

(defun set-bash-exe ()
  (interactive)
  (set-shell-cmd gr-bash-exe))

(defmacro with-cmd (cmd x)
  (let ((shell-file-name cmd)
        (explicit-shell-file-name shell-file-name))
    x))

(set-cmd-exe)
