(defun with-sh (c) (concat ". ~/.e; " c))
(setq compile-command (with-sh "buildgraehl forest-em"))
(setq compile-command (with-sh "HYPERGRAPH_DBG=6 LAZYK_DBG=10 test=Hypergraph/Best raccm Debug"))
(setq compile-command (with-sh "ARGS='--b.xs 1 2 --a.xs 1 asdf --a.str hi' g1 configure_program_options.hpp -DGRAEHL_CONFIGURE_SAMPLE_MAIN=1"))
(setq compile-command (with-sh "makerun Utf8Normalize --help"))
(setq compile-command (with-sh "test=Hypergraph/Best racm Debug"))

;;(global-set-key (kbd "<home>") (lambda () (interactive )(switch-to-buffer "*Inferior Octave*")))
(setq transient-mark-mode t)

(setq gud-latest "~/bin/egdbFsTokenize --nbest=1 --tokenizer /Users/graehl/x/RegressionTests/FsTokenize/morenums-train.stdout-expected  /Users/graehl/x/RegressionTests/FsTokenize/nums.untok --prune-to-nbest 0")

(setq gud-latest "~/bin/egdbHgBest --nbest=2 /Users/graehl/x/bugs/xyz/fsaw")
(setq compilation-skip-threshold 2)
