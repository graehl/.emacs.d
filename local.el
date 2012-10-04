(defun with-sh (c) (concat ". ~/.e; " c))
(setq compile-command (with-sh "buildgraehl forest-em"))
(setq compile-command (with-sh "HYPERGRAPH_DBG=6 LAZYK_DBG=10 test=Hypergraph/Best raccm Debug"))
(setq compile-command (with-sh "g1 configure_program_options.hpp -DGRAEHL_CONFIGURE_SAMPLE_MAIN=1"))
(setq compile-command (with-sh "makerun Utf8Normalize --help"))
(setq compile-command (with-sh "tests=Hypergraph/Best racm Debug"))
(when (string= system-name "LATTE")
  (setq inferior-octave-program "C:\\octave\\Octave3.6.1_gcc4.6.2\\bin\\octave.exe")
(setq py-python-command "c:/cygwin/bin/python.exe")
(setq python-command "c:/cygwin/bin/python.exe")
(setup-cygwin)
)
<<<<<<< HEAD

(global-set-key (kbd "<insert>") (lambda () (interactive )(switch-to-buffer "*Inferior Octave*")))
=======
(setq transient-mark-mode t)

(setq gud-latest "~/bin/egdbFsTokenize --nbest=1 --tokenizer /Users/graehl/x/RegressionTests/FsTokenize/morenums-train.stdout-expected  /Users/graehl/x/RegressionTests/FsTokenize/nums.untok --prune-to-nbest 0")

(setq gud-latest "~/bin/egdbHgBest --nbest=2 /Users/graehl/x/bugs/xyz/fsaw")
>>>>>>> 98df42b27f13930cb8cbc7652a556c94c069f600
