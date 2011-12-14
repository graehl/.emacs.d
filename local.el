(defun with-sh (c) (concat ". ~/.bashrc;. ~/a; " c))
(setq compile-command (with-sh "buildgraehl forest-em"))
(setq compile-command (with-sh "HYPERGRAPH_DBG=6 LAZYK_DBG=10 test=Hypergraph/Best raccm Debug"))
(setq compile-command (with-sh "HYPERGRAPH_DBG=6 HGBEST_DBG=10 LAZYK_DBG=10 tests=Hypergraph/Best racm Debug"))
