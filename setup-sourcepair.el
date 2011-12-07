(require 'sourcepair)
(setq sourcepair-source-path    '( "src" "impl" "." "../src"))
(setq sourcepair-header-path    '( "." "include" "../include" ))
(setq sourcepair-recurse-ignore '( "CVS" "Obj" "Debug" "Release" ".svn" ".git"))

(provide 'setup-sourcepair)
