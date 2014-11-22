
  (when (re-search-backward "\n\n\n+" (gr-before-point within) t)
    (delete-region (+ (or keep 1) (match-beginning 0)) (match-end 0)))))
