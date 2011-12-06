(require 'key-chord)

(setq key-chord-two-keys-delay 0.05)

(key-chord-mode nil)
;; i don't like how it's turning chords i haven't defined on e.g. ctrl, release, backspace - at least i think it is.

;; Move to char similar to "f" in vim, f+j forward, f+h backward
(key-chord-define-global "fj" 'iy-go-to-char)
(key-chord-define-global "fh" 'iy-go-to-char-backward)


;; Indent entire buffer and fix whitespace
(key-chord-define-global "i0" 'indent-buffer)
(key-chord-define-global "i9" 'whitespace-cleanup)


(provide 'key-chords)
