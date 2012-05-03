(require 'isearch+)
(setq isearchp-set-region-flag nil) ; i like t, but need it to restore state after i move or it's unusable
(require 'misc-cmds)
(defun isearchp-set-region ()
  "Set region around search target, if `isearchp-set-region-flag'.
Used only for Transient Mark mode."
  (when (and isearchp-set-region-flag transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)

(provide 'setup-isearch)


;;; isearch+ has:

;;* Ability to search ''within character-property zones''.  Example:
;;    search within zones having a `face' text property with a value
;;    of `font-lock-comment-face' or `font-lock-string-face'.  Search
;;    overlays or text properties.  From within Isearch: `C-t' (or
;;    `C-M-t' for regexp search).  First time, or with a prefix
;;    argument, you are prompted for the property and its values.  See
;;    the doc string of command `isearchp-char-prop-forward'.
;;
;;  * Besides relying on other code to set `face' and other text
;;    properties for use with `C-t', you can use command
;;    `isearchp-put-prop-on-region' (outside of Isearch) to add a text
;;    property to a zone of text.  By default, it applies the last
;;    property and value whose zones you searched using `C-t', but a
;;    prefix arg lets you specify the property and value to apply.
;;    This gives you an interactive way to set up zones for
;;    text-property search (`C-t').  For property `face', empty input
;;    removes all faces from the region.
;;
;;  * Option and commands to let you select the last target occurrence
;;    (set the region around it):
;;
;;    - Option `isearchp-set-region-flag' - Non-`nil' means
;;      automatically set the region around the last search target.
;;    - Command `isearchp-toggle-set-region', bound to `C-SPC' during
;;      isearch - toggle `isearchp-set-region-flag'.
;;    - Command `set-region-around-search-target' - manually set the
;;      region around the last search target.
;;
;;  * Option (`isearchp-regexp-quote-yank-flag') and command
;;    (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
;;    quoting (escaping) of regexp special characters.  With escaping
;;    turned off, you can yank text such as `^\*.*' without it being
;;    transformed to `\^\\\*\.\*'.
;;
;;  * `C-M-y' yanks the secondary selection into the search string, if
;;    you also use library `second-sel.el'.
;;
;;  * `C-_' yanks successive symbols (or words or chars) into the
;;    search string.
;;
;;  * `C-(' yanks successive sexps (or symbols or words or chars) into
;;    the search string.
;;
;;  * Command and binding to toggle (incremental) word search:
;;    `isearch-toggle-word', bound to `M-w'.
;;
;;  * Command and binding to toggle invisible-text sensitivity while
;;    searching: `isearchp-toggle-invisible, bound to `C-+'.
;;
;;  * Bindings during Isearch (the standard bindings for some of these
;;    use the Meta modifier, `M-',  instead):
;;
;;    - `next', `prior' repeat the last Isearch forward and backward
;;      (easier than using the chords `C-s', `C-r'.
;;    - `C-h' provides help on Isearch while isearching.  This library
;;      also redefines `isearch-mode-help' so that it lists all
;;      Isearch bindings and ends Isearch properly
;;    - `C-c' lets you toggle case-sensitivity while isearching.
;;      (Standard binding is `M-c'.)
;;    - `C-+' lets you toggle invisible-text sensitivity while
;;      isearching.
;;    - `C-SPC' lets you toggle setting the region around the last
;;      found occurrence.
;;    - `C-end' - go to the longest line.  Repeat to go to the longest
;;      line following that one in the buffer.  As usual, `C-g' puts
;;      you back where you started.  This binding is made only if you
;;      also use `misc-cmds.el'.
;;
;;  * Highlighting of the mismatched portion of your search string in
;;    the minibuffer (actually, Isearch uses the echo area) - that is,
;;    the portion that will be removed if you do `C-g'.  (I added this
;;    feature to vanilla Emacs in release 23.1.)
;;
;;  * `M-e' (`isearch-edit-string') automatically puts the cursor at
;;    the first mismatch position in the search string, for easy
;;    editing.  Whereas `C-g' removes all of the mismatch, this
;;    feature lets you change or insert a character or two, without
;;    losing the rest of the search string.
;;
;;  * A user option, `isearchp-initiate-edit-commands', that specifies
;;    commands whose keys will not exit Isearch but will instead
;;    initiate editing of the search string.  For example, if
;;    `backward-char' is included in the list then `C-b' and `left'
;;    will just move the cursor backward over the search string so you
;;    can change, delete, or insert chars in the middle somewhere.
;;    This makes the search string more minibuffer-like.
;;
;;  * You can, by default, select text with the mouse, then hit `C-s'
;;    etc. to search for it.  This is controlled by user option
;;    `isearchp-mouse-2-flag'.
