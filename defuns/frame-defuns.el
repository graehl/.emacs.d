(defun size-frame (w h)
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) w h)
  )
(defun maximize-frame ()
  (interactive)
  (size-frame 270 76)
  )

                                        ;(global-set-key (kbd "<C-S-return>") 'maximize-frame)

(defun fullscreen (&optional f)
  (interactive)
                                        ;(maximize-frame)
  (progn
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
                                        ;  (menu-bar-mode 1)
    ;;-1) ;;no menubar
    (scroll-bar-mode -1) ;; no scroll bar
    )
  (if (equal system-type 'darwin)
                                        ;          (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth))
      (ns-toggle-fullscreen)
    (progn
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))))
