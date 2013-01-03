(require 'setup-ediff)

(defun my-save-frame ()
  (when (fboundp 'my-ediff-bsh) (my-ediff-bsh)))

(defun my-default-frame (&optional full)
  (interactive)
  (my-size-frame)
  (default-split))

(defun size-frame (&optional w h)
  "with no args, set frame to default w/h"
  (interactive)
  (when (eq nil w) (setq w (cdr (assoc 'width default-frame-alist))))
  (when (eq nil h) (setq h (cdr (assoc 'height default-frame-alist))))
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) w h)
  (my-save-frame))

(defun maximize-frame ()
  (interactive)
  (size-frame 270 76)
  (my-save-frame))

(defun fullscreen (&optional f)
  (interactive)
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
  (scroll-bar-mode -1) ;; no scroll bar
  (if (equal system-type 'darwin)
                                        ;          (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth))
      (ns-toggle-fullscreen)
    (if (equal system-type 'windows-nt)
        (w32-send-sys-command 61488)
      (progn
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                               '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                               '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))))
  (my-save-frame))

(defun default-size-frame (w h)
  (interactive)
  (add-to-list 'default-frame-alist (cons 'height h))
  (add-to-list 'default-frame-alist (cons 'width w))
  (my-save-frame)
  )

(defun default-split ()
  (interactive)
  (delete-other-windows)
  (if (eq (emacs-version) "GNU Emacs 24.0.90.2 (x86_64-apple-darwin11.1.0, NS apple-appkit-1138.00)
 of 2011-10-11 on graehl.local")
      (split-window-side-by-side)
    (split-window-right))
  )

(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame.
The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame
simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (<  1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (when (fboundp 'iswitchb-close) (iswitchb-close)))
