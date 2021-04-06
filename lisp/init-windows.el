;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)



;; Make "C-x o" prompt for a target window when there are more than 2
(require 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun yd-toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'yd-toggle-delete-other-windows)



;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun yd-split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'yd-split-window)



;; (defun sanityinc/toggle-current-window-dedication ()
;;   "Toggle whether the current window is dedicated to its current buffer."
;;   (interactive)
;;   (let* ((window (selected-window))
;;          (was-dedicated (window-dedicated-p window)))
;;     (set-window-dedicated-p window (not was-dedicated))
;;     (message "Window %sdedicated to %s"
;;              (if was-dedicated "no longer " "")
;;              (buffer-name))))

;; (global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)


;; (unless (memq window-system '(nt w32))
;;   (windmove-default-keybindings 'control))


(provide 'init-windows)
;;; init-windows.el ends here
