(defun t1 ()
  "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (shell (concat "*shell " (projectile-project-name) "*"))))

(defun t2 ()
  (interactive)
  (shell "terminal"))


(provide 'init-shell)
