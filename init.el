;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)




(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; check system type
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))




(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package command-log-mode)


(require 'init-diminish) ;; DONE
;; (require 'init-frame-hooks)
;; (require 'init-xterm)

(require 'init-themes)   ;; DONE
(require 'init-osx-keys) ;; DONE  FOR MACOS
(require 'init-grep)     ;; TODO
(require 'init-uniquify) ;; DONE
(require 'init-ibuffer)  ;; DONE
(require 'init-flycheck) ;; DONE
(require 'init-recentf)  ;; DONE
(require 'init-ivy)      ;; DONE
(require 'init-hippie-expand) ;; DONE
(require 'init-company)  ;; DONE
(require 'init-windows)  ;; DONE
(require 'init-mmm)      ;; DONE

(require 'init-editing-utils)  ;; TODO
(require 'init-whitespace)   ;; DONE

(require 'init-git)  ;; TODO with magit
(require 'init-github) ;; DONE

(require 'init-projectile) ;; DONE

(require 'init-compile) ;; DONE
(require 'init-textile) ;; DONE
(require 'init-markdown) ;; DONE
(require 'init-csv) ;; DONE
(require 'init-javascript)  ;; DONE
(require 'init-org) ;; DONE
(require 'init-python) ;; TODO
(require 'init-sql) ;; DONE
;; (require 'init-hive)
(require 'init-docker) ;; DONE
(require 'init-paredit) ;; ..
(require 'init-lisp) ;; DONE
(require 'init-slime) ;; DONE common lisp
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)
(require 'init-dash)
;; Extra packages which don't require any configuration


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)
;; (require 'init-blog)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)


;; load myself packages
;; for note taking
(require 'init-deft)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(require 'aweshell)


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PYTHONPATH")

(require 'highlight-parentheses)


(add-to-list 'load-path
             "~/.emacs.d/snippets")
(require 'yasnippet)
(yas-global-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'init-shell)
(require 'awesome-tab)
(awesome-tab-mode t)
(when (not (display-graphic-p))
  (setq frame-background-mode 'dark))

(require 'multi-term)


;; disable scroll bar
(scroll-bar-mode -1)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/snails") ; add snails to your load-path

;; (require 'snails)
;; (snails '(snails-backend-buffer snails-backend-recentf) t)

(require 'thing-edit)



(require 'init-rsync)
;; (add-hook 'sql-mode-hook (lambda ()
;;                            "Turn on `bar-minor-mode' mode."
;;                            (auto-rsync-mode t)))

(auto-rsync-mode nil)
(setq auto-rsync-dir-alist
      '(("/home/yingdai/Workspace/Qunar/Gitlab/hotel-analysis-jobs" . "tr:/home/q/home/yingying.dai/")
        ))

;;https://stackoverflow.com/questions/11272236/how-to-make-formule-bigger-in-org-mode-of-emacs
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))


;; (add-to-list 'company-backends #'company-tabnine)

;; (setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
;; (setq company-show-numbers t)
;; (require 'nox)

;; (dolist (hook (list
;;                'js-mode-hook
;;                'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (nox-ensure))))


;;(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
(require 'org-ref)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-export-allow-bind-keywords t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
