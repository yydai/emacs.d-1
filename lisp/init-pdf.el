(require 'pdf-tools)
(require 'pdf-occur)
(require 'pdf-history)
(require 'pdf-links)
(require 'pdf-outline)
(require 'pdf-annot)
(require 'pdf-sync)

(use-package pdf-tools
             :ensure t
             :config
             (custom-set-variables
              '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
             (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)

;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)
;; use normal isearch
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; midnite mode hook
(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs

(setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; set the green profile as default (see below)

(defun pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1)
  )

;; change midnite mode colours functions
(defun pdf-midnite-original ()
  "Set pdf-view-midnight-colors to original colours."
  (interactive)
  (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#839496" . "#292b2e" )) ; amber
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-green ()
  "Set pdf-view-midnight-colors to green on black."
  (interactive)
  (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-colour-schemes ()
  "Midnight mode colour schemes bound to keys"
  (local-set-key (kbd "!") (quote pdf-no-filter))
  (local-set-key (kbd "@") (quote pdf-midnite-amber))
  (local-set-key (kbd "#") (quote pdf-midnite-green))
  (local-set-key (kbd "$") (quote pdf-midnite-original))
  (local-set-key (kbd ".") (quote pdf-view-last-page))
  (local-set-key (kbd ",") (quote pdf-view-first-page))
  (local-set-key (kbd "g") (quote pdf-view-goto-page))
  (local-set-key (kbd "P") (quote pdf-view-previous-page))
  (local-set-key (kbd "N") (quote pdf-view-next-page))
  )
(add-hook 'pdf-view-mode-hook 'pdf-midnite-colour-schemes)

;; http://pragmaticemacs.com/
;; (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
;; (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
;; (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

(provide 'init-pdf)
