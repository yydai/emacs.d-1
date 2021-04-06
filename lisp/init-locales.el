;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match-p "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(global-unset-key (kbd "C-,"))

(global-set-key (kbd "C-,") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-char)

(defun yd-toggle-move-indent ()
  "Test."
  (interactive)
  (if (get 'yd-toggle-move-indent 'state)
      (progn
        (call-interactively 'move-beginning-of-line)
        (put 'yd-toggle-move-indent 'state nil))
    (progn
      (call-interactively 'back-to-indentation)
      (put 'yd-toggle-move-indent 'state t))))

;; unbind org model key
(require 'bind-key)
(unbind-key "C-a" org-mode-map)
(global-set-key (kbd "C-a") 'yd-toggle-move-indent)

;; delete trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set cursor
(setq-default cursor-type 'bar)
(set-cursor-color "red")

;; (require 'pyim)
;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;; (setq default-input-method "pyim")

;; (setq pyim-page-length 5)

;; IELM    REPL of Elisp
(defun my-ielm-mode-defaults ()
  (turn-on-eldoc-mode))

(setq my-ielm-mode-hook 'my-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda () (run-hooks 'my-ielm-mode-hook)))
(add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
(setq ielm-dynamic-return t)

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
	(multi-term)
      (switch-to-buffer b))))

(defun term-send-kill-line ()
  "Kill line in multi-term mode with the possibility to paste it like in a normal shell."
  (interactive)
  (kill-line)
  (term-send-raw-string "\C-k"))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

(setq-default indent-tabs-mode nil)

(tool-bar-mode 0)

(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-toc-command "\\tableofcontents \\clearpage")
(setq reftex-default-bibliography '("~/Downloads/manuscript.bib"))


(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(add-to-list 'org-src-lang-modes '("latex-macros" . latex))

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))

(provide 'init-locales)
;;; init-locales.el ends here
