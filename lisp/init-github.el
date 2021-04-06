
;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(require 'yagist)
(require 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(require 'github-clone)
(require 'forge)
(require 'github-review)

(provide 'init-github)
;;; init-github.el ends here
