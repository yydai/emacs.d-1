(require 'diminish)

;; hide modeline contents
(use-package diminish
  :ensure t)
;; Let's hide some markers.
(diminish 'eldoc-mode)
(diminish 'org-indent-mode)
(diminish 'subword-mode)
(diminish 'page-break-lines-mode)

(provide 'init-diminish)
