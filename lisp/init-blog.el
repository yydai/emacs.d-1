(require 'simple-httpd)
(require 'browse-url)

(defconst python-script-path "~/Workspace/blog/org/t.py")
(defconst localhost "127.0.0.1")
(defconst base_path "~/Workspace/blog/org/")
(defconst port 8080)



(defmacro define-background-function-wrapper (bg-function fn)
  (let ((is-loading-sym (intern (concat "*" (symbol-name bg-function) "-is-loading*"))))
    `(progn
       (defvar ,is-loading-sym nil)
       (defun ,bg-function ()
         (interactive)
         (when ,is-loading-sym
           (message ,(concat (symbol-name fn) " is already loading")))
         (setq ,is-loading-sym t)
         (make-thread (lambda ()
                        (unwind-protect
                            (,fn)
                          (setq ,is-loading-sym nil))))))))

(defun blog-open ()
  (interactive)
  (setq base (concat base_path "index.org"))
  (find-file base))

;;search the file of my blog
(defun blog-find ()
  (interactive)
  (call-interactively (lambda () (interactive) (bjm-deft base_path))))

;; set the http server
(setq httpd-root "~/Workspace/blog/public_html/")
(httpd-serve-directory httpd-root) ;; old way  use python instead
(httpd-start) ;; old way

;; run python script
(defun my-run-python-script (name)
  (shell-command-to-string (format "python3 %s" name)))


(defun my/run-command (command-format-string)
  "command-format-string: "
  (shell-command-to-string command-format-string))



;;(setq tdd default-directory)
;;(setq default-directory httpd-root)
                                        ;(my-run-python-script "-m http.server 8087 --bind 127.0.0.1")
                                        ;(setq default-directory tdd)

;; the port of 8087 need open the customize and set the to 8087
(defun blog-preview(&optional publish-current)
  "Preivew blog."
  (interactive
   (list (y-or-n-p "Publish current file(y), or publish all(n)?")))

  (if publish-current (org-publish-current-file)
    (org-publish-all))

  (if publish-current
      (progn
        (if (equalp (file-name-nondirectory buffer-file-name) "index.org")
            (setq subpath "/index")
          (setq subpath (substring buffer-file-name (string-match-p
                                                     "/[[:word:]_?]+/[[:word:]-?]+.org$"
                                                     buffer-file-name) (- (length buffer-file-name) 4))))
        (setq url (format "http://%s:%d%s%s" localhost port subpath ".html")))

    (setq url (format "http://%s:%d" localhost port)))

  ;; (my-run-python-script python-script-path) ;;; replace index.org css file. DON'T DELETE
  (browse-url url))


(defun blog-stop-preview ()
  (interactive)
  (httpd-stop))


(defun auto-sequence (format start end)
  (interactive "sSequence format is? \nnEnter start number: \nnEnter end number:")
  (progn
    (kmacro-set-format format)
    (kmacro-set-counter start)
    (while (< start (+ 1 end))
      (execute-kbd-macro (read-kbd-macro "<f3> RET"))
      (setq start (+ 1 start)))
    ))

(defun blog-create (title &optional dir)
  "nblog is used to create a new blog in the default directory(~/workspace/blog/org/).
And you should know it needs the blog name and the directory that to restore the file.
reference: https://www.emacswiki.org/emacs/InteractiveFunction
and https://learnxinyminutes.com/docs/elisp/
and http://ergoemacs.org/emacs/elisp_buffer_file_functions.html"

  (interactive "sBlog title to show? \nsDirectory is?")
  (setq base "~/Workspace/blog/org/")
  (setq filename
        (concat base dir  "/" title ".org"))
  (if (file-exists-p filename)
      (find-file filename)
    (let ((buf (generate-new-buffer title)))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (insert (concat "#+TITLE: " "\n\n-------\n"))
      (write-file filename)
      (goto-char (+ (length "#+TITLE: ") 1))
      )))


(defun publish-site ()
  "Push the site to github, and then open my blog site."
  (interactive)
  (let ((path "~/Workspace/blog/public_html/"))
    (if (= 0 (call-process-shell-command (format "cd %s;make" path)))
        (progn
          (browse-url "http://yydai.github.io")
          (httpd-stop))
      (message "Publish site failed. Please manually do this."))))


(defun blog-site ()
  (interactive)
  (browse-url "https://yydai.github.io"))



;;; download images from server
(defun my/blog-download-image (link)
  (interactive "sUrl: ")
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

  (shell-command-to-string (format "wget %s -O %s" link filename))
  (message "download image success")
  ;; public_html 中 imgs 文件夹可能不存在，判断下 TODO ??
  ;; code here
  ;;
  (setq public_path (replace-regexp-in-string "org" "public_html" filename))
  (copy-file filename public_path)
  (setq relative-dir (concat "./imgs/" (file-name-nondirectory filename)))
  (if (file-exists-p filename)
      (insert (concat "#+ATTR_HTML: :width 70%\n[[file:" relative-dir "]]"))))

(defun my/org-download-image (link)
  (interactive "sUrl: ")
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command-to-string (format "wget %s -O %s" link filename))
  (message "download image success")
  (setq relative-dir (concat "./imgs/" (file-name-nondirectory filename)))
  (if (file-exists-p filename)
      (insert (concat "#+ATTR_HTML: :width 70%\n[[file:" relative-dir "]]")))
  )

;; advice org mode code highlight
(defun org-html-src-block2 (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (code (org-html-format-code src-block info))
          (label (let ((lbl (and (org-element-property :name src-block)
                                 (org-export-get-reference src-block info))))
                   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre><code class=\"example\"%s>\n%s</code></pre>" label code)
        (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                ;; Build caption.
                (let ((caption (org-export-get-caption src-block)))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            (format
                             (org-html--translate "Listing %d:" info)
                             (org-export-get-ordinal
                              src-block info nil #'org-html--has-caption-p)))))
                      (format "<label class=\"org-src-name\">%s%s</label>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (format "<pre class=\"%s\"><code class=\"%s\"%s>%s</code></pre>"
                        (concat "src src-" lang) lang label code))))))

(advice-add 'org-html-src-block :override 'org-html-src-block2)

;; (require 'org)
;; (advice-add 'org-html-src-block :override 'org-html-src-block2)
;; (advice-remove 'org-html-src-block 'org-html-src-block2)

;; another method is eval-after-load
;; https://stackoverflow.com/questions/15717103/preferred-method-of-overriding-an-emacs-lisp-function
;; (eval-after-load "telnet"
;;   '(defun telnet-initial-filter (proc string)
;;      ...))


;; this is a very useful feature for writing blogs
;; It will construct the blog link of index page.
(defun blog-path-for-index ()
  (interactive)
  (setq subpath (substring buffer-file-name (string-match-p
                                             "/[[:word:]_?]+/[[:word:]-?]+.org$"
                                             buffer-file-name) (length buffer-file-name)))
  (goto-line 1)
  (setq url (format "[[.%s][%s]]" subpath
                    (buffer-substring-no-properties (search-forward "#+TITLE: ") (line-end-position) )))

  (kill-new url)
  (message "Will open index file, paste the content to somewhere....")
  ;; sleep for 3 second
  ;;(sit-for 1)
  (find-file "../index.org")
  )

(defun blog-url-search ()
  (interactive)
  (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files ".."))))
         (selected-file (completing-read "Select article: " files nil t)))
    (insert (format "[[%s][]]" selected-file))))


;; thing-at-point
(defun blog-cp-image-tofolder ()
  (interactive)
  (setq filename (file-name-nondirectory  (thing-at-point 'filename)))
  (setq source (file-name-directory buffer-file-name))
  (setq public_path (replace-regexp-in-string "org" "public_html" source))
  (unless (file-exists-p (concat public_path "/imgs"))
    (make-directory (concat public_path "/imgs")))
  (copy-file (concat source "/imgs/" filename) (concat public_path "/imgs"))
  )

(provide 'init-blog)
