;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:


;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(when *is-a-mac*
  (require 'grab-mac-link))

(require 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; TODO: fail gracefully
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing %s for org." jar-name)
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))

(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))


;; Re-align tags when window shape changes
(after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(require 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (clojure . t)
     (C . t)
     (org . t)
     (shell . t)
     (scheme . t)
     (ditaa . t)
     (dot . t)
     (java . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))


(defun clear-single-linebreak-in-cjk-string (string)
  "clear single line-break between cjk characters that is usually soft line-breaks"
  (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
         (start (string-match regexp string)))
    (while start
      (setq string (replace-match "\\1\\2" nil nil string)
            start (string-match regexp string start))))
  string)

(defun ox-html-clear-single-linebreak-for-cjk (string backend info)
  (when (org-export-derived-backend-p backend 'html)
    (clear-single-linebreak-in-cjk-string string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;blog;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox-publish)
(require 'ox-html)

(setq org-publish-project-alist
      '(
        ("blog-notes"
         :base-directory "~/Workspace/blog/org/"
         :base-extension "org"
         :publishing-directory "~/Workspace/blog/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :section-numbers nil
         :html-mathjax-template "<script type=\"text/javascript\" async src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML\"></script>"
         )
        ("blog-static"
         :base-directory "~/Workspace/blog/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|jpeg"
         :publishing-directory "~/Workspace/blog/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("blog" :components ("blog-notes" "blog-static" "rss"))

        ))


;; add jquery support
(setq org-html-head-extra
      "<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js'></script>
<link rel='stylesheet' type='text/css' href='https://gongzhitaao.org/orgcss/org.css' />
<link rel='stylesheet' href='../css/custom.css' typbe='text/css'/>
<link rel='shortcut icon' type='image/x-icon' href='/favicon.ico'>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-111585106-1'></script>
<script>
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'UA-111585106-1');
</script>")

(setq org-html-preamble "
<div class='nav'>
<div class='blog' style='text-align:right'>
<a href='/index.html'> Home </a> | <a href='/gallery_books/index.html'> Books </a> | <a href='/gallery_movies/index.html'> Movies </a> | <a href='/contact.html'> About </a>
</div>
</div>")


(setq org-html-postamble "<script type=\"text/javascript\" src=\"https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js\"></script>
<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/datejs/1.0/date.min.js\"></script>
<script>
var base_url = 'https://api.github.com';
var title = document.title;
var owner = 'yydai';
var repo = 'yydai.github.io';
var search_issues = base_url + '/search/issues?q=' + title + 'in:title' + '+user:' + owner + '+label:blog'+ '+state:open';
console.log(\"search_issues = \"+ search_issues);
function test() {
  jQuery.ajax({
      type: 'GET',
      async: false,
      dataType:'json',
      url: search_issues,
      success:function(data) {
         result = data;
      }
  });
  return result;
}
var result = test();
var items = result.items[0];
if(jQuery.isEmptyObject(items)) {
    create(title);
} else {
    html_url = items.html_url;
        document.body.innerHTML +=
'<div id=\"comments\"><h2>Comments</h2><div id=\"header\">Want to leave a comment? Visit <a href=\"'+ html_url + '\"> this issue page on GitHub</a> (you will need a GitHub account).</div></div>'
}
function create(title) {
        var create_url = 'https://blog-api-server.herokuapp.com/issues?title=' + title + '&labels=blog&body=Welcome to leave comments here.&owner=yydai&repo=yydai.github.io&auth=eXlkYWk6ZGVpc3Q5MjgxNw=='
        jQuery.ajax({
      type: 'GET',
      async: false,
      dataType:'json',
      url: create_url,
      success:function(data) {
         result = data;
      }
  });
}
console.log(\"total_count = \" + result.total_count);
if(result.total_count == 1) {
    var comments_url = result.items[0].comments_url;
} else if (result.total_count == 0) {
        // create a new issue
    //create(title);
} else {
        // result not only
        alert('Cannot load the comments.');
}
function loadComments(data) {
        repo = 'github.com'
    for (var i=0; i<data.length; i++) {
      var cuser = data[i].user.login;
      var cuserlink = 'https://' + repo + '/' + data[i].user.login;
      var cbody = data[i].body_html;
      var cavatarlink = data[i].user.avatar_url;
      var cdate = Date.parse(data[i].created_at).toString('yyyy-MM-dd HH:mm:ss');
          var html_url = items.html_url + '#issuecomment-' + data[i].url.substring(data[i].url.lastIndexOf('/')+1);
      var code = '<div class=\"comment\"><div class=\"commentheader\"><div class=\"commentgravatar\">' + '<img src=\"' + cavatarlink + '\" alt=\"\" width=\"20\" height=\"20\">' + '</div><a class=\"commentuser\" href=\"'+ cuserlink + '\">' + cuser + '</a><a class=\"commentdate\" href=\"' + html_url + '\">' + cdate + '</a></div><div class=\"commentbody\">' + cbody + '</div></div>';
      $('#comments').append(code);
    }
  }
var comments_api = comments_url + '?per_page=100';
console.log(\"comments api: \" + comments_api);
$.ajax(comments_api, {
    headers: {Accept: 'application/vnd.github.full+json'},
    dataType: 'json',
    success: function(msg){
      loadComments(msg);
   }
  });
</script>
<hr />\n <div class='footer'>
© 2017 yydai<br/>
Email: dai92817@icloud.com
</div>")



(setq org-export-babel-evaluate nil)
;; (setq org-publish-use-timestamps-flag nil)



(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;;(org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "/imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

  (setq public_path (replace-regexp-in-string "org" "public_html" filename))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
                                        ; take screenshot
  (if (eq system-type 'darwin)
      (progn
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
                                                                            "\"" filename "\"" ))
        (copy-file filename public_path)
        (message "Upload '%s' finished" public_path)
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
        ))
  (if (eq system-type 'gnu/linux)
      (progn
        (call-process "import" nil nil nil filename)
        ))

  (setq relative-dir (concat "./imgs/" (file-name-nondirectory filename)))
  (if (file-exists-p filename)
      (insert (concat "#+ATTR_HTML: :width 100%\n[[file:" relative-dir "]]")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;agenda part;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-agenda-files '("~/gtd/inbox.org"))

;; (defun verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets."
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'verify-refile-target)

;; (setq org-refile-targets '((nil :maxlevel . 5)
;;                            ("~/gtd/inbox.org" :maxlevel . 1)))



(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(setq org-agenda-span 'day)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(s)" "BLOCKED" "REVIEW" "|" "DONE(d)")))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("DOING" :foreground "yellow" :weight bold)
              ("BLOCKED" :foreground "red" :weight bold)
              ("REVIEW" :foreground "orange" :weight bold)
              )))


;; Fast todo selection allows changing from any task todo state to any other state directly
;; by selecting the appropriate key from the fast todo selection key menu.
(setq org-use-fast-todo-selection t)


;; Then each time you turn an entry from a TODO (not-done) state into any of the DONE
;; states, a line ‘CLOSED: [timestamp]’ will be inserted just after the headline.
;; http://orgmode.norg/manual/Closing-items.html#Closing-items
(setq org-log-done 'time)


;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "DOING")


(setq org-clock-out-switch-to-state "DONE")

;; tags
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@buy" . ?b)
                      ("@bossurgent" . ?u)
                      ("@study" . ?s)))


;; clock
;;To save the clock history across Emacs sessions, use
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-default-notes-file (concat org-directory "~/gtd/notes.org"))

;; Capture templates
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ))

;; In order to include entries from the Emacs diary into Org mode's agenda
(setq org-agenda-include-diary t
      diary-file (locate-user-emacs-file "~/gtd/diary.org")
      org-agenda-diary-file 'diary-file)


(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
(setq org-agenda-window-setup 'current-window)

;; diary for chinese birthday
;; https://emacs-china.org/t/topic/2119/14
(defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))


(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays
              ))


;; pomodoro 通知功能
(defun notify-osx (title message)
  (notifications-notify
   :title title
   :body message
   :app-name "Emacs"
   :urgency "critical"
   :app-icon "/home/yingdai/gtd/bulb-curvy-flat.png"
   ))


(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (notify-osx "Pomodoro completed!" "Time for a break.")))


(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))


(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))


(add-hook 'org-pomodoro-killed-hook
          (lambda ()
            (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))


(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(defun my-appt-display (min-to-app new-time msg)
  (notify-osx
   (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "%s" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))


;; like search study, we can use C-c a s s command
;; http://sachachua.com/blog/2013/06/how-i-use-emacs-org-mode-for-my-weekly-reviews/
(setq org-agenda-custom-commands
      '(("x" agenda)
        ("d" todo "DONE")
        ("W" todo-tree "BLOCKED")
        ("gs" tags "@study")
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
          (todo "TODO") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "DONE")))
        ("v" tags-todo "@bossurgent")
        ("U" tags-tree "@bossurgent")
        ("f" occur-tree "\\<FIXME\\>")
        ("h" . "HOME+Name tags searches") ; description for "h" prefix
        ("ss" tags "@study")
        ("su" tags "@bossurgent")
        ("sw" tags "@work")
        ("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo "")))
        ))

(setq org-clock-out-when-done t)
(setq org-clock-report-include-clocking-task t)
(setq org-clock-continuously 'nil)

(defun archive-when-done ()
  "Archive current entry if it is marked as DONE (see `org-done-keywords')."
  )
(add-hook 'org-after-todo-state-change-hook
          'archive-when-done)

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")


(setq org-archive-location (concat "~/gtd/archive/archive-" (format-time-string "%Y%m" (current-time)) ".org_archive::"))

;; pomodoro setting
(defun pomodoro-start ()
  "Starts and automatically clocks out a Pomodoro unit of 30 minutes."
  (interactive)
  (org-pomodoro)
  (org-clock-in)
  (message "Starting pomodoro cycle of 30 minutes.")
  )


(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (org-clock-out)))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (pomodoro-start)))

(global-set-key '[f5] 'pomodoro-start)

;; simplifying clock-in / clock-out
(global-set-key '[f5] 'org-clock-in)
(global-set-key '[f6] 'org-clock-out)

;; https://stackoverflow.com/questions/22394394/orgmode-a-report-of-tasks-that-are-done-within-the-week


(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(defun my-beamer-bold (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

(add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)

(defun my-beamer-structure (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))

(add-to-list 'org-export-filter-strike-through-functions 'my-beamer-structure)


(add-to-list 'org-agenda-custom-commands
             '("W" "Weekly review"
               agenda ""
               ((org-agenda-start-day "-7d")
                (org-agenda-span 7)
                (org-agenda-start-on-weekday 1)
                (org-agenda-start-with-log-mode '(closed))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(provide 'init-org)
;;; init-org.el ends here
