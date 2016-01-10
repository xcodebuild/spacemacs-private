;;; packages.el --- codefalling Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq codefalling-packages
      '(
        ;; package names go here
        org-mac-link
        fcitx
        ;; realgud
        org-bullets
        org
        uimage
        org-octopress
        hl-sexp
        aggressive-indent
        elfeed
        sws-mode
        org-caldav
        ))

;; List of packages to exclude.
(setq codefalling-excluded-packages '())

;; For each package, define a function codefalling/init-<package-name>
;;


(defun codefalling/init-org-caldav ()
  (use-package org-caldav
    ))

(defun codefalling/init-org-mac-link  ()
  (use-package org-mac-link
    :defer t
    :init (add-hook'org-mode-hook (lambda () (require 'org-mac-link)))
    ))

(defun codefalling/init-org-octopress ()
  (use-package org-octopress
    :config
    (progn
      (setq org-octopress-directory-top       "~/blog/source")
      (setq org-octopress-directory-posts     "~/blog/source/_posts")
      (setq org-octopress-directory-org-top   "~/blog/source")
      (setq org-octopress-directory-org-posts "~/blog/source/_posts")
      )

    ;; rewrite in org-octopress.el
    (defun org-octopress--summary-table (contents keymap)
      (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
        (ctbl:create-table-component-region
         :param param
         :width  nil
         :height nil
         :keymap keymap
         :model
         (make-ctbl:model
          :data contents
          :sort-state '(-1 2)
          :column-model
          (list (make-ctbl:cmodel
                 :title "Date"
                 :sorter 'ctbl:sort-string-lessp
                 :min-width 10
                 :align 'left)
                (make-ctbl:cmodel
                 :title "Category"
                 :align 'left
                 :sorter 'ctbl:sort-string-lessp)
                (make-ctbl:cmodel
                 :title "Title"
                 :align 'left
                 :min-width 40
                 :max-width 140)
                )))))
    (define-key org-octopress-summary-mode-map "w" 'codefalling/hexo-org-new-open-post)

    (defun org-octopress--scan-post ()
      (mapcar
       (lambda (filename)
         (org-jekyll-property
          '(:date
            :jekyll-categories
            :title
            :input-file)
          filename))
       (directory-files
        (expand-file-name
         org-octopress-directory-org-posts) t "^.*\\.org$")))

    (defun org-octopress (&optional title)
      "Org-mode and Octopress."
      (interactive)
      (setq org-octopress-summary-buffer (get-buffer-create "Octopress"))
      (switch-to-buffer org-octopress-summary-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (org-octopress--summary-header title))
      (save-excursion
        (setq org-octopress-component (org-octopress--summary-table
                                       (org-octopress--scan-post) org-octopress-summary-mode-map)))
      (ctbl:cp-add-click-hook
       org-octopress-component
       (lambda ()
         (find-file (nth 3 (ctbl:cp-get-selected-data-row org-octopress-component)))))
      (org-octopress-summary-mode)
      (ctbl:navi-goto-cell
       (ctbl:find-first-cell (ctbl:component-dest org-octopress-component)))
      )

    ;; rewrite in ox-jekyll.el
    (defcustom org-jekyll-date ""
      "Default date used in Jekyll article."
      :group 'org-export-jekyll
      :type 'string)
    (org-export-define-derived-backend'jekyll 'html
                                              :export-block '("HTML" "JEKYLL")
                                              :menu-entry
                                              '(?j "Jekyll: export to HTML with YAML front matter."
                                                   ((?H "As HTML buffer" org-jekyll-export-as-html)
                                                    (?h "As HTML file" org-jekyll-export-to-html)))
                                              :translate-alist
                                              '((template . org-jekyll-template) ;; add YAML front matter.
                                                (src-block . org-jekyll-src-block)
                                                (inner-template . org-jekyll-inner-template)) ;; force body-only
                                              :options-alist
                                              '((:jekyll-layout "LAYOUT" nil org-jekyll-layout)
                                                (:jekyll-categories "CATEGORIES" nil org-jekyll-categories)
                                                (:jekyll-tags "TAGS" nil org-jekyll-tags)
                                                (:date "DATE" nil org-jekyll-date)
                                                (:jekyll-published "PUBLISHED" nil org-jekyll-published)
                                                (:jekyll-comments "COMMENTS" nil org-jekyll-comments)))
    ))

(defun codefalling/init-uimage ()
  (use-package uimage
    :defer t
    ))

(defun codefalling/init-realgud ()
  (use-package realgud
    :defer t
    :init (load-library "realgud")
    ))

(defun codefalling/init-erc-colorize ()
  (use-package colorize
    :defer t
    :init (add-hook 'erc-mode-hook 'erc-colorize-mode)
    ))
(defun codefalling/init-hl-sexp ()
  (use-package hl-sexp
    :defer t
    :init
    (progn
      (add-hook 'lisp-mode-hook 'hl-sexp-mode)
      (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
      )))

(defun codefalling/post-init-aggressive-indent ()
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)
    ))

(defun codefalling/init-fcitx  ()
  (use-package fcitx
    :defer t
    :init
    (fcitx-aggressive-setup)
    ))


(defun codefalling/post-init-org-bullets ()
  (setq org-bullets-bullet-list '("☰" "☷" "⋗" "⇀")))

(defun codefalling/post-init-org ()
  (setq org-agenda-dir "~/Dropbox/org-notes")
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-gtd-archive (expand-file-name "gtd.org_archive" org-agenda-dir))
  (setq org-agenda-files `(,org-agenda-file-gtd ,org-agenda-file-gtd-archive))

  (setq org-default-notes-file org-agenda-file-gtd)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INBOX(i)" "|" "WAITTING(w)" "NOTE(n)""DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "CANCELLED(c)")))

  (setq org-refile-targets
        '(("gtd.org" :maxlevel . 1)))

  (setq org-log-into-drawer t)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Daily Tasks")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
          ("i" "Inbox" entry (file+headline org-agenda-file-gtd "Inbox")
           "* INBOX %?\n  %i\n"
           :empty-lines 1)
          ("n" "Quick Notes" entry (file+headline org-agenda-file-gtd "Quick notes")
           "* NOTE %?\n  %i\n %U"
           :empty-lines 1)
          ("b" "Blog Ideas" entry (file+headline org-agenda-file-gtd "Blog Ideas")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("w" "work" entry (file+headline org-agenda-file-gtd "Programming")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("j" "Journal Entry"
           entry (file+datetree "~/Dropbox/org-notes/journal.org")
           "* %?"
           :empty-lines 1)))

  (setq org-agenda-custom-commands
        '(
          ("i" "Inbox" todo "INBOX")
          ("w" . " 任务安排 ")
          ("wa" " 重要且紧急的任务 " tags-todo "+PRIORITY=\"A\"")
          ("wb" " 重要且不紧急的任务 " tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" " 不重要且紧急的任务 " tags-todo "+PRIORITY=\"C\"")
          ("b" "Blog" tags-todo "BLOG")
          ("p" . " 项目安排 ")
          ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"programming\"")
          ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"codefalling\"")
          ("W" "Weekly Review"
           ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
            (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
            ))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook'org-after-todo-statistics-hook 'org-summary-todo)
  ;; used by org-clock-sum-today-by-tags
  (defun filter-by-tags ()
    (let ((head-tags (org-get-tags-at)))
      (member current-tag head-tags)))

  (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
    (interactive "P")
    (let* ((timerange-numeric-value (prefix-numeric-value timerange))
           (files (org-add-archive-files (org-agenda-files)))
           (include-tags'("PROG" "EMACS" "DREAM" "WRITING" "MEETING" "BLOG"
                          "LIFE" "PROJECT"))
           (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
           (output-string "")
           (tstart (or tstart
                       (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                       (org-time-today)))
           (tend (or tend
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                     (+ tstart 86400)))
           h m file item prompt donesomething)
      (while (setq file (pop files))
        (setq org-agenda-buffer (if (file-exists-p file)
                                    (org-get-agenda-file-buffer file)
                                  (error "No such file %s" file)))
        (with-current-buffer org-agenda-buffer
          (dolist (current-tag include-tags)
            (org-clock-sum tstart tend'filter-by-tags)
            (setcdr (assoc current-tag tags-time-alist)
                    (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
      (while (setq item (pop tags-time-alist))
        (unless (equal (cdr item) 0)
          (setq donesomething t)
          (setq h (/ (cdr item) 60)
                m (- (cdr item) (* 60 h)))
          (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
      (unless donesomething
        (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
      (unless noinsert
        (insert output-string))
      output-string))

  (eval-after-load 'org
    '(progn
       (global-set-key (kbd "C-c a") 'org-agenda)
       (define-key org-mode-map (kbd "s-p") 'org-priority)
       (define-key global-map (kbd "<f9>") 'org-capture)
       (global-set-key (kbd "C-c b") 'org-iswitchb)
       (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
       (evil-leader/set-key-for-mode'org-mode
        "owh" 'plain-org-wiki-helm
        "owf" 'plain-org-wiki)
       (require 'ob-js)
       (require 'ob-shell)
       )
    )
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  ;; Sync with google calander
  (setq org-caldav-url "https://www.google.com/calendar/dav")
  (setq org-caldav-calendar-id "code.falling@gmail.com")
  (setq org-caldav-files org-agenda-files)
  (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")

  )

(defun codefalling/post-init-elfeed ()
  (setq-default elfeed-search-filter "@1-week +unread")
  )

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
