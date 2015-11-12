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
        rainbow-blocks
        realgud
        org-bullets
        org
      ))

;; List of packages to exclude.
(setq codefalling-excluded-packages '())

;; For each package, define a function codefalling/init-<package-name>
;;
(defun codefalling/init-org-mac-link  ()
  (use-package org-mac-link
    :defer t
    :init (add-hook'org-mode-hook (lambda () (require 'org-mac-link)))
    ))

(defun codefalling/init-rainbow-blocks  ()
  (use-package rainbow-blocks
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'rainbow-blocks-mode)))

(defun codefalling/init-realgud  ()
  (use-package realgud
    :defer t
    :init (load-library "realgud")
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
  (setq org-agenda-files (quote ("~/.spacemacs.d/org-notes" "~/.spacemacs.d/org-notes/inbox.txt")))
  (setq org-default-notes-file "~/.spacemacs.d/org-notes/gtd.org")
  (setq org-todo-keywords
        '((sequence "INBOX(i)" "TODO(t)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "CANCELED(c)")))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/.spacemacs.d/org-notes/gtd.org" "Daily Tasks")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
          ("i" "Inbox" entry (file+headline "~/.spacemacs.d/org-notes/inbox.txt" "Inbox")
           "* INBOX %?\n  %i\n"
           :empty-lines 1)
          ("n" "notes" entry (file+headline "~/.spacemacs.d/org-notes/notes.org" "Quick notes")
           "* TODO [#C] %?\n  %i\n %U"
           :empty-lines 1)
          ("b" "Blog Ideas" entry (file+headline "~/.spacemacs.d/org-notes/notes.org" "Blog Ideas")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("w" "work" entry (file+headline "~/.spacemacs.d/org-notes/gtd.org" "Programming")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("c" "Chrome" entry (file+headline "~/.spacemacs.d/org-notes/notes.org" "Quick notes")
           "* TODO %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
           :empty-lines 1)
          ("l" "links" entry (file+headline "~/.spacemacs.d/org-notes/notes.org" "Quick notes")
           "* TODO %?\n  %i\n %a \n %U"
           :empty-lines 1)
          ("j" "Journal Entry"
           entry (file+datetree "~/.spacemacs.d/org-notes/journal.org")
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
           (include-tags'("PROG" "EMACS" "DREAM" "WRITING" "MEETING"
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

  (eval-after-load'org
    '(progn
       (global-set-key (kbd "C-c a") 'org-agenda)
       (define-key org-mode-map (kbd "s-p") 'org-priority)
       (define-key global-map (kbd "<f9>") 'org-capture)
       (global-set-key (kbd "C-c b") 'org-iswitchb)
       (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
       (evil-leader/set-key-for-mode'org-mode
         "owh" 'plain-org-wiki-helm
         "owf" 'plain-org-wiki)
       (require 'ob-js))
    )
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  )


;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
