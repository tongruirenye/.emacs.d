;;
;; Org
;;

(require 'setup-config)


;; Org
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :init
  (setq org-directory cc-org-dir)
  :config
  (setq org-todo-keywords '((sequence "INBOX(i)"  "TODO(t)" "REVIEW(r)" "|" "DONE(d!)")
                            (type "JOURNAL" "SUBJECT" "GOAL" "CLIP" "ISSUE"  "|" "ARCH")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("REVIEW" . "blue")
	      ("INBOX" . "spring green")
	      ("DONE" . "gray")))
  (setq org-use-fast-todo-selection t
        org-adapt-indentation t
        org-clock-into-drawer t
        org-hide-leading-stars t
        org-log-into-drawer t
        org-log-done 'time
        org-deadline-warning-days 3
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>")
        org-agenda-files (list (concat cc-org-dir "roam/daily/agenda.org"))
        org-attach-dir-relative t
        org-refile-targets '((org-agenda-files . (:tag . "project"))
                             (nil . (:tag . "看板"))
                             )
        ))

(defvar cc-agenda-log-file
  (concat cc-org-dir "roam/daily/timelog.org"))

(defun cc-org-mode-project-prompt ()
    (completing-read
     "项目: "
	 (sort
     (-distinct
      (org-map-entries
       (lambda ()
	 (org-element-property :title (org-element-at-point)))
       "+LEVEL=2+project"
       'agenda))
     #'string<)))

(defun cc-org-mode-todo-prompt ()
    (completing-read
     "任务: "
	 (sort
     (-distinct
      (org-map-entries
       (lambda ()
	 (org-element-property :title (org-element-at-point)))
       "TODO=\"TODO\"|SCHEDULED=<today>"
       'agenda))
     #'string<)))

(cl-defun cc-org-mode-find-project-node (&key
					   (project (cc-org-mode-project-prompt))
					   (within_headline (format-time-string "%Y-%m-%d %A")))
    (with-current-buffer (find-file-noselect cc-agenda-log-file)
      (let ((existing-position (org-element-map
				   (org-element-parse-buffer)
				   'headline
				 (lambda (hl)
				   (and (=(org-element-property :level hl) 4)
					(string= project (plist-get (cadr hl) :raw-value))
					(member "project" (org-element-property :tags hl))
					(string= within_headline
						 (plist-get
						  (cadr (car (org-element-lineage hl))) :raw-value))
					(org-element-property :end hl)))
				 nil t)))
	(if existing-position
	    (goto-char existing-position)
	  (progn
	    (end-of-buffer)
	    (insert (concat "\n**** " project " :project:\n\n")))))))

(setq org-capture-templates
      '(("p" "Project"
	    entry (file+olp+datetree cc-agenda-log-file)
	    "* %(cc-org-mode-project-prompt) :project:\n\n%?"
	    :empty-lines-before 1
	    :empty-lines-after 1)
	  ("a" "Task"
	   plain (file+function cc-agenda-log-file cc-org-mode-find-project-node)
	   "***** %? :task:\n\n"
	   :empty-lines-before 1
	   :empty-lines-after 1
       :clock-in t)
      ("t" "Todo"
	   plain (file+function cc-agenda-log-file cc-org-mode-find-project-node)
	   "***** %(cc-org-mode-todo-prompt) :task:\n\n"
	   :empty-lines-before 1
	   :empty-lines-after 1
       :clock-in t
       :immediate-finish t)
	  ))

;; Roam
(use-package org-roam
 :ensure t
 :hook (after-init . org-roam-db-autosync-enable)
 :bind (("C-c n l" . org-roam-buffer-toggle)
        ("C-c n f" . org-roam-node-find)
        ("C-c n g" . org-roam-graph)
        ("C-c n i" . org-roam-node-insert)
        ("C-c n c" . org-roam-capture)
        ("C-c n j" . org-roam-dailies-capture-today))
 :init
 (setq org-roam-directory (concat cc-org-dir "roam")
       org-roam-v2-ack t
       org-roam-dailies-directory "daily/")
 (setq org-roam-capture-templates
       '(("d" "default" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ))
 :config
 (unless (file-exists-p org-roam-directory)
   (make-directory org-roam-directory)))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . (lambda () ((org-save-all-org-buffers))))
         (org-clock-out . (lambda () ((org-save-all-org-buffers))))
  ))

(use-package eaf
  :load-path "~/.emacs.d/elpa/emacs-application-framework")

(provide 'setup-org)

