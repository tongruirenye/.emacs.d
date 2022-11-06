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
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
                            (type "JOURNAL" "SUBJECT" "GOAL" "CLIP"  "INBOX(i)" "ISSUE"  "|" "ARCH")))
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
        ;;org-agenda-files (list (concat cc-org-dir "roam/daily/agenda.org"))
        org-attach-dir-relative t
        org-refile-targets '((nil . (:level . 1)))
        ))


;; org-journal
(use-package org-journal
  :ensure t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir (concat cc-org-dir "roam/daily/")
        org-journal-enable-agenda-integration t
        org-journal-file-type 'yearly
        org-journal-date-format "%Y-%m-%d %A"))

(defun cc-org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))


(defun cc-org-mode-todo-prompt ()
    (completing-read
     "开始一个任务: "
	 (sort
     (-distinct
      (org-map-entries
       (lambda ()
	 (org-element-property :title (org-element-at-point)))
       "TODO=\"TODO\"|SCHEDULED=<today>"
       'agenda))
     #'string<)))
   
(setq org-capture-templates '(("j" "Journal" plain (function cc-org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)
                              ("t" "Task" plain (function cc-org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%(cc-org-mode-todo-prompt) :plan:\n%i%?"
                               :clock-in t :immediate-finish t)
                              ("a" "Adhoc" plain (function cc-org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title} :adhoc:\n%i%?"
                               :clock-in t :immediate-finish t)
                              ("c" "Task" plain (clock)
                               "*** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
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


(defun cc-clock-hook ()
  (org-save-all-org-buffers))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . cc-clock-hook)
         (org-clock-out .cc-clock-hook)))

(provide 'setup-org)

