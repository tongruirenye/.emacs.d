
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(i)" "CHECK(c)" "|" "DONE(d)")))


(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("INPROGRESS" . "forest green")
	("CHECK" . "orange")
	("DONE" . "gray")))

	       
(setq org-refile-targets (quote ((nil :maxlevel . 1)
				 (org-agenda-files :maxlevel . 2))))

(setq org-use-fast-todo-selection t)
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)
(setq org-agenda-block-separator nil)


(setq my-org-agenda-personal-file (concat org-directory "personal.org"))
(setq my-org-agenda-work-file (concat org-directory "work.org"))
(setq my-org-agenda-family-file (concat org-directory "family.org"))
(setq my-org-agenda-project-file (concat org-directory "project.org"))
(setq org-agenda-files (list my-org-agenda-personal-file my-org-agenda-work-file my-org-agenda-family-file my-org-agenda-project-file))
(setq org-archive-location (concat org-directory "archive/archive.org::* From %s"))


;; journal
(require 'org-journal)
(customize-set-variable 'org-journal-dir (concat org-directory "journal/"))
(setq org-journal-file-type 'weekly)
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

;; capture
(setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
			      ("i" "Inbox entry" entry (file "inbox.org")
                               "* TODO %^{Title}\n%?" :immediate-finish t)
			      ("m" "Money entry" table-line (file+headline "finance.org" "Spend")
                               "|%^t|%(cc-spend-item-promot)|%^{cost}|%^{log}|%?")
			      ("s" "Body entry" table-line (file+headline "body.org" "Weight")
                               "|%^t|%^{weight}|%^{waist}|%^{log}|%?")))



(setq org-agenda-custom-commands
      '(("d" "Dashbord" agenda
         (org-super-agenda-mode)
         ((org-agenda-span 'day)
	  (org-super-agenda-groups
           '(
             (:name "Important"
		    :face (:underline t)
                    :priority "A")
             (:name "Today Calendar"
                    :time-grid t)
	     (:name "Today Schedule"
		    :scheduled today)
	     (:name "Over Schedule"
		    :scheduled past)
             (:name "Over Due"
		    :deadline past)
	     (:name "Due Future"
		    :deadline future)
             )))
         (org-agenda nil "a"))
	("p" "Project" alltodo
	 (org-super-agenda-mode)
	 ((org-super-agenda-groups
	   '(
	     (:name "Inprogress Project"
		    :children "NEXT")
	     (:name "All Project"
		    :children t)
	     (:discard (:anything t))
	     ))))
	))


(defun my-org-insert-block-line (name)
  (interactive "sName:")
  (insert (format "* =---------------------------------------------------%s-------------------------------------------=" name)))


;; drill
;;(require 'org-drill)
;;(setq org-drill-maximum-items-per-session 60)
;;(setq org-drill-maximum-duration 30)

;; pomodoro
(require 'org-pomodoro)

(defun my-org-pomodoro-notify (orig-fun &rest args)
  "Send a toast notification on windows 10"
  (apply orig-fun args)
  (call-process-shell-command (apply #'format "powershell -ExecutionPolicy Bypass -File %s %s %s" my-windows-toast-file args) nil "*scratch*")
  )
(advice-add 'org-pomodoro-notify :around #'my-org-pomodoro-notify)


(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)


;; (defun cc-org-mp3-app (file-path link)
;;   (w32-shell-execute
;;    "open"
;;    "C:/Program Files (x86)/VideoLAN/VLC/vlc.exe"
;;    (concat "file:///" file-path)))
;; (require 'org)
;; (add-to-list 'org-file-apps '("\\.mp3\\'" . cc-org-mp3-app))
;; (add-to-list 'org-file-apps '("\\.wav\\'" . cc-org-mp3-app))

(provide 'setup-org)




