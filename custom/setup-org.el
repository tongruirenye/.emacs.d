
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WORKING(w)"  "|" "DONE(d)")
	(sequence  "PROJECT(p)" "|" "COMPLETE(c)")))


(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("PROJECT" . "snow")
	("WORKING" . "forest green")
	("NEXT" . "orange")
	("COMPLETE" . "gray")
	("DONE" . "gray")))

	       
(setq org-refile-targets (quote ((nil :maxlevel . 1)
				 (org-agenda-files :maxlevel . 2))))

(setq org-use-fast-todo-selection t)
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

(setq org-directory "d:/mydata/org/")
(setq my-org-agenda-personal-file (concat org-directory "personal.org"))
(setq my-org-agenda-work-file (concat org-directory "work.org"))
(setq my-org-agenda-family-file (concat org-directory "family.org"))
(setq org-agenda-files (list my-org-agenda-personal-file my-org-agenda-work-file my-org-agenda-family-file))
(setq org-archive-location (concat org-directory "archive/archive.org::* From %s"))


;; capture
(setq org-capture-templates '(("j" "Journal entry" entry (file+olp+datetree "journal.org")
                               "* %^{Title}\n%i%?" :tree-type week)
			      ("i" "Inbox entry" entry (file "inbox.org")
                               "* TODO %^{Title}\n%?" :immediate-finish t)
			      ("m" "Money entry" table-line (file+headline "finance.org" "Spend")
                               "|%^t|%(cc-spend-item-promot)|%^{cost}|%^{log}|%?")
			      ("s" "Body entry" table-line (file+headline "body.org" "Weight")
                               "|%^t|%^{weight}|%^{waist}|%^{log}|%?")
			      ("w" "Word entry" entry (file "d:/mydata/org/english/word/card.org")
			       "* Word :drill:
  :PROPERTIES:
  :DRILL_CARD_TYPE: twosided
  :END:
  Added:%U
  %^{context}
** English
   %(cc-english-word-prompt)
** Chinese
   %(cc-english-get-definition cc-english-word)
** Pronucation
   [[%(cc-english-get-pronucation cc-english-word)]]")
			      ("f" "Sentence entry" entry (file "d:/mydata/org/english/word/sentence.org")
			       "* Sentence :drill:
  :PROPERTIES:
  :DRILL_CARD_TYPE: dictation
  :END:
  [[%(cc-english-get-sentence)]]
** English
   %^{English}
** Chinese
   %^{Chinese}")
			      ("b" "Reading entry" entry (function cc-english-choose-card)
			       "* %^{Title} :drill:
  :PROPERTIES:
  :DATE_ADDED: %U
  :PAGE_NUM: %^{Page}
  :END:
  %^{Content}")
			      ("k" "Lesson entry" entry (file "d:/mydata/org/english/word/lesson.org")
			       "* Lesson :drill:
  :PROPERTIES:
  :DRILL_CARD_TYPE: twosided
  :END:
  Added:%U
  %^{context}
** English
   %(cc-english-word-prompt)
** Chinese
   %(cc-english-get-definition cc-english-word)
** Pronucation
   [[%(cc-english-get-pronucation cc-english-word)]]")))



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
(require 'org-drill)
(setq org-drill-maximum-items-per-session 60)
(setq org-drill-maximum-duration 30)

;; pomodoro
(require 'org-pomodoro)

(setq my-windows-toast-file "C:/Users/chenchao/AppData/Roaming/.emacs.d/custom/notify.ps1")

(defun my-org-pomodoro-notify (orig-fun &rest args)
  "Send a toast notification on windows 10"
  (apply orig-fun args)
  (call-process-shell-command (apply #'format "powershell -ExecutionPolicy Bypass -File %s %s %s" my-windows-toast-file args) nil "*scratch*")
  )
(advice-add 'org-pomodoro-notify :around #'my-org-pomodoro-notify)


(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)


(provide 'setup-org)




