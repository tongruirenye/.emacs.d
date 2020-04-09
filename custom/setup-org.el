(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(a)" "|" "DONE(d)")
        (type "TASK" "NOTE" "EVENT" "|" "INBOX")
        ))


(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("INPROGRESS" . "spring green")
	("DONE" . "gray")))


(server-start)
(require 'org-protocol)


	       
;; (setq org-refile-targets (quote ((nil :todo . "CATEGORY")
;;                                  (nil :todo . "PROJECT")
;;                                  (org-agenda-files :todo . "CATEGORY")
;;                                  (org-agenda-files :todo . "PROJECT"))))

(setq org-use-fast-todo-selection t)
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)
(setq org-agenda-block-separator nil)
(setq org-log-done 'time)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 3)


(setq org-archive-location (concat org-directory "archive/archive.org::* From %s"))
(setq org-agenda-files (directory-files (concat org-directory "project") t "\\w+.org"))

;; journal
(require 'org-journal)
(customize-set-variable 'org-journal-dir (concat org-directory "journal/"))
(setq org-journal-file-type 'daily)
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min))
)

;; capture
(setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                              ("c" "web capture" entry (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%:description\n   [[%:link][%:description]]\n   %:initial\n   " :immediate-finish t)
                              ))


;; agenda
(setq org-agenda-custom-commands
      '(("d" "Today" agenda
         (org-super-agenda-mode)
         ((org-agenda-span 'day)
	  (org-super-agenda-groups
           '(
             (:name "Today Calendar"
                    :time-grid t
                    :todo "INPROGRESS")
             (:name "Today Deadline"
                :face (:underline t)
                :deadline today)
             (:name "Today Scheduled"
                    :scheduled today)
             (:habit t)
             (:discard (:anything t))
             )))
         (org-agenda nil "a"))
	("p" "Dashbord" alltodo
	 (org-super-agenda-mode)
	 ((org-super-agenda-groups
	   '((:name "InProgress Tasks"
                :todo "INPROGRESS")
         (:name "Todo Tasks"
                :todo "TODO")
         (:name "Inbox"
		    :face (:underline t)
            :todo "INBOX")
	     (:discard (:anything t))
	     ))))
	))

;; drill
(require 'org-drill)
(setq org-drill-maximum-items-per-session 60)
(setq org-drill-maximum-duration 30)

;; pomodoro
(require 'org-pomodoro)

(defun my-org-pomodoro-notify (orig-fun &rest args)
  "Send a toast notification on windows 10"
  (apply orig-fun args)
  (call-process-shell-command (apply #'format "powershell -ExecutionPolicy Bypass -File %s %s %s" my-windows-toast-file args) nil "*scratch*")
  (org-save-all-org-buffers)
  )
(advice-add 'org-pomodoro-notify :around #'my-org-pomodoro-notify)


(defun my-org-notify_handler (msg)
  (let ((m (list "Effort" msg)))
    (call-process-shell-command (format "powershell -ExecutionPolicy Bypass -File %s %s %s" my-windows-toast-file "Effort" msg) nil "*scratch*")))
(setq org-show-notification-handler 'my-org-notify_handler)

(defun my-start-pomodoro ()
  (interactive)
  (let* ((org-journal-find-file 'find-file)
        (cc-todo-list '())
        (cc-todo-pos '())
        (prev-buffer (current-buffer)))
    (save-excursion
      (save-restriction
        (org-journal-new-entry t)
        (goto-char (point-min))
        (org-map-entries (lambda ()
                           (push (nth 4 (org-heading-components)) cc-todo-list)
                           (push (cons (nth 4 (org-heading-components)) (point))
                                 cc-todo-pos)) "TODO=\"INPROGRESS\"")
        (ivy-read "Peek a Item:" cc-todo-list
                  :action (lambda (x)
                            (dolist (v cc-todo-pos)
                              (if (string-equal x (car v))
                                  (goto-char (cdr v))))
                            (org-pomodoro)))))
    (switch-to-buffer prev-buffer)))


(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)


(provide 'setup-org)




