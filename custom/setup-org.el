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
(setq org-journal-enable-agenda-integration t)
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
                               ("n" "Note entry" entry (function org-journal-find-location)
                               "** NOTE %(format-time-string org-journal-time-format)%^{Title}\n    %i%?")
                              ("e" "Event entry" entry (function org-journal-find-location)
                               "** EVENT %(format-time-string org-journal-time-format)%^{Title}\n    %i%?")
                              ("t" "Task entry" entry (function org-journal-find-location)
                               "** TODO %(format-time-string org-journal-time-format)%^{Title}\n    %i%?")
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
  )
(advice-add 'org-pomodoro-notify :around #'my-org-pomodoro-notify)

(defun my-org-notify_handler (msg)
  (let ((m (list "Effort" msg)))
    (call-process-shell-command (format "powershell -ExecutionPolicy Bypass -File %s %s %s" my-windows-toast-file "Effort" msg) nil "*scratch*")))
(setq org-show-notification-handler 'my-org-notify_handler)

(defun my-clock-in-hook ()
  (let* ((org-journal-find-file 'find-file)
         (id (org-entry-get (point) "ID"))
         (content (nth 4 (org-heading-components)))
         (prev-buffer (current-buffer)))
    (save-excursion
      (save-restriction
        (org-journal-new-entry t)
        (org-insert-heading '(16))
        (insert "TASK ")
        (insert (format-time-string "%H:%M " org-clock-start-time))
        (insert (format "[[id:%s][%s]]" id content))
        (org-clock-find-position nil)
        (insert-before-markers "\n")
	    (backward-char 1)
	    (org-indent-line)
	    (insert org-clock-string " ")
        (org-insert-time-stamp org-clock-start-time
					   'with-hm 'inactive)
        ))
    (switch-to-buffer prev-buffer)
    (org-save-all-org-buffers)))

(add-hook 'org-clock-in-hook 'my-clock-in-hook)

(defun my-clock-out-hook ()
  (let* ((org-journal-find-file 'find-file)
         (id (org-entry-get (point) "ID"))
         (content (nth 4 (org-heading-components)))
         (time (format-time-string "%H:%M " org-clock-start-time))
         (title (concat "** TASK " time (format "\\[\\[id:%s\\]\\[%s\\]\\]" id content)))
         (prev-buffer (current-buffer))
         )
    (save-excursion
      (save-restriction
        (org-journal-new-entry t)
        (goto-char (point-min))
        (org-map-entries (lambda ()
                           (if (looking-at title)
                               (let (ts
                                     te
                                     s
                                     h
                                     m)
                                 (org-clock-find-position t)
                                 (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
                                          (equal (match-string 1) org-clock-string))
                                     (setq ts (match-string 2))
                                   (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
                                 (goto-char (match-end 0))
                                 (delete-region (point) (point-at-eol))
                                 (insert "--")
                                 (setq te (org-insert-time-stamp org-clock-out-time 'with-hm 'inactive))
                                 (setq s (org-time-convert-to-integer
                                          (time-subtract
                                           (org-time-string-to-time te)
                                           (org-time-string-to-time ts)))
                                       h (floor s 3600)
                                       m (floor (mod s 3600) 60))
                                 (insert " => " (format "%2d:%02d" h m))
                                 ))
                           ) "TODO=\"TASK\"")
        ))
    (switch-to-buffer prev-buffer)
    (org-save-all-org-buffers)
    ))

(add-hook 'org-clock-out-hook 'my-clock-out-hook)

(defun my-org-todo-trigger-hook (list)
  (if (equal "DONE" (plist-get list :to))
      (let* ((org-journal-find-file 'find-file)
             (id (org-entry-get (point) "ID"))
             (content (nth 4 (org-heading-components)))
             (time (format-time-string "%H:%M "))
             )
        (save-excursion
          (save-restriction
            (org-journal-new-entry t)
            (org-insert-heading '(16))
            (insert "DONE ")
            (insert time)
            (insert (format "[[id:%s][%s]]" id content))
            ))
        (org-save-all-org-buffers))))

(add-hook 'org-trigger-hook 'my-org-todo-trigger-hook)



(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)


(provide 'setup-org)




