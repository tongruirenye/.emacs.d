;;
;; Org
;;

(require 'setup-config)

(defun cc-org-todo-trigger-hook (list)
  (if (equal "DONE" (plist-get list :to))
      (let* ((org-journal-file-name (org-journal-get-entry-path))
             (id (org-entry-get (point) "ID"))
             (content (nth 4 (org-heading-components)))
             (time (format-time-string "%H:%M "))
             (org-journal-buffer (find-file-noselect org-journal-file-name))
             )
        (save-excursion
          (save-restriction
            (with-current-buffer org-journal-buffer
              (org-journal-new-entry t)
              (org-insert-heading '(16))
              (insert "DONE ")
              (insert time)
              (insert (format "[[id:%s][%s]]" id content))
              )))
        (org-save-all-org-buffers))))

;; Org
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory cc-org-dir)
  :hook (org-trigger . cc-org-todo-trigger-hook)
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(a)" "WAITING" "|" "DONE(d)")
          (type "TASK" "NOTE" "EVENT" "CAPTURE" "|" "INBOX")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
	      ("INPROGRESS" . "spring green")
	      ("DONE" . "gray")))
 (setq org-use-fast-todo-selection t
  org-clock-into-drawer t
  org-log-into-drawer t
  org-agenda-block-separator nil
  org-log-done 'time
  org-agenda-skip-deadline-prewarning-if-scheduled 3
  org-archive-location (concat org-directory "archive/archive.org::* From %s")
  org-agenda-files (directory-files cc-project-dir t "\\w+.org")
  ))



;; Org Journal
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir cc-journal-dir)
  (org-journal-file-type 'daily)
  :config
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-min))
    )
  (setq org-journal-enable-agenda-integration t)
  (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "** %^{Title}\n   %i%?")
                               ("n" "Note entry" entry (function org-journal-find-location)
                               "** NOTE %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                              ("e" "Event entry" entry (function org-journal-find-location)
                               "** EVENT %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                              ("t" "Task entry" entry (function org-journal-find-location)
                               "** TODO ^{Title}\n   %i%?")
                              ("c" "web capture" entry (function org-journal-find-location)
                               "** CAPTURE %(format-time-string org-journal-time-format)%:description\n   [[%:link][%:description]]\n   %:initial\n   " :immediate-finish t)
                              ))
  )


;; protocol
(use-package org-protocol
  :ensure t
  :config
  (server-start))


;; agenda
(use-package org-super-agenda
  :ensure t
  :config
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
  )

;; drill
(use-package org-drill
  :ensure t
  :config
  (setq org-drill-maximum-items-per-session 60)
  (setq org-drill-maximum-duration 30)
  )


;; pomodoro
(use-package org-pomodoro
  :ensure t
  :config
  (defun cc-org-pomodoro-notify (orig-fun &rest args)
    "Send a toast notification on windows 10"
    (apply orig-fun args)
    (call-process-shell-command (apply #'format "powershell -ExecutionPolicy Bypass -File %s %s %s" cc-windows-toast-file args) nil "*scratch*")
    )
  (advice-add 'org-pomodoro-notify :around #'cc-org-pomodoro-notify)
  )

(defun cc-clock-in-hook ()
    (let* ((org-journal-file-name (org-journal-get-entry-path))
           (id (org-entry-get (point) "ID"))
           (content (nth 4 (org-heading-components)))
           (org-journal-buffer (find-file-noselect org-journal-file-name)))
      (save-excursion
        (save-restriction
          (with-current-buffer org-journal-buffer
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
            )
          ))
      (org-save-all-org-buffers)))

(defun cc-clock-out-hook ()
  (let* ((org-journal-file-name (org-journal-get-entry-path))
         (id (org-entry-get (point) "ID"))
         (content (nth 4 (org-heading-components)))
         (time (format-time-string "%H:%M " org-clock-start-time))
         (title (concat "** TASK " time (format "\\[\\[id:%s\\]\\[%s\\]\\]" id content)))
         (org-journal-buffer (find-file-noselect org-journal-file-name))
         )
    (save-excursion
      (save-restriction
        (with-current-buffer org-journal-buffer
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
          )
        ))
    (org-save-all-org-buffers)
    ))

;; clock
(use-package org-clock
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-out-hook))
  )


;; brain
(use-package org-brain
  :ensure t
  :bind (("C-c n" . org-brain-visualize))
  :init
  (setq org-brain-path cc-note-dir)
  :config
  (setq org-id-track-globally t)
  ;;(add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(provide 'setup-org)




