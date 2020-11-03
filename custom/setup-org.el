;;
;; Org
;;

(require 'setup-config)
(require 'org-id)
(require 'org-clock)
(require 'org-journal)

(defun cc-org-todo-trigger-hook (list)
  (when (equal "DONE" (plist-get list :to))
    (let ((journal-file (org-journal--get-entry-path))
          (id (org-entry-get-with-inheritance "ID"))
          (ca (org-entry-get-with-inheritance "CATEGORY"))
          (content (nth 4 (org-heading-components)))
          (time (format-time-string "%H:%M "))
          )
        (save-excursion
          (with-current-buffer (find-file-noselect journal-file)
            (org-journal-new-entry t)
            (org-insert-heading '(16))
            (insert "COMPLETE ")
            (insert time)
            (insert content)
            (org-entry-put (point) "TCATEGORY" ca)
            (org-entry-put (point) "TID" id)
            ))
        (org-save-all-org-buffers)))
  (when (equal "DEFER" (plist-get list :to))
    (let ((journal-file (org-journal--get-entry-path))
          (id (org-entry-get-with-inheritance "ID"))
          (ca (org-entry-get-with-inheritance "CATEGORY"))
          (content (nth 4 (org-heading-components)))
          (time (format-time-string "%H:%M "))
          )
        (save-excursion
          (with-current-buffer (find-file-noselect journal-file)
            (org-journal-new-entry t)
            (org-insert-heading '(16))
            (insert "DEFER ")
            (insert time)
            (insert content)
            (org-entry-put (point) "TCATEGORY" ca)
            (org-entry-put (point) "TID" id)
            ))
        (org-todo "TODO")
        (org-save-all-org-buffers)))
  (when (equal "CLOCKIN" (plist-get list :to))
    (let ((journal-file (org-journal--get-entry-path))
          (id (org-entry-get-with-inheritance "ID"))
          (ca (org-entry-get-with-inheritance "CATEGORY"))
          (content (nth 4 (org-heading-components)))
          (time (format-time-string "%H:%M "))
          )
        (save-excursion
          (with-current-buffer (find-file-noselect journal-file)
            (org-journal-new-entry t)
            (org-insert-heading '(16))
            (insert "CLOCKIN ")
            (insert time)
            (insert content)
            (org-entry-put (point) "TCATEGORY" ca)
            (org-entry-put (point) "TID" id)
            ))
        (org-todo "TODO")
        (org-save-all-org-buffers))))




;; Org
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory cc-org-dir)
  :hook ((org-trigger . cc-org-todo-trigger-hook))
  :config
  ;;(add-hook 'org-agenda-mode-hook 'org-agenda-follow-mode)
  (setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAITING" "|" "DONE(d)")
          (type "POMODORO" "JOURNAL" "PROJ(p)"  "REVIEW" "IDEA" "CLOCKIN"  "NOTE" "DATA" "INBOX" "DEFER"  "|" "COMPLETE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROJ" . "red")
	      ("ACTIVE" . "spring green")
          ("POMODORO" . "tomato")
          ("NOTE" . "chocolate")
	      ("DONE" . "gray")))
 (setq org-use-fast-todo-selection t
  org-clock-into-drawer t
  org-log-into-drawer t
  org-agenda-block-separator nil
  org-log-done 'time
  org-deadline-warning-days 7
  org-agenda-skip-scheduled-if-deadline-is-shown t
  org-archive-location (concat org-directory "archive/archive.org::* From %s")
  org-agenda-files (directory-files cc-project-dir t "\\.org")
  org-refile-targets '((org-agenda-files . (:level . 1)))
  org-clock-in-switch-to-state (lambda (s)
                                 (if (equal s "TODO")
                                     "ACTIVE"
                                   nil))
  ))


(defun cc-note-ref()
  (let (abc)
    (ivy-read "Choose a Ref:" '("你就是极客！软件开发人员生存指南" "A(7)" "B(5)" "C(3)" "D(1)")
              :action (lambda (w)
                        (cond
                         ((equal w "你就是极客！软件开发人员生存指南" )
                          (setq abc ":PROPERTIES:
   :AUTHOR: Michael Lopp
   :FROM: 你就是极客！软件开发人员生存指南
   :END:")
                          ))
                        ))
    abc))

(defun cc-review-tpl()
  "review tpl"
  "备忘：
   - [ ] 学习
   - [ ] 健康
   - [ ] 家庭
   - [ ] 阅读
   - [ ] 项目


   随记：
")

(defun cc-data-money-tpl()
  "money data tpl"
  ":PROPERTIES:
   :KIND: money
   :OUTPUT: 
   :INPUT: 
   :END:")


;; Org Journal
(use-package org-journal
  :ensure t
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
                               "** JOURNAL %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                               ("n" "Note entry" entry (function org-journal-find-location)
                               "** NOTE %(format-time-string org-journal-time-format)%^{Title}\n   %(cc-note-ref)\n %?")
                              ("t" "Task entry" entry (function org-journal-find-location)
                               "** TODO %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                              ("r" "Review entry" entry (function org-journal-find-location)
                               "** REVIEW %(format-time-string org-journal-time-format)%^{Title}\n   %(cc-review-tpl)\n")
                              ("d" "Data entry" entry (function org-journal-find-location)
                               "** DATA %(format-time-string org-journal-time-format)记账\n   %(cc-data-money-tpl)\n")
                              ("c" "Web entry" entry (function org-journal-find-location)
                               "** INBOX %:description\n   :PROPERTIES:\n   :SOURCE: [[%:link][%:description]]\n   :END:\n   %?" :immediate-finish t)
                              ))
   )

;; protocol
(use-package org-protocol
  :ensure nil
  :config
  (server-start))

(defun cc-clock-in-hook ()
  (let ((org-journal-file-name (org-journal--get-entry-path))
         (id (org-entry-get-with-inheritance "ID"))
         (content (nth 4 (org-heading-components))))
    (save-excursion
      (with-current-buffer  (find-file-noselect org-journal-file-name)
        (org-journal-new-entry t)
        (org-insert-heading '(16))
        (insert "POMODORO ")
        (insert (format-time-string "%H:%M " org-clock-start-time))
        (insert content)
        (org-entry-put (point) "TID" id)
        (org-clock-find-position nil)
        (insert-before-markers "\n")
        (backward-char 1)
        (org-indent-line)
        (insert org-clock-string " ")
        (org-insert-time-stamp org-clock-start-time
                               'with-hm 'inactive)
        )
      )
    (call-process-shell-command (apply #'format "MiniPomodoro.exe \"%s\"" (list content)) nil 0)
    (org-save-all-org-buffers)))

(defun cc-clock-out-hook ()
  (let* ((org-journal-file-name (org-journal--get-entry-path))
        (time (format-time-string "%H:%M " org-clock-start-time))
        (title (concat "** POMODORO " time)))
    (save-excursion
      (with-current-buffer (find-file-noselect org-journal-file-name)
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
                           ) "TODO=\"POMODORO\"")))
    (org-save-all-org-buffers)))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-out-hook))
  :config
  (defun cc-org-pomodoro-notify (orig-fun &rest args)
    "Send a toast notification on windows 10"
    (apply orig-fun args)
    (call-process-shell-command (apply #'format "powershell -ExecutionPolicy Bypass -File %s %s %s" cc-windows-toast-file args) nil "*scratch*")
    )
  (advice-add 'org-pomodoro-notify :around #'cc-org-pomodoro-notify)
  )

;; agenda
(use-package org-super-agenda
  :ensure t
  :config
  (setq org-agenda-custom-commands
      '(("d" "Dashbord" agenda
         (org-super-agenda-mode)
         ((org-agenda-span 'day)
	  (org-super-agenda-groups
           '(
             (:name "Highlight"
		    :face (:underline t)
                    :tag "HIGHLIGHT")
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
		    :children "ACTIVE")
	     (:name "All Project"
		    :children t)
	     (:discard (:anything t))
	     ))))
	)))

(provide 'setup-org)

