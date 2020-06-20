;;
;; Org
;;

(require 'setup-config)
(require 'org-id)
(require 'org-clock)

(defun cc-org-todo-trigger-hook (list)
  (when (equal "DONE" (plist-get list :to))
      (let ((journal-file (org-journal-get-entry-path))
             (id (org-entry-get (point) "TID"))
             (content (nth 4 (org-heading-components)))
             (time (format-time-string "%H:%M "))
             )
        (save-excursion
          (with-current-buffer (find-file-noselect journal-file)
            (org-journal-new-entry t)
            (org-insert-heading '(16))
            (insert "COMPLETE "))
            (insert time)
            (insert content)
            (org-entry-put (point) "TID" id)
            ))
        (org-save-all-org-buffers)))


(defun cc-org-after-refile-insert-hook (category)
  (org-entry-put (point) "CATEGORY" category)
  )

;;;###autoload
(defun cc-org-refile-to-journal ()
  (interactive)
  (let ((journal-file (org-journal-get-entry-path))
        find
        id
        category
        rfloc)
    (when (equal journal-file (buffer-file-name))
      (error "you are in journal file"))
    (save-excursion
      (while (not (or find (not (org-up-heading-safe))))
        (when (setq id (org-entry-get (point) "ID"))
          (setq category (org-entry-get-with-inheritance "CATEGORY"))
          (setq find t))))
    (unless find
      (error "no project id"))
    (unless category
      (error "no project category"))
    (unless (org-entry-get (point) "TID")
      (org-entry-put (point) "TID" (format "[[id:%s]]" id)))
    (org-schedule nil "+0d")
    
    (with-current-buffer (find-file-noselect journal-file)
      (org-journal-new-entry t)
      (setq rfloc (list "Journal" journal-file nil 1)))
    (let ((org-after-refile-insert-hook
           (apply-partially
            #'cc-org-after-refile-insert-hook category))
          )
      (org-refile nil nil rfloc "RefileToJournal")
    )))

;;;###autoload
(defun cc-org-refile-to-agenda (&optional ARG)
  (interactive "P")
  (when (equal cc-bullet-agenda-file (buffer-file-name))
      (error "you are in agenda file"))
  (if ARG
      (cc-org-refile-to-agenda-entries)
    (cc-org-refile-to-agenda-entry))
  )

(defun cc-org-refile-update-pomo ()
  (let ((pomo (or (org-entry-get (point) "POMO") "0"))
        (time (org-clock-sum-current-item)))
    (if (> time 0)
        (+ (string-to-number pomo) time)
      nil)))

(defun cc-org-refile-to-agenda-entry ()
  (let ((tid (org-entry-get (point) "TID"))
        (todo (nth 2 (org-heading-components)))
        id
        idpos
        pomo
        rfloc)
    (unless tid
      (error "no project id"))
    (string-match "\\[\\[id:\\(.*\\)\\]\\]" tid)
    (setq id (match-string 1 tid))
    (unless id
      (setq id "no project id"))

    (setq idpos (org-id-find-id-in-file id cc-bullet-agenda-file))
    (unless (cdr idpos)
      (error "no project postion"))
    (setq rfloc (list id cc-bullet-agenda-file nil (cdr idpos)))

    (setq pomo (cc-org-refile-update-pomo))
    (when pomo
      (org-entry-put (point) "POMO" (number-to-string pomo)))
    (org-entry-delete (point) "CATEGORY")
    
    (cond
     ((equal todo "DONE")
      (when (equal "habit" (org-entry-get (point) "KIND"))
        (org-todo "TODO")
        (org-schedule nil (format "+%sd" (or (org-entry-get (point) "DAY") 1))))
      )
     ((or (equal todo "TODO") (equal todo "INPROGRESS"))
      (when (equal "habit" (org-entry-get (point) "KIND"))
        (org-schedule nil "+1d")))
     (t
      (org-todo "TODO"))
     )

    (org-refile nil nil rfloc "RefileToAgenda")))

(defun cc-org-refile-to-agenda-entries ()
  (org-map-entries 'cc-org-refile-to-agenda-entry
                   "TODO=\"TODO\"|TODO=\"DONE\"|TODO=\"INPROGRESS\"|TODO=\"WAITING\""))

(defun cc-org-refile-carryover ()
  (save-excursion
    (save-restriction
      (when (let ((inhibit-message t))
              (org-journal-open-previous-entry 'no-select))
        (cc-org-refile-to-agenda-entries))))
  )

(advice-add 'org-journal-carryover :override #'cc-org-refile-carryover)


;; Org
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c n" . cc-org-refile-to-journal)
         ("C-c m" . cc-org-refile-to-agenda))
  :init
  (setq org-directory cc-org-dir)
  :hook ((org-trigger . cc-org-todo-trigger-hook))
  :config
  (add-hook 'org-agenda-mode-hook 'org-agenda-follow-mode)
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(a)" "WAITING" "|" "DONE(d)")
          (type "POMODORO" "NOTE" "EVENT" "DATA" "|" "COMPLETE")))
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
  org-agenda-files (list cc-bullet-agenda-file)
  org-refile-targets '((nil . (:level . 2)) (nil . (:level 3)))
  org-clock-in-switch-to-state "INPROGRESS"
  ))


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
                               "** %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                               ("n" "Note entry" entry (function org-journal-find-location)
                               "** NOTE %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                              ("e" "Event entry" entry (function org-journal-find-location)
                               "** EVENT %(format-time-string org-journal-time-format)%^{Title}\n   %i%?")
                              ("t" "Task entry" entry (file+headline cc-bullet-agenda-file "Task")
                               "*** TODO %^{Title}\n    %i%?")
                              ("c" "Web entry" entry (file+headline cc-bullet-agenda-file "Capture")
                               "*** TODO %:description\n   :PROPERTIES:\n   :SOURCE: [[%:link][%:description]]\n   :END:\n   %?" :immediate-finish t)
                              ))
   )

;; protocol
(use-package org-protocol
  :ensure nil
  :config
  (server-start))


(defun cc-clock-in-hook ()
  (let ((org-journal-file-name (org-journal-get-entry-path))
         (tid (org-entry-get (point) "TID"))
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
    (org-save-all-org-buffers)))

(defun cc-clock-out-hook ()
  (let ((org-journal-file-name (org-journal-get-entry-path))
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

(provide 'setup-org)

