;;
;; Org
;;

(require 'setup-config)
(require 'org-clock)


;; Org
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda))
  :init
  (setq org-directory cc-org-dir)
  :config
  (add-hook 'org-agenda-mode-hook 'org-agenda-follow-mode)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(a)" "SCHED(s)" "LONG(l)" "WAITING(w@)" "|" "DONE(d!)" "CANCEL(c@)")
                            (type "QUESTION" "DISCUSSION" "THINKING" "DECISION" "ISSUE" "DOCUMENT" "|" "DROP")
                            (type "POMO" "DIET" "JOURNAL" "|" "DATA")
          (type "GOAL" "PROJ" "SUBJECT" "INBOX" "|" "COMPLETE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROJ" . "blue")
	      ("DOING" . "spring green")
          ("DOCUMENT" . "tomato")
          ("SUBJECT" . "chocolate")
	      ("DONE" . "gray")))
  (setq org-agenda-sorting-strategy
  '((agenda habit-down time-up todo-state-up priority-down category-keep)
    (todo todo-state-up priority-down category-keep)
    (tags   priority-down category-keep)
    (search category-keep)))
  (setq org-use-fast-todo-selection t
        org-clock-into-drawer t
        org-hide-leading-stars t
        org-log-into-drawer t
        org-agenda-block-separator nil
        org-log-done 'time
        org-deadline-warning-days 1
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-archive-location (concat org-directory "archive/archive.org::* From %s")
        org-agenda-files (directory-files (concat cc-org-dir "roam/") t "20211215103549-项目计划.org")
        org-attach-dir-relative t
        org-default-notes-file (concat org-directory "phone/capture.org")
        org-refile-targets '((org-agenda-files . (:level . 1))
                             (org-agenda-files . (:todo . "SUBJECT")))
        org-clock-in-switch-to-state (lambda (s)
                                       (if (or (equal s "TODO") (equal s "SCHED"))
                                           "DOING"
                                         nil))
        ))

(defun cc-org-switch-to-state(&rest ignore)
  (let ((style (org-entry-get (point) "STYLE"))
        (sche (org-entry-get (point) "SCHEDULED"))
        (repeat (org-get-repeat)))
    (if (and sche (and (not style) (not repeat)))
        (org-todo "SCHED"))))

(advice-add 'org-schedule :after #'cc-org-switch-to-state)

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
  ;; (when *is-win*
  ;;   (setq org-roam-database-connector 'sqlite3))
  (setq org-roam-directory (concat cc-org-dir "roam")
        org-roam-v2-ack t
        org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M> %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory)))


(defun cc-remove-drawer-at (pos)
  "Remove an drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (memq (org-element-type drawer) '(drawer property-drawer))
		  (org-element-property :contents-begin drawer))
	 (delete-region (org-element-property :begin drawer)
			(progn (goto-char (org-element-property :end drawer))
			       (skip-chars-backward " \r\t\n")
			       (forward-line)
			       (point))))))))

(defun cc-clock-remove-clock-drawer ()
  "Remove clock drawers in current subtree."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-map-tree
     (lambda ()
       (let ((drawer (org-clock-drawer-name))
	         (case-fold-search t))
	     (when drawer
	       (let ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer)))
		         (end (save-excursion (outline-next-heading))))
	         (while (re-search-forward re end t)
	           (cc-remove-drawer-at (point))))))))))

(defun cc-clock-remove-all-clock-drawer ()
  "Remove clock drawers in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries
     (lambda ()
       (let ((drawer (org-clock-drawer-name))
             (style (org-entry-get (point) "STYLE"))
	         (case-fold-search t))
	     (when drawer
	       (let ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer)))
		         (end (save-excursion (outline-next-heading))))
	         (while (re-search-forward re end t)
	           (cc-remove-drawer-at (point)))))))))
  (org-save-all-org-buffers))


(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))

(defun cc-clock-in-hook ()
  (let ((pomo (or (org-entry-get-with-inheritance "POMO") "Focus...")))
    (when *is-win*
      (call-process-shell-command (apply #'format "MiniPomodoro.exe \"%s\"" (list pomo)) nil 0))
    (org-save-all-org-buffers)))


(require 'org-roam-dailies)
(defun cc-org-roam-dailies-capture-today ()
  (let ((org-roam-dailies-capture-templates
         '(("d" "default" entry
         "* %c%?"
         :immediate-finish t
         :target (file+head "daily/%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))
  (org-roam-capture- :goto (when nil '(4))
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time (current-time)))))


(defun cc-clock-out-hook()
  (let ((category (org-entry-get-with-inheritance "CATEGORY"))
        (project (org-entry-get-with-inheritance "PROJECT"))
        (title (nth 4 (org-heading-components)))
        ts
        te
        s
        h
        m)
    (with-temp-buffer
      (org-mode)
      (insert "* POMO ")
      (insert (format-time-string "%H:%M " org-clock-start-time))
      (insert title)
      (org-entry-put (point) "KIND" category)
      (org-entry-put (point) "PROJECT" project)
      (org-clock-find-position nil)
      (insert-before-markers "\n")
      (backward-char 1)
      (org-indent-line)
      (insert org-clock-string " ")
      (setq ts (org-insert-time-stamp org-clock-start-time
                             'with-hm 'inactive))
      (insert "--")
      (setq te (org-insert-time-stamp org-clock-out-time 'with-hm 'inactive))
      (setq s (org-time-convert-to-integer
               (time-subtract
                (org-time-string-to-time te)
                (org-time-string-to-time ts)))
            h (floor s 3600)
            m (floor (mod s 3600) 60))
      (insert " => " (format "%2d:%02d" h m))
      (goto-char (point-min))
      (delete-char 2)
      (kill-ring-save (point-min) (point-max))
      (cc-org-roam-dailies-capture-today))
    (org-save-all-org-buffers)))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-out-hook))
  )
(add-hook 'org-clock-out-hook 'cc-clock-out-hook)

(defun cc-org-todo-trigger-hook (list)
  (when (equal "DONE" (plist-get list :to))
    (let ((category (org-entry-get-with-inheritance "CATEGORY"))
          (project (org-entry-get-with-inheritance "PROJECT"))
          (level (nth 0 (org-heading-components)))
          (s (org-copy-special))
          (time (format-time-string "%H:%M ")))
      (with-temp-buffer
      (org-mode)
      (yank)
      (goto-char (point-min))
      (cc-clock-remove-clock-drawer)
      (org-entry-put (point) "KIND" category)
      (org-entry-put (point) "PROJECT" project)
      (while (> level 1)
        (org-promote)
        (setq level (1- level)))
      (goto-char (point-min))
      (delete-char 2)
      (kill-ring-save (point-min) (point-max))
      (cc-org-roam-dailies-capture-today)))
    (org-reset-checkbox-state-maybe)
    (org-save-all-org-buffers)))

(add-hook 'org-trigger-hook #'cc-org-todo-trigger-hook)


(defun cc-store-log ()
  (let ((txt (buffer-string))
        (i 0)
        lines)
    (with-temp-buffer
      (org-mode)
      (setq lines (org-split-string txt "\n"))
      (print lines)
      (insert (format-time-string "%H:%M "))
      (dolist (line lines)
        (if (= i 3)
            (progn
              (insert line)
              (insert "\n")))
        (when (> i 3)
          (insert "  ")
          (insert line)
          (insert "\n"))
        (setq i (1+ i)))
      (goto-char (point-min))
      (kill-ring-save (point-min) (point-max))
      (cc-org-roam-dailies-capture-today)
      )))

(advice-add 'org-store-log-note :before #'cc-store-log)

;; yankpad
(use-package yankpad
  :ensure t
  :defer 10
  :config
  (bind-key "<f11>" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  (setq yankpad-default-category "company"))


;; rss
(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat cc-org-dir "roam/20220228220815-elfeed.org"))))

(global-set-key (kbd "C-x w") 'elfeed)


(use-package dirvish
  :ensure t)

(provide 'setup-org)

