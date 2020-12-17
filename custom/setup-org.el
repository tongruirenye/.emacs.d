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
  ;;(add-hook 'org-agenda-mode-hook 'org-agenda-follow-mode)
  (setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAIT(w)" "|" "DONE(d)")
          (type "POMO" "JOURNAL" "PROJ" "EVENT" "NOTE" "DATA" "INBOX" "|" "COMPLETE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROJ" . "red")
	      ("ACTIVE" . "spring green")
          ("POMO" . "tomato")
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




;; Org Roam
(use-package org-roam
      :ensure t
      ;;:hook
      ;;(after-init . org-roam-mode)
      :custom
      (org-roam-directory cc-roam-dir)
      :config
      (setq org-roam-completion-system 'ivy)
      (setq org-roam-dailies-directory "daily/")
      
      (defun cc-parse-read-option ()
        (let ((rlist '())
              name
              choose)
          (with-current-buffer (find-file-noselect cc-read-file)
            (org-map-entries (lambda ()
                               (setq name (org-entry-get (point) "NAME"))
                               (when name
                                 (push name rlist)))
                             "TODO=\"PROJ\""))
          (ivy-read "Choose a Book" rlist
                    :action (lambda (b)
                              (setq choose b)))
          choose))
      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry
               #'org-roam-capture--get-point
               "* %(format-time-string \"%H:%M\" (current-time)) %?"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")
              ("t" "todo" entry
               #'org-roam-capture--get-point
               "* TODO %?\n  %u"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")
              ("o" "opinion" entry
               #'org-roam-capture--get-point
               "* OPINION %?\n  %u"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")
              ("i" "IDEA" entry
               #'org-roam-capture--get-point
               "* IDEA %?\n  %u"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")
              ("r" "book clock in" entry
               #'org-roam-capture--get-point
               "* %(format-time-string \"%H:%M\" (current-time)) %(cc-parse-read-option) %?"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")))
      
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n d" . org-roam-dailies-capture-today)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; Org Roam Server
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        ;;org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        ;;org-roam-server-network-poll t
        ;;org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; protocol
(use-package org-protocol
  :ensure nil
  :config
  (server-start))
(require 'org-roam-protocol)


(defun cc-open-roam ()
  (interactive)
  (org-roam-mode)
  (org-roam-server-mode))

;; roam auto daily capture
(defun cc-org-auto-roam-dailies-capture ()
  (unless org-roam-mode (org-roam-mode))
  (let ((org-roam-capture-templates (--> '(("d" "default" entry
               #'org-roam-capture--get-point
               "* %c%?"
               :immediate-finish t
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n"))))
        (org-roam-capture--info (list (cons 'time (current-time))))
        (org-roam-capture--context 'dailies))
    ;;(setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (org-roam-capture--capture (when nil '(4)))))

(defun cc-clock-in-hook ()
  (let ((pomo (or (org-entry-get-with-inheritance "POMO") "GAME")))
    (call-process-shell-command (apply #'format "MiniPomodoro.exe \"%s\"" (list pomo)) nil 0)
    (org-save-all-org-buffers)))

(defun cc-clock-out-hook()
  (let ((category (org-entry-get-with-inheritance "CATEGORY"))
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
      (cc-org-auto-roam-dailies-capture))
    (org-save-all-org-buffers)))

(defun cc-org-todo-trigger-hook (list)
  (when (equal "DONE" (plist-get list :to))
    (let ((ca (org-entry-get-with-inheritance "CATEGORY"))
          (content (nth 4 (org-heading-components)))
          (time (format-time-string "%H:%M "))
          )
      (with-temp-buffer
        (org-mode)
        (insert "* COMPLETE " )
        (insert time)
        (insert content)
        (org-entry-put (point) "KIND" ca)
        (goto-char (point-min))
        (delete-char 2)
        (kill-ring-save (point-min) (point-max))
        (cc-org-auto-roam-dailies-capture))
      (org-save-all-org-buffers))))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-out-hook)
         (org-trigger . cc-org-todo-trigger-hook))
  )

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
         (:name "Active"
                :todo "ACTIVE")
             (:name "Highlight"
		    :face (:underline t)
                    :tag "HIGHLIGHT")
             (:name "Calendar"
                    :time-grid t)
	     (:name "Schedule"
		        :scheduled today
                :deadline today)
	     (:name "Over"
		        :scheduled past
                :deadline past)
             )))
         (org-agenda nil "a"))
	("p" "Dashbord" alltodo
	 (org-super-agenda-mode)
	 ((org-super-agenda-groups
	   '(
         (:name "All Project"
		        :todo "PROJ")
	     (:name "Inprogress"
		        :todo "ACTIVE"
                :scheduled past)
         (:name "Today"
                :and (:todo "TODO" :scheduled today)
                :and (:todo "TODO" :deadline today)
                )
         (:name "Deadline"
                :deadline past)
         (:name "Future"
                :scheduled future
                :deadline future)
         (:name "Todo-read"
                :and (:category "阅读" :todo "TODO"))
         (:name "Todo-work"
                :and (:category "工作" :todo "TODO"))
         (:name "Inbox"
                :category "收集箱")
	     (:discard (:anything t))
	     ))))
	)))


(defun cc-remove-drawer-at (pos)
  "Remove an  drawer at position POS.
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

(add-hook 'org-clock-out-hook 'cc-clock-remove-clock-drawer)


(use-package org-edna
  :ensure t
  :hook (after-init . org-edna-mode))


(provide 'setup-org)

