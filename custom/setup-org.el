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
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)"  "INPROGRESS(a)"  "|" "DONE(d)")
          (type "POMO" "JOURNAL" "PROJ" "EVENT" "NOTE" "DATA" "INBOX" "|" "COMPLETE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROJ" . "red")
	      ("INPROGRESS" . "spring green")
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
                                 (if (or (equal s "TODO") (equal s "NEXT"))
                                     "INPROGRESS"
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
      
      (setq org-roam-capture-templates
            '(("d" "default" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "%<%Y%m%d%H%M%S>-${slug}"
               :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n"
               :unnarrowed t)
              ("p" "project" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "project/${slug}"
               :head "#+title: ${title}\n"
               :unnarrowed t)
              ("b" "blog" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "blog/${slug}"
               :head "#+title: ${title}\n"
               :unnarrowed t)
              ("e" "english" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "english/${slug}"
               :head "#+title: ${title}\n"
               :unnarrowed t)
              ))
      
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
              ("i" "idea" entry
               #'org-roam-capture--get-point
               "* IDEA %?\n  %u"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")
              ("n" "note" entry
               #'org-roam-capture--get-point
               "* NOTE %?\n  %u"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n#+STARTUP: showall\n\n")))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n c" . org-roam-capture)
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
  (let ((pomo (or (org-entry-get-with-inheritance "POMO") "FOCUS...")))
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
         (:name "当前进行中:"
                :todo "INPROGRESS")
             (:name "日期:"
                    :time-grid t)
	     (:name "今日:"
		        :scheduled today
                :deadline today)
	     (:name "过期:"
		        :scheduled past
                :deadline past)
             )))
         (org-agenda nil "a"))
	("p" "Dashbord" alltodo
	 (org-super-agenda-mode)
	 ((org-super-agenda-groups
	   '(
         (:name "项目:"
		        :todo "PROJ")
	     (:name "进行中的任务:"
		        :todo "INPROGRESS"
                :scheduled past)
         (:name "今日任务:"
                :and (:todo "TODO" :scheduled today)
                :and (:todo "TODO" :deadline today)
                )
         (:name "过期任务:"
                :deadline past)
         (:name "将来任务:"
                :scheduled future
                :deadline future)
         (:name "下一个任务:"
                :todo "NEXT")
         (:name "等待任务:"
                :todo "TODO")
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
  :hook
  (after-init . org-edna-mode)
  :config
  (defun org-edna-action/cc-add-book-statistics! (_last-entry name author kind)
    (org-insert-subheading '(16))
    (insert name)
    (org-entry-put (point) "AUTHOR" author)
    (org-entry-put (point) "TIME" (format-time-string "%Y-%m-%d" (current-time)))
    (org-set-tags (list kind))))

(use-package yankpad
  :ensure t
  :defer 10
  :config
  (bind-key "<f11>" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  (setq yankpad-default-category "project"))


(provide 'setup-org)

