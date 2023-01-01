;;
;; Org
;;

(require 'setup-config)


(defun cc-clock-in-hook ()
  (when *is-win*
    (call-process-shell-command (apply #'format "MiniPomodoro.exe \"%s\"" (list "Focus")) nil 0))
  (org-save-all-org-buffers))

(defun cc-clock-hook ()
  (org-save-all-org-buffers))

;; org
(use-package org
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-out-hook)
         )
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :init
  (setq org-directory cc-org-dir)
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "HIGHLIGHT(h)" "WAITING(w)"  "|" "CANCEL(c)" "DONE(d!)")
                            (type "SUBJECT" "TASK" "MOTIVE" "GOAL" "KEYPOINT" "CLIP"  "ISSUE" "|" "INBOX")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("HIGHLIGHT" . "blue")
          ("WAITING" . "purple")
          ("TASK" . "blue")
	      ("KEYPOINT" . "spring green")
          ("MOTIVE" . "DeepPink")
	      ("DONE" . "gray")))
  (setq org-tag-alist '(("@office" . ?o) ("@home" . ?h) ("@metro" . ?l)))
  (setq org-use-fast-todo-selection t
        org-adapt-indentation t
        org-clock-into-drawer t
        org-hide-leading-stars t
        org-log-into-drawer t
        org-log-done 'time
        org-deadline-warning-days 3
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>")
        org-attach-dir-relative t
        org-agenda-files (directory-files (concat cc-org-dir "roam/project") t ".org")
        org-refile-targets '(
                             (org-agenda-files . (:tag . "#inbox"))
                             )
        )
  (setq org-capture-templates '(("c" "ClockNote" plain (clock)
                                 "* %?"
                                 :immediate-finish t)
                                ))
  )

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
 (setq org-roam-directory (concat cc-org-dir "roam")
       org-roam-v2-ack t)
 (setq org-roam-dailies-directory "daily/")
 (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
 (setq org-roam-capture-templates
       '(("d" "default" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ("b" "blog" plain "%?" :target (file+head "blog/${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ("a" "area" plain "%?" :target (file+head "area/${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ("p" "project" plain "%?" :target (file+head "project/${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ("r" "resource" plain "%?" :target (file+head "resource/${slug}.org" "#+title: ${title}\n#+time: %<%Y%m%d%H%M%S>")
          :unnarrowed t)
         ))
 :config
 (unless (file-exists-p org-roam-directory)
   (make-directory org-roam-directory)))


(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "习惯"
                 :habit t)
          (:name "已过期"
                 :scheduled past
                 :deadline past)
          (:name "今日"
                 :time-grid t
                 :scheduled today
                 :deadline today)
         (:name "等待..."
                :todo "WAITING"
                :order 98)
         ))
  (org-agenda nil "a"))


(use-package plantuml-mode
  :ensure t
  :init
  (setq org-plantuml-jar-path (expand-file-name (concat user-emacs-directory "misc/plantuml.jar")))
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))


(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(provide 'setup-org)

