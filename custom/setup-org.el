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
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(a)" "|" "DONE(d)")
                            (type "DECISION" "ISSUE" "|" "QUESTION")
          (type "GOAL"  "JOURNAL" "PROJ" "SUBJECT" "DOCUMENT" "INBOX" "|" "COMPLETE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROJ" . "blue")
	      ("INPROGRESS" . "spring green")
          ("DOCUMENT" . "tomato")
          ("SUBJECT" . "chocolate")
	      ("DONE" . "gray")))
 (setq org-use-fast-todo-selection t
  org-clock-into-drawer t
  org-log-into-drawer t
  org-agenda-block-separator nil
  org-log-done 'time
  org-deadline-warning-days 7
  org-agenda-skip-scheduled-if-deadline-is-shown t
  org-archive-location (concat org-directory "archive/archive.org::* From %s")
  org-agenda-files (directory-files cc-org-dir t "index.org")
  org-attach-dir-relative t
  org-default-notes-file (concat org-directory "phone/capture.org")
  org-refile-targets '((org-agenda-files . (:level . 1))
                       (nil . (:todo . "SUBJECT")))
  org-clock-in-switch-to-state (lambda (s)
                                 (if (or (equal s "TODO") (equal s "NEXT"))
                                     "INPROGRESS"
                                   nil))
  ))

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
	           (cc-remove-drawer-at (point)))))))))
  (org-save-all-org-buffers))


(defun cc-clock-in-hook ()
  (let ((pomo (or (org-entry-get-with-inheritance "POMO") "Focus...")))
    (when *is-win*
      (call-process-shell-command (apply #'format "MiniPomodoro.exe \"%s\"" (list pomo)) nil 0))
    (org-save-all-org-buffers)))

;; pomodoro
(use-package org-pomodoro
  :ensure t
  :hook ((org-clock-in . cc-clock-in-hook)
         (org-clock-out . cc-clock-remove-clock-drawer))
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
  (when *is-win*
    (setq org-roam-database-connector 'sqlite3))
  (setq org-roam-directory (concat cc-org-dir "roam")
        org-roam-v2-ack t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory)))

;; yankpad
(use-package yankpad
  :ensure t
  :defer 10
  :config
  (bind-key "<f11>" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  (setq yankpad-default-category "company"))


(provide 'setup-org)

