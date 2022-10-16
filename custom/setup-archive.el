

;; @see https://writequit.org/articles/emacs-org-mode-generate-ids.html
(defun cc-org-id-new (&optional prefix)
  "Create a new globally unique ID."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat prefix "-")))
         unique)
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@" (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))

(defun cc-auto-add-ids-to-todo-headlines ()
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" 100 t)
      (org-map-entries (lambda () (org-id-get-create)) "TODO=\"PROJECT\""))))


(defun cc-auto-add-custom-ids-to-headlines ()
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-customid:t" 100 t)
      (org-map-entries (lambda () (org-id-get-create))))))

(defun cc-org-mode-save-hook ()
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (cc-auto-add-ids-to-todo-headlines)))


;; protocol
(use-package org-protocol
  :ensure nil
  :config
  (server-start))
(require 'org-roam-protocol)


(defun cc-clock-out-hook()
  (when (org-entry-get-with-inheritance "POMO")
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
      (org-save-all-org-buffers))))

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


(add-hook 'org-clock-out-hook 'cc-clock-remove-clock-drawer)

;; modeline
;; (use-package simple-modeline
;;   :ensure t
;;   :config
;;   (setq simple-modeline--mode-line
;;   '((:eval
;;      (simple-modeline--format
;;       '(simple-modeline-segment-modified
;;         simple-modeline-segment-buffer-name
;;         simple-modeline-segment-position)
;;       '(simple-modeline-segment-misc-info
;;         simple-modeline-segment-input-method
;;         simple-modeline-segment-eol
;;         simple-modeline-segment-encoding
;;         simple-modeline-segment-vc
;;         simple-modeline-segment-process
;;         simple-modeline-segment-major-mode)))))
;;   :hook (after-init . simple-modeline-mode))


(use-package org-protocol
  :ensure nil
  :config
  (server-start))
(setq org-protocol-the-protocol "orgprotocol")

(defun cc-protocol-open-file (info)
  (when-let ((file (plist-get info :file)))
    (raise-frame)
    (if-let ((id (plist-get info :id)))
        (org-link-open-from-string (format "[[file:%s::#%s]]" (concat org-directory file) id))
      (find-file (concat org-directory file))))
  nil)
(push '("orgprotocol"  :protocol "cc-file" :function cc-protocol-open-file)
      org-protocol-protocol-alist)



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
	     (when (and drawer (not style)) 
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


(defun cc-org-todo-trigger-hook (list)
  (when (equal "DONE" (plist-get list :to))
    (let ((category (org-entry-get-with-inheritance "CATEGORY"))
          (project (org-roam-id-at-point))
          (style (org-entry-get (point) "STYLE"))
          (level (nth 0 (org-heading-components)))
          (s (org-copy-special))
          (time (format-time-string "%H:%M ")))
      (with-temp-buffer
      (org-mode)
      (yank)
      (goto-char (point-min))
      (cc-clock-remove-clock-drawer)
      (org-entry-put (point) "AREA" category)
      (org-entry-put (point) "PROJECT" (format "[id:%s]" project))
      (while (> level 1)
        (org-promote)
        (setq level (1- level)))
      (goto-char (point-min))
      (delete-char 2)
      (if style
          (replace-string "TODO" "LOGGING" nil 1 10)
        (replace-string "DONE" "FINISH" nil 1 10))
      (kill-ring-save (point-min) (point-max))
      (cc-org-roam-dailies-capture-today)))
    (org-reset-checkbox-state-maybe)
    (org-save-all-org-buffers)))

(add-hook 'org-trigger-hook #'cc-org-todo-trigger-hook)


(defun cc-clock-out-hook()
  (let ((category (org-entry-get-with-inheritance "CATEGORY"))
        (project (org-roam-id-at-point))
        (title (nth 4 (org-heading-components)))
        (tod (nth 2 (org-heading-components)))
        ts
        te
        s
        h
        m)
    (unless (equal "TASK" tod)
      (with-temp-buffer
        (org-mode)
        (insert "* LOGGING ")
        (insert (format-time-string "%H:%M "))
        (insert title)
        (org-entry-put (point) "AREA" category)
        (org-entry-put (point) "PROJECT" (format "[id:%s]" project))
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
        (cc-org-roam-dailies-capture-today)))
    (org-save-all-org-buffers)))


 (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* JOURNAL %<%H:%M> %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))
        ("t" "task" entry
         "* TASK %<%H:%M> %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")
         :clock-in)))


 ("a" "area" plain "%?" :target (file+head "area/${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("p" "project" plain "%?" :target (file+head "project/${slug}.org" "#+title: ${title}")
           :unnarrowed t)
