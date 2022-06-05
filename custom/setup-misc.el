;;;
;;; Msic
;;;


;; kindle pc filter copy content
(defun cc-filter-kindlepc-content ()
  "kindle pc filter copy content"
  (interactive)
  (let ((xyz "")
        abc
        def)
    (setq abc (current-kill 0))
    (setq def (split-string abc "\n"))
    (when (eq 3 (length def))
      (dolist (item (split-string (nth 0 def) " "))
        (setq xyz (concat xyz item)))
      (insert xyz))))


(defun cc-filter-applebook-content ()
  (interactive)
  (let ((xyz "")
        abc
        def)
    (setq abc (current-kill 0))
    (setq def (split-string abc "\n"))
    (if (eq 3 (length def))
        (insert (nth 0 def))
      (insert abc))))
(global-set-key (kbd "C-c c") ' cc-filter-applebook-content)


(defun cc-tid-get-create()
  (interactive)
  (org-entry-put (point) "TID" (org-id-new)))


(defun cc-insert-recent-pic-link ()
  (interactive)
  (shell-command "powershell.exe C:\\Users\\Moon\\scoop\\shims\\listFile.ps1" t))


(defun cc-get-links (links)
  (let ((link (nth 4 (org-heading-components))))
    (push link links)
  ))

(defun cc-insert-link ()
  (interactive)
  (let* ((loc (org-entry-get (point) "LOCATION")))
    (when loc
      (ivy-read (concat loc ":") '()
                :action (lambda (l)
                          (org-end-of-subtree)
                          (org-insert-heading '(16))
                          (insert (format "[[file:%s/%s.org][%s]]" loc l l))
                          ))
      )))


;; (directory-files (concat cc-org-dir "book/") nil "\\.[pe]")
(defun cc-sync-book-index()
  (interactive)
  (let* ((loc (org-entry-get (point) "LOCATION"))
         (poss (save-excursion
                 (org-end-of-meta-data t)
                 (point)))
         (pose (save-excursion
                (org-end-of-subtree)
                (point)))
         (dir (concat cc-org-dir loc)))
    (when loc
      (goto-char poss)
      (kill-region poss pose)
      (dolist (file (directory-files dir nil "\\.[pe]"))
        (insert (format "** [[file:%s/%s][%s]]" loc file file))
        (insert "\n")))))


(defun cc-change-agenda-files()
  (interactive)
  (let* ((loc (org-entry-get (point) "LOCATION")))
    (when loc
      (setq org-agenda-files '())
      (setq org-agenda-files (directory-files (concat cc-org-dir loc) t "\\.org")))
    ))


(defun cc-parse-agenda-link (s)
  (pcase (with-temp-buffer
	   (let ((org-inhibit-startup nil))
	     (insert s)
	     (org-mode)
	     (goto-char (point-min))
	     (org-element-link-parser)))
    (`nil "")
    (link (org-element-property :path link))))

(defun cc-get-agenda-link-file ()
  (let* ((title (nth 4 (org-heading-components)))
         (link (cc-parse-agenda-link title)))
    (when (and link  (not (string-empty-p link)))
      (push (concat cc-org-dir link) org-agenda-files))))
     
    
(defun cc-change-agenda-subject()
  (interactive)
  (setq org-agenda-files '())
  (org-map-entries #'cc-get-agenda-link-file t 'tree))



(global-set-key (kbd "C-c c") 'cc-filter-kindlepc-content)
(global-set-key (kbd "C-c l") 'cc-insert-recent-pic-link)

(defun cc-refile-copy-link()
  (interactive)
  (let ((todo (nth 2 (org-heading-components)))
        (title (nth 4 (org-heading-components)))
        (id (org-id-get-create)))
    (with-temp-buffer
      (let ((org-inhibit-startup nil))
        (insert (format "* %s [[id:%s][%s]]" todo id title))
        (org-mode)
        (org-refile-copy)
        (org-save-all-org-buffers)))))

(provide 'setup-misc)
