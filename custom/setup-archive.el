

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

