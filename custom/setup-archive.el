

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
