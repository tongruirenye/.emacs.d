;;;
;;; agenda list
;;;

(defvar cc-agenda-mode-entries nil)


(defun cc-agenda-mode-parse-entry ()
  (let* ((heading (org-heading-components))
         (role (org-entry-get (point) "ROLE"))
         (schedule (org-entry-get (point) "SCHEDULED"))
         (deadline (org-entry-get (point) "DEADLINE")))
    
  )

(defun cc-agenda-mode-parse-file (file)
  (let ((buffer (find-file-noselect file))
         keyword
         key
         val
         (title "")
         (draft "")
         (date "")
         (category "")
         (tags ""))
    (with-current-buffer buffer
      (goto-char (point-min))
      (org-map-entries #'cc-english-sentence)
      (while (re-search-forward org-keyword-regexp 200 t)
        (setq keyword (org-element-at-point))
        (when (memq (org-element-type keyword) '(keyword))
          (setq key (org-element-property :key keyword))
          (setq val (org-element-property :value keyword))
          (cond
           ((string-equal key "TITLE") (setq title val))
           ((string-equal key "DATE") (setq date val))
           ((string-equal key "HUGO_TAGS") (setq tags val))
           ((string-equal key "HUGO_CATEGORIES") (setq category val))
           ((string-equal key "HUGO_DRAFT") (setq draft val)))
          ))
      (kill-buffer buffer))
    (list file (vector title draft date category tags))))

(defun blog-mode-refresh-data ()
  (setq cc-agenda-mode-entries nil)
  (dolist (file (directory-files cc-sync-dir t "\\.org"))
    (let ((entry (cc-agenda-mode-parse-entry file)))
      (if entry
          (push entry cc-agenda-mode-entries)))))

(define-derived-mode cc-agenda-mode tabulated-list-mode "cc-agenda-mode" "Major mode cc agenda Mode"
  (setq tabulated-list-format [("时间" 30 t)
                               ("周日" 5 nil)
                               ("周一"  11 t)
                               ("周二" 11 t)
                               ("周三" 11 t)
                               ("周四" 11 t)
                               ("周五" 11 t)
                               ("周六" 0 nil)])
  (setq tabulated-list-padding 2)
  ;;(setq tabulated-list-sort-key (cons "Date" t))
  (use-local-map cc-agenda-mode-map)
  (tabulated-list-init-header))

(defun blog-list ()
  (interactive)
  (pop-to-buffer "*CC Agenda Mode*" nil)
  (cc-agenda-mode)
  (cc-agenda-parse)
  (setq tabulated-list-entries (-non-nil blog-mode-entries))
  (tabulated-list-print t))

(defvar cc-agenda-mode-map nil "keymap for blog-mode")

(setq cc-agenda-mode-map (make-sparse-keymap))

(define-key blog-mode-map (kbd "o") 'blog-mode-open)
(define-key blog-mode-map (kbd "<return>") 'blog-mode-open)
(define-key blog-mode-map (kbd "d") 'blog-mode-drafts)
(define-key blog-mode-map (kbd "a") 'blog-mode-all)
(define-key blog-mode-map (kbd "r") 'blog-mode-refresh-all)
(define-key blog-mode-map (kbd "RET") 'blog-mode-open)
