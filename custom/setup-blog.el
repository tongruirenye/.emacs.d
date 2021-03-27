;;;
;;; Blog
;;;


(use-package ox-hugo
  :ensure t
  :after ox)

;; see https://willschenk.com/articles/2021/emacs_blogging_mode/
(defvar blog-mode-entries nil)

(defun blog-mode-parse (file)
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
  (setq blog-mode-entries nil)
  (dolist (file (directory-files cc-blog-dir t "\\.org"))
    (let ((entry (blog-mode-parse file)))
      (if entry
          (push entry blog-mode-entries)))))


(define-derived-mode blog-mode tabulated-list-mode "blog-mode" "Major mode Blog Mode, to edit hugo blogs"
  (setq tabulated-list-format [("Title" 30 t)
                               ("Draft" 5 nil)
                               ("Date"  11 t)
                               ("Category" 11 t)
                               ("Tags" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (use-local-map blog-mode-map)
  (tabulated-list-init-header))

(defun blog-list ()
  (interactive)
  (pop-to-buffer "*Blog Mode*" nil)
  (blog-mode)
  (blog-mode-refresh-data)
  (setq tabulated-list-entries (-non-nil blog-mode-entries))
  (tabulated-list-print t))

(defvar blog-mode-map nil "keymap for blog-mode")

(setq blog-mode-map (make-sparse-keymap))

(define-key blog-mode-map (kbd "o") 'blog-mode-open)
(define-key blog-mode-map (kbd "<return>") 'blog-mode-open)
(define-key blog-mode-map (kbd "d") 'blog-mode-drafts)
(define-key blog-mode-map (kbd "a") 'blog-mode-all)
(define-key blog-mode-map (kbd "r") 'blog-mode-refresh-all)
(define-key blog-mode-map (kbd "RET") 'blog-mode-open)


(defun blog-mode-open ()
  (interactive)
  (find-file (tabulated-list-get-id)))

(defun blog-mode-refresh-all ()
  (interactive)
  (progn
    (blog-mode-refresh-data)
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

(defun blog-mode-all () 
  (interactive)
  (progn
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

(defun blog-mode-drafts () 
  (interactive)
  (progn
    (setq tabulated-list-entries 
          (-filter (lambda (x)
                     (string= "true"
                              (aref (car (cdr x)) 1))) (-non-nil blog-mode-entries)))
    (tabulated-list-print t)))


(global-set-key (kbd "C-c q") 'blog-list)

(provide 'setup-blog)
