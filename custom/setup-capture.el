(require 'ivy)

(defvar cc-english-mdx-url "http://localhost:8000/%s")

(defvar cc-english-voice-src-path "d:/english/voice/")

(defvar cc-english-voice-path "../audio/")
(defvar cc-english-voice-dest-path "d:/mydata/org/english/audio/")

(defvar cc-english-card-path "d:/mydata/org/english/word/")

(defvar cc-english-sentence-path "d:/mydata/org/english/sentence/")


(defvar cc-english-word "")
(defvar cc-english-definition "")
(defvar cc-english-voice "")


(defun cc-english-choose-card ()
  (let ((default-directory (file-truename (expand-file-name cc-english-card-path))))
    (ivy-read "Peek a Card" #'read-file-name-internal
              :require-match t
              :action (lambda (x)
                        (let ((buf (find-file-noselect x)))
                          (set-buffer buf)
                          (with-current-buffer buf
                            (unless (eq major-mode 'org-mode)
                              (org-mode))
                            (goto-char (point-max))
                            (insert "\n"))))
	      )))

(defun cc-english-word-prompt ()
  "prompt a english word"
  (setq cc-english-word (read-from-minibuffer "Word: ")))

(defun cc-english-get-definition (word)
  (cc--english-mdx-search-definition word))


(defun cc-english-get-pronucation (word)
  (cc--english-voice-search word))

(defun cc-english-get-sentence ()
  (cc--english-sentence-search))


(defun cc--english-mdx-search-definition1 (word)
  (let ((def (cc--mdx-search word)))
    (ivy-read "Definition:" def
	      :action (lambda (choose)
			(setq cc-english-definition choose)))))

(defun cc--english-mdx-search-definition (word)
  (let ((cc-english-definition ""))
    (cc--english-mdx-search-definition1 word)
    cc-english-definition))


(defun cc--english-mdx-search-definition1 (word)
  (let ((def (cc--english-mdx-search word)))
    (ivy-read "Peek a Definition:" def
	      :action (lambda (choose)
			(setq cc-english-definition choose)))))

(defun cc--english-mdx-search (word)
  (let* ((url (format cc-english-mdx-url (url-hexify-string word)))
	 dom
	 (candlist '("not found"))
	 (candstr ""))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (re-search-forward "200 OK" nil 'move)
	(pop candlist)
	(setq dom (libxml-parse-html-region (point) (point-max)))
	(dolist (caption (dom-by-class dom "caption"))
	  (setq candstr "")
	  (dolist (child (dom-children caption))
	    (if (and (listp child) (nth 1 child))
		(setq candstr (concat candstr (dom-text child)))))
	  (setq candstr (concat candstr (dom-text caption)))
	  (push candstr candlist)))
      (kill-buffer (current-buffer)))
    candlist))


(defun cc--english-voice-search (word)
  (let* ((path (concat cc-english-voice-src-path (substring word 0 1)))
	 (cc-english-voice "")
	(default-directory (file-truename (expand-file-name path))))
    (ivy-read "Peek a Voice" #'read-file-name-internal
              :initial-input word
	      :require-match t
	      :action (lambda (x)
			(let ((df (concat cc-english-voice-dest-path (file-name-nondirectory x))))
			  (if (and (file-exists-p x) (not (file-exists-p df)))
			  (copy-file x df)))
		        (setq cc-english-voice (concat cc-english-voice-path (file-name-nondirectory x)))))
    cc-english-voice))


(defun cc--english-sentence-search ()
  (let* ((default-directory (file-truename (expand-file-name cc-english-sentence-path)))
	 (cc-sentence-voice ""))
    (ivy-read "Peek a Sentence" #'read-file-name-internal
	      :action (lambda (x)
		        (setq cc-sentence-voice (concat "../sentence/l3/" (file-name-nondirectory x)))))
    cc-sentence-voice))


(defun cc-spend-item-promot ()
  (let ((cc-spend-item ""))
    (ivy-read "Peek a Item:" '("Food" "House" "Clothes" "Transport" "Study" "Relation" "Family" "Other")
	      :action (lambda (x)
			(setq cc-spend-item x)))
    cc-spend-item))

(provide 'setup-capture)
