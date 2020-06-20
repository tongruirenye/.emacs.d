;;
;; english
;;

(require 'org)
(require 'ivy)
(require 'setup-config)

(defgroup cc-english nil
  "cc english"
  :tag "CC English"
  :group 'cc)


(defcustom cc-english-dir (concat cc-org-dir "english/")
  "cc english dir"
  :group 'cc-english
  :type 'string)


(defun cc-english-sentence-word (&optional arg)
  "create word link"
  (interactive "P")
  (let* ((heading (org-heading-components))
         (words (cc-english--split-sentence (nth 4 heading)))
         (sid (org-entry-get (point) "CUSTOM_ID"))
         )
    (unless sid
      (cc-english--id-new "S" (nth 4 heading)))
    (unless (equal arg '(16))
      (ivy-read "Choose a Word:" words
                :action (lambda (w)
                          (cc-english--link-create "" "WID" w (downcase w))
                          ))
      )
    ))

(defun cc-english-sentence-phrase ()
  "create phrase link"
  (interactive)
  (let* ((heading (org-heading-components))
         (words (cc-english--split-sentence (nth 4 heading)))
         (sid (org-entry-get (point) "CUSTOM_ID"))
         )
    (unless sid
      (cc-english--id-new "S" (nth 4 heading)))
    (ivy-read "Choose a phrase:" words
              :action (lambda (w)
                        (cc-english--link-create "" "PID" w (cc-english--id-gen "P" w))
                        ))
    ))

(defun cc-english-sentence-grammar ()
  "create grammar link"
  (interactive)
  (let* ((heading (org-heading-components))
         (words (cc-english--split-sentence (nth 4 heading)))
         (sid (org-entry-get (point) "CUSTOM_ID"))
         )
    (unless sid
      (cc-english--id-new "S" (nth 4 heading)))
    (ivy-read "Choose a grammar:" words
              :action (lambda (w)
                        (cc-english--link-create "" "GID" w (cc-english--id-gen "G" w))
                        ))
    ))

(defun cc-english--word-open (id)
  (let ((dir (cc-english--get-dir)))
    (unless dir
      (error "no english book dir found!!!"))
    
    (let* ((sid (org-entry-get (point) "CUSTOM_ID"))
           (sent (nth 4 (org-heading-components)))
           (res (counsel-rg (concat "^\\*\\s" id) (concat cc-english-dir dir "/word")))
           )
      (unless sid
        (error "no sentence CUSTOME_ID!!!"))

      (if (string-equal res "No matches found")
          (progn
            (let* ((cards (cc-english--get-card "WORD"))
                  (cdir (concat cc-english-dir dir "/word/"))
                  (default-directory (file-truename (expand-file-name cdir)))
                  )
              (if (and cards (listp cards))
                  (ivy-read "Choose Card:" cards
                            :require-match t
                            :action (lambda (card)
                                      (let ((buffer (find-file-noselect (concat cdir card))))
                                        (with-current-buffer buffer
                                          (goto-char (point-max))
                                          (org-insert-heading nil nil t)
                                          (insert id)
                                          (org-insert-subheading nil)
                                          (org-insert-subheading nil)
                                          (insert sent)
                                          (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                          (org-up-heading-safe)
                                          (org-up-heading-safe)
                                          (org-entry-put (point) "VOICE" (format "[[file:../voice/%s.mp3]]" id))
                                          )
                                        (switch-to-buffer-other-window buffer)
                                        )))
                (ivy-read "Choose Card:" #'read-file-name-internal
                          :action (lambda (x)
                                    (let ((buffer (find-file-noselect x)))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                      (org-insert-heading nil nil t)
                                      (insert id)
                                      (org-insert-subheading nil)
                                      (org-insert-subheading nil)
                                      (insert sent)
                                      (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                      (org-up-heading-safe)
                                      (org-up-heading-safe)
                                      (org-entry-put (point) "VOICE" (format "[[file:../voice/%s.mp3]]" id))
                                      )
                                    (switch-to-buffer-other-window buffer)))))
              ))))))
(org-link-set-parameters "WID" :follow #'cc-english--word-open)

(defun cc-english--phrase-open (id)
  (let ((dir (cc-english--get-dir)))
    (unless dir
      (error "no english book dir found!!!"))
    
    (let* ((sid (org-entry-get (point) "CUSTOM_ID"))
           (sent (nth 4 (org-heading-components)))
           (pros (org-entry-properties))
           (val (format "[[PID:%s]]" id))
           (key)
           (res (counsel-rg id (concat cc-english-dir dir "/phrase")))
           )
      (unless sid
        (error "no sentence CUSTOME_ID!!!"))
      (catch 'found
        (dolist (item pros)
          (when (equal val (cdr item))
            (setq key (car item))
            (throw 'found item)))
        )
      (unless key
        (error "no phrase property"))

      (if (string-equal res "No matches found")
          (progn
            (let* ((cards (cc-english--get-card "PHRASE"))
                  (cdir (concat cc-english-dir dir "/phrase/"))
                  (default-directory (file-truename (expand-file-name cdir))))
              (if (and cards (listp cards))
                  (ivy-read "Choose Card:" cards
                            :require-match t
                            :action (lambda (card)
                                      (let ((buffer (find-file-noselect (concat cdir card))))
                                        (with-current-buffer buffer
                                          (goto-char (point-max))
                                          (org-insert-heading nil nil t)
                                          (insert (downcase key))
                                          (org-insert-subheading nil)
                                          (insert sent)
                                          (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                          (org-up-heading-safe)
                                          (org-entry-put (point) "CUSTOM_ID" id)
                                          )
                                        (switch-to-buffer-other-window buffer)
                                        )))
                (ivy-read "Choose Card:" #'read-file-name-internal
                        :action (lambda (card)
                                  (let ((buffer (find-file-noselect card)))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                          (org-insert-heading nil nil t)
                                          (insert (downcase key))
                                          (org-insert-subheading nil)
                                          (insert sent)
                                          (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                          (org-up-heading-safe)
                                          (org-entry-put (point) "CUSTOM_ID" id)
                                          )
                                    (switch-to-buffer-other-window buffer)
                                    ))))
              ))))))
(org-link-set-parameters "PID" :follow #'cc-english--phrase-open)

(defun cc-english--gramma-open (id)
  (let ((dir (cc-english--get-dir)))
    (unless dir
      (error "no english book dir found!!!"))
    
    (let* ((sid (org-entry-get (point) "CUSTOM_ID"))
           (sent (nth 4 (org-heading-components)))
           (pros (org-entry-properties))
           (val (format "[[GID:%s]]" id))
           (key)
           (res (counsel-rg id (concat cc-english-dir dir "/gramma")))
           )
      (unless sid
        (error "no sentence CUSTOME_ID!!!"))
      (catch 'found
        (dolist (item pros)
          (when (equal val (cdr item))
            (setq key (car item))
            (throw 'found item)))
        )
      (unless key
        (error "no gramma property"))

      (if (string-equal res "No matches found")
          (progn
            (let* ((cards (cc-english--get-card "GRAMMA"))
                  (cdir (concat cc-english-dir dir "/gramma/"))
                  (default-directory (file-truename (expand-file-name cdir))))
              (if (and cards (listp cards))
                  (ivy-read "Choose Card:" cards
                            :require-match t
                            :action (lambda (card)
                                      (let ((buffer (find-file-noselect (concat cdir card))))
                                        (with-current-buffer buffer
                                          (goto-char (point-max))
                                          (org-insert-heading nil nil t)
                                          (insert (downcase key))
                                          (org-insert-subheading nil)
                                          (insert sent)
                                          (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                          (org-up-heading-safe)
                                          (org-entry-put (point) "CUSTOM_ID" id)
                                          )
                                        (switch-to-buffer-other-window buffer)
                                        )))
                (ivy-read "Choose Card:" #'read-file-name-internal
                        :action (lambda (card)
                                  (let ((buffer (find-file-noselect card)))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                          (org-insert-heading nil nil t)
                                          (insert (downcase key))
                                          (org-insert-subheading nil)
                                          (insert sent)
                                          (org-entry-put (point) "SID" (format "[[SID:%s]]" sid))
                                          (org-up-heading-safe)
                                          (org-entry-put (point) "CUSTOM_ID" id)
                                          )
                                    (switch-to-buffer-other-window buffer)
                                    ))))
              ))))))
(org-link-set-parameters "GID" :follow #'cc-english--gramma-open)

(defun cc-english--sentence-open (id)
  (let ((dir (cc-english--get-dir))
        res)
    (unless dir
      (error "no english book dir found!!!"))
    
    (setq res (counsel-rg (concat "CUSTOM_ID:\\s+" id) (concat cc-english-dir dir "/lession")))
    (if (string-equal res "No matches found")
        (error "no sentence"))))
(org-link-set-parameters "SID" :follow #'cc-english--sentence-open)


(defun cc-english--get-dir ()
  (let ((dir)
        dirl)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+ENGLISHBOOK:" 100 t)
        (setq dirl (ivy--split-spaces (buffer-substring (point) (line-end-position))))
        (when (and dirl (listp dirl))
          (setq dir (car dirl)))
        ))
    dir))
 
(defun cc-english--get-card (kind)
  (let ((cards))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^#\\+ENGLISH" kind ":") 200 t)
        (setq cards (ivy--split-spaces (buffer-substring (point) (line-end-position))))
        ))
    cards))

(defun cc-english--link-create (prefix kind property id)
  (org-entry-put (point) property (format "[[%s:%s%s]]" kind prefix id)))


(defun cc-english--id-gen (prefix &optional content)
  (let* ((id (or content ""))
         (rnd (md5 (format "%s%s" (random) id)))
         )
    (format "%s%s" prefix (substring rnd 0 12))
    )
  )

(defun cc-english--id-new (prefix &optional content)
  (let* ((id (or content ""))
         (rnd (md5 (format "%s%s" (random) id)))
         )
    (org-entry-put (point) "CUSTOM_ID" (format "%s%s" prefix (substring rnd 0 12)))
    ))

(defun cc-english--split-sentence (str)
  (let ((len (length str))
        res
        (i 0)
        (j 0))
    (message str)
    (while (and (string-match "[.,!? ]+" str i)
                (< i len))
      (if (>= (- (match-end 0) (match-beginning 0)) 1)
          (progn
            (push (substring str i (match-beginning 0)) res)
            (setq i (match-end 0)))))
    (when (< i len)
      (push (substring str i len) res))
    (print res)
    res))


(provide 'setup-english)
