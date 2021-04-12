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


(defcustom cc-english-dir (concat cc-roam-dir "english/")
  "cc english dir"
  :group 'cc-english
  :type 'string)

(defcustom cc-english-resource "d:/laishixiong/"
  "cc english resource"
  :group 'cc-english
  :type 'string)

(defconst cc-english-custom-id "CUSTOM_ID")
(defconst cc-english-voice-start "VOICE_START")
(defconst cc-english-voice-duration "VOICE_DURATION")

(defface cc-english--question-face '((t :inherit default :height 4.0))
  "Face used for question"
  :group 'cc-english)

(defface cc-english--right-face '((t :inherit match))
  "Face used for right word"
  :group 'cc-english)

(defface cc-english--wrong-face '((t :inherit error))
  "Face used for wrong word"
  :group 'cc-english)

(defvar cc-english-drill-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'cc-english--drill-play)
    (define-key map "n" #'cc-english--drill-next)
    (define-key map "s" #'cc-english--drill-show)
    (define-key map "i" #'cc-english--drill-input)
    (define-key map "c" #'cc-english--drill-check)
    map)
  "key map")

(defun cc-english-drill-mode ()
  (kill-all-local-variables)
  (use-local-map cc-english-drill-mode-map)
  (setq major-mode 'cc-english-drill-mode
        mode-name "cc-english-drill-mode"
        buffer-read-only t)
  )

(defvar cc-english--drill-current 0)

(defun cc-english--drill-entry (entry)
  (interactive)
  (unless (get-buffer "**EnglishDrill**")
    (generate-new-buffer "**EnglishDrill**"))
  (switch-to-buffer "**EnglishDrill**")
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((w (window-width))
         (h (window-height))
         (hsep (cond ((> w 26) "   ")
                     ((> w 20) " ")
                     (t "")))
         (vsep (cond ((> h 17) "\n\n")
                     (t "\n")))
         (indent (make-string (/ (- w 7 (* 6 (length hsep))) 2) ?\s))
         (question (plist-get entry :id))
         beg end)
    (insert (make-string (/ (- h 7 (if (> h 12) 3 0)
                               (* 6 (1- (length vsep)))) 2) ?\n))
    (when (or (string= vsep "\n\n") (> h 12))
      (insert indent)
      (setq beg (point))
      (insert (format "%s " (propertize question
                                        'face 'cc-english--question-face
                                        'mouse-face 'mode-line-highlight
                                        'help-echo (format "https://en.wikipedia.org/wiki/%s" question))))
      (setq end (point))
      (put-text-property beg end 'question question)
      (put-text-property beg end 'indent indent))
    )
  (setq buffer-read-only t)
  (goto-char (point-min))
  (unless (eq major-mode 'cc-english-drill-mode)
    (cc-english-drill-mode))
  (cc-english--drill-play)
  )


(defun cc-english--drill-play ()
  (interactive)
  (let* ((question-pos (save-excursion
                (goto-char (text-property-not-all (point-min) (point-max) 'question nil))
                (point)))
        (question (get-text-property question-pos 'question))
        entry
        voice
        voice-path
        cmd)
    (unless question
      (error "no question"))
    (catch 'found
      (dolist (item cc-english--drill-queue)
        (when (equal question (plist-get item :id))
          (setq entry item)
          (throw 'found item)))
      )
    (unless entry
      (error "no entry"))
    (setq voice (plist-get entry :voice))
    (unless voice
      (setq voice ""))
    (string-match "\\[\\[file:\\(.*\\)\\]\\]" voice)
    (setq voice-path (match-string 1 voice))
    (unless voice-path
      (error "no voice"))
    (setq voice-path (concat cc-english-dir "laishixiong" (string-trim-left voice-path "..")))
    (when (file-exists-p voice-path)
      (setq voice-path (subst-char-in-string ?/ ?\\ voice-path))
    (setq cmd (format "vlc.exe --qt-start-minimized --play-and-exit --qt-notification=0 %s" voice-path))
    (call-process-shell-command cmd nil 0))
    ))

(defun cc-english--drill-input ()
  (interactive)
  (unless (get-buffer "**EnglishDrillInput**")
    (generate-new-buffer "**EnglishDrillInput**"))
  (switch-to-buffer-other-window "**EnglishDrillInput**")
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (unless (eq major-mode 'text-mode)
    (text-mode)
    (set (make-local-variable 'company-backends) '(company-css)))
  )

(defun cc-english--drill-check ()
  (interactive)
  (let* ((question-pos (save-excursion
                (goto-char (text-property-not-all (point-min) (point-max) 'question nil))
                (point)))
        (question (get-text-property question-pos 'question))
        entry
        inlist
        oulist
        i
        j)
    (unless question
      (error "no question"))
    (catch 'found
      (dolist (item cc-english--drill-queue)
        (when (equal question (plist-get item :id))
          (setq entry item)
          (throw 'found item)))
      )
    (unless entry
      (error "no entry"))
    (unless (get-buffer "**EnglishDrillInput**")
      (error "no input buffer"))
    (with-current-buffer "**EnglishDrillInput**"
      (setq inlist (nreverse (cc-english--split-sentence (buffer-substring-no-properties (point-min) (point-max)))))
      (setq oulist (nreverse (cc-english--split-sentence (plist-get entry :heading))))
      (erase-buffer)
      (setq i 0 j 0)
      (while (< i (length inlist))
        (if (< j (length oulist))
          (progn (if (equal (nth i inlist) (nth j oulist))
            (insert (format "%s " (propertize (nth i inlist)
                                              'face 'cc-english--right-face)))
            (insert (format "%s " (propertize (nth i inlist)
                                              'face 'cc-english--wrong-face)))))
          (insert (format "%s " (propertize (make-string (length (nth i inlist)) 45)
                                            'face 'cc-english--wrong-face))))
        (setq i (1+ i) j (1+ j)))
      (while (< i (length oulist))
        (insert (format "%s " (propertize (make-string (length (nth i oulist)) 120)
                                          'face 'cc-english--wrong-face)))
        (setq i (1+ i)))
      (insert "\n")
      (insert (format "%s" (propertize (plist-get entry :heading)
                                       'face 'cc-english--right-face)))
      (setq buffer-read-only t)
      )
    (switch-to-buffer-other-window "**EnglishDrillInput**")
  ))

(defun cc-english--drill-next ()
  (interactive)
  (if (>= cc-english--drill-current (length cc-english--drill-queue))
      (error "no more entrys")
    (progn 
      (cc-english--drill-entry (nth cc-english--drill-current cc-english--drill-queue))
      (setq cc-english--drill-current (1+ cc-english--drill-current)))))

(defun cc-english--drill-score ()
  (interactive)
  (ivy-read "Choose a Score:" '("S(15)" "A(7)" "B(5)" "C(3)" "D(1)")
                :action (lambda (w)
                          (cond
                           ((equal w "S(15)") )
                          ))
                ))

(defun cc-english--drill-session-save (id)
  (let (entry)
    (catch 'found
      (dolist (item cc-english--drill-session)
        (when (equal id (plist-get item :id))
          (setq entry item)
          (throw 'found item)))
      )
    (unless entry
      )
    )
  )

(defun cc-english--drill-show ()
  (interactive)
  (let* ((question-pos (save-excursion
                (goto-char (text-property-not-all (point-min) (point-max) 'question nil))
                (point)))
        (question (get-text-property question-pos 'question))
        (indent (save-excursion
                  (goto-char (text-property-not-all (point-min) (point-max) 'indent nil))
                  (get-text-property (point) 'indent)))
        entry)
    (unless question
      (error "no question"))
    (catch 'found
      (dolist (item cc-english--drill-queue)
        (when (equal question (plist-get item :id))
          (setq entry item)
          (throw 'found item)))
      )
    (unless entry
      (error "no entry"))
    (setq buffer-read-only nil)
    (goto-char question-pos)
    (end-of-line)
    (insert "\n")
    (insert indent)
    (insert (plist-get entry :heading))
    (insert "\n")
    (insert indent)
    (insert (plist-get entry :trans))
    (setq buffer-read-only t)
    (goto-char (point-min))
  ))


(defvar cc-english--drill-session nil)
(defvar cc-english--drill-queue nil)

(defun cc-english-drill ()
  (interactive)
  (let ((dir (cc-english--get-dir))
        (bufname (concat (buffer-file-name) ".drill"))
        )
    (unless dir
      (error "no english book dir found!!!"))
    
    (setq cc-english--drill-session nil)
    (when (file-exists-p bufname)
      (let ((buffer (find-file-noselect bufname)))
        (with-current-buffer buffer
          (org-mode)
          (org-map-entries
           (apply-partially #'cc-english--drill-item-session cc-english--drill-session)))))

    (setq cc-english--drill-queue nil)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (org-map-entries
       #'cc-english--drill-item-queue))

    (unless cc-english--drill-queue
      (error "no review entrys"))
    (setq cc-english--drill-current 0)
    (setq cc-english--drill-queue (nreverse cc-english--drill-queue))
    (cc-english--drill-entry (nth cc-english--drill-current cc-english--drill-queue))
    (setq cc-english--drill-current (1+ cc-english--drill-current))
  ))



(defun cc-english--drill-item-session (session)
  (let ((id (nth 4 (org-heading-components)))
        (scheduled (org-entry-get (point) "SCHEDULED"))
        (quality (org-entry-get (point) "QUALITY"))
        item
        )
    (setq item (plist-put item :id id))
    (setq item (plist-put item :scheduled scheduled))
    (setq item (plist-put item :quality quality))
    (push item session)
  ))

(defun cc-english--drill-item-queue ()
  (let ((id (org-entry-get (point) "CUSTOM_ID"))
        (heading (nth 4 (org-heading-components)))
        (voice (org-entry-get (point) "VOICE"))
        (trans (org-entry-get (point) "TRANS"))
        ;;(props (org-entry-properties))
        item)
    (when id
      (setq item (plist-put item :id id))
      (setq item (plist-put item :heading heading))
      (setq item (plist-put item :voice voice))
      (setq item (plist-put item :trans trans))
      (push item cc-english--drill-queue))))



;;;
;;; Make Lesson
;;;

(defun cc-english-sentence ()
  (interactive)
  (let* ((heading (org-heading-components))
         (sid (org-entry-get (point) cc-english-custom-id)))
    (unless sid
      (setq sid (cc-english--id-new "S" (nth 4 heading))))
    (org-entry-put (point) cc-english-voice-start "00")
    (org-entry-put (point) cc-english-voice-duration "00")))

(defun cc-english-sentence-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries #'cc-english-sentence)))

(defun cc-english-sentence-play ()
  (interactive)
  (let* ((voice-dir (cc-english--get-voice-dir))
         (voice-start (org-entry-get (point) cc-english-voice-start))
         (voice-end (org-entry-get (point) cc-english-voice-duration)))
    (unless voice-dir
      (error "no voice"))
    (call-process-shell-command (apply #'format "ffplay.exe -nodisp -noborder -autoexit -i \"%s\" -ss \"%s\" -t \"%s\"" (list voice-dir voice-start voice-end)) nil 0)))


;; (defun cc-english-sentence-voice-extract()
;;   (interactive)
;;   (let* ((voice-dir (cc-english--get-voice-dir))
;;          (sid (org-entry-get (point) cc-english-custom-id))
;;          (output-dir (concat cc-english-resource "sentence/" sid ".mp3"))
;;         (voice-start (org-entry-get (point) "VOICE_START"))
;;         (voice-end (org-entry-get (point) "VOICE_DURATION")))
;;     (print voice-dir)
;;     (print output-dir)
;;     (print sid)
;;     (print (apply #'format "ffmpeg.exe -i \"%s\" -acodec copy -ss \"%s\" -t \"%s\" \"%s\"" (list voice-dir voice-start voice-end output-dir)))
;;     (when (and sid voice-dir)
;;       (call-process-shell-command (apply #'format "ffmpeg.exe -i \"%s\" -acodec copy -ss \"%s\" -t \"%s\" \"%s\"" (list voice-dir voice-start voice-end output-dir)) nil 0)
;;       )
;;     ))

;; (defun cc-english-sentence-voice-extract-all ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (org-map-entries #'cc-english-sentence-voice-extract)))


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
  (let ((dir (concat cc-english-dir "/word")))
    (unless dir
      (error "no word dir found!!!"))
    
    (let* ((sid (org-entry-get (point) "CUSTOM_ID"))
           (sent (nth 4 (org-heading-components)))
           (res (counsel-rg (concat "^\\*\\s" id) dir))
           )
      (unless sid
        (error "no sentence CUSTOME_ID!!!"))

      (if (string-equal res "No matches found")
          (progn
            (let* ((cards (cc-english--get-card "WORD"))
                  (cdir (concat cc-english-dir dir "/word/"))
                  (default-directory (file-truename (expand-file-name dir)))
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

(defun cc-english--get-voice-dir ()
  (let ((dir)
        dirl)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+VOICE:" 200 t)
        (setq dirl (ivy--split-spaces (buffer-substring-no-properties (point) (line-end-position))))
        (when (and dirl (listp dirl))
          (setq dir (car dirl))
          (setq dir (concat cc-english-resource dir)))
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
    (format "%s%s" prefix rnd)
    )
  )

(defun cc-english--id-new (prefix &optional content)
  (let* ((id (or content ""))
         (rnd (md5 (format "%s%s" (random) id)))
         (cid (format "%s%s" prefix rnd))
         )
    (org-entry-put (point) "CUSTOM_ID" cid)
    cid
    ))

(defun cc-english--split-sentence (str)
  (let ((len (length str))
        res
        (i 0)
        (j 0))
    (while (and (string-match "[.,!? ]+" str i)
                (< i len))
      (if (>= (- (match-end 0) (match-beginning 0)) 1)
          (progn
            (push (substring str i (match-beginning 0)) res)
            (setq i (match-end 0)))))
    (when (< i len)
      (push (substring str i len) res))
    res))


(provide 'setup-english)
