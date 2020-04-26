;;; screen
(setq inhibit-startup-screen t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode -1)
(setq-default
 bidi-display-reordering nil
 blink-matching-paren nil
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 mode-line-default-help-echo nil
 show-help-function nil
 ring-bell-function #'ignore
 visible-bell nil
 )

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8-unix)
(setq make-backup-files nil)
(which-key-mode 1)
(electric-pair-mode t)
(defalias 'list-buffers 'ibuffer)

;;; font
(set-default-font "JetBrains Mono-13.0")
(when *is-win*
  (dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "微软雅黑" :size 20))))


;; see doom
(defun +doom-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

;;;###autoload
(defun doom-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively
     (cond ((featurep 'ivy) #'counsel-find-file)
           (#'find-file)))))

;;;###autoload
(defun my/open-config ()
  (interactive)
  (unless (file-directory-p my-config-dir)
    (message "no config dir"))
  (doom-project-browse my-config-dir))

(defun my/banner ()
  (mapc (lambda (line)
	  (insert (+doom-dashboard--center 80 line))
	  (insert "\n"))
	'("
 _______           _______  _        _______           _______  _______ 
(  ____ \\|\\     /|(  ____ \\( (    /|(  ____ \\|\\     /|(  ___  )(  ___  )
| (    \\/| )   ( || (    \\/|  \\  ( || (    \\/| )   ( || (   ) || (   ) |
| |      | (___) || (__    |   \\ | || |      | (___) || (___) || |   | |
| |      |  ___  ||  __)   | (\\ \\) || |      |  ___  ||  ___  || |   | |
| |      | (   ) || (      | | \\   || |      | (   ) || (   ) || |   | |
| (____/\\| )   ( || (____/\\| )  \\  || (____/\\| )   ( || )   ( || (___) |
(_______/|/     \\|(_______/|/    )_)(_______/|/     \\||/     \\|(_______)"
	 )))


(defun my/menu ()
  (mapc (lambda (btn)
	  (when btn
	    (cl-destructuring-bind (label icon fn) btn
	      (insert
	       (with-temp-buffer
		 (insert-text-button
		  label
		  'action `(lambda (_) ,fn)
		  'follow-link t)
		 (+doom-dashboard--center 68 (buffer-string)))
	       "\n\n"))))
	`(("Config Files" "file-text"
	   (call-interactively 'my/open-config))
	  )))


(defun my/scratch ()
  (with-temp-buffer
    (dolist (widget-name '(banner menu))
      (funcall (intern (format "my/%s" widget-name)))
      (insert "\n"))
    (setq initial-scratch-message (buffer-string))))



(add-hook 'after-init-hook #'my/scratch t)

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; modeline
(use-package simple-modeline
  :config
  (setq simple-modeline--mode-line
  '((:eval
     (simple-modeline--format
      '(simple-modeline-segment-modified
        simple-modeline-segment-buffer-name
        simple-modeline-segment-position)
      '(simple-modeline-segment-misc-info
        simple-modeline-segment-input-method
        simple-modeline-segment-eol
        simple-modeline-segment-encoding
        simple-modeline-segment-vc
        simple-modeline-segment-process
        simple-modeline-segment-major-mode)))))
  :hook (after-init . simple-modeline-mode))


(provide 'setup-environment)
