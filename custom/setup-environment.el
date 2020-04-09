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

(setq make-backup-files nil)
(which-key-mode 1)
(electric-pair-mode t)

;;; font
(set-default-font "JetBrains Mono-13.0")
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "微软雅黑" :size 20)))

(prefer-coding-system 'utf-8-unix)


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


(defun my/poem ()
  (mapc (lambda (line)
	  (insert (propertize (+doom-dashboard--center 64 line) 'face 'font-lock-comment-face))
	  (insert "\n"))
	'("黄色的树林里分出两条路,"
	  "可惜我不能同时去涉足,"
	  "我在那路口久久伫立,"
	  "我向着一条路极目望去,"
	  "直到它消失在丛林深处."
	  "但我选了另外一条路,"
	  "它荒草萋萋,十分幽寂,"
	  "显得更诱人,更美丽,"
	  "虽然在这条小路上,"
	  "很少留下旅人的足迹,"
	  "那天清晨落叶满地,"
	  "两条路都未经脚印污染."
	  "啊,留下一条路等改日再见!"
	  "但我知道路径延绵无尽头,"
	  "恐怕我难以再回返,"
	  "也许多少年后在某个地方,"
	  "我将轻声叹息将往事回顾:"
	  "一片树林里分出两条路——"
	  "而我选择了人迹更少的一条,"
	  "从此决定了我一生的道路."
	  )))


(defun my/scratch ()
  (with-temp-buffer
    (dolist (widget-name '(banner menu poem))
      (funcall (intern (format "my/%s" widget-name)))
      (insert "\n"))
    (setq initial-scratch-message (buffer-string))))



(add-hook 'after-init-hook #'my/scratch t)
(add-hook 'emacs-startup-hook '(lambda () (goto-char (point-min))))


(require 'doom-themes)

;; Global settings (defaults)
;;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
;;(doom-themes-neotree-config)
;; or for treemacs users
;;(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


(provide 'setup-environment)
