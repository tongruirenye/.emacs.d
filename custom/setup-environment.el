;;
;; Environment
;;

(require 'setup-config)

;; base
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

;; setting
(when *is-win*
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K))

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb
;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; font
(set-face-font 'default cc-default-font)
(when *is-win*
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "微软雅黑" :size 20))))


;; dashbord
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Life Begins at the End of Your Comfort Zone"
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 5)
                          (bookmarks . 5))
        dashboard-set-footer t
        dashboard-footer-icon ""
        dashboard-footer-messages (list (format "Powered by ChenChao, %s" (format-time-string "%Y")))
        )
  :config
  (dashboard-setup-startup-hook))

;; theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme cc-theme t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; modeline
(use-package doom-modeline
  :ensure t
  :init
  (unless cc-use-all-the-icons
    (setq all-the-icons-color-icons nil))
  :hook (after-init . doom-modeline-mode))


;; treemacs
(use-package treemacs
  :ensure t
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           (not cc-use-all-the-icons))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(provide 'setup-environment)
