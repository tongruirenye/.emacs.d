;;
;; Environment
;;

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

;; font
(set-default-font "JetBrains Mono-13.0")
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
        dashboard-items '((recents  . 10)
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
  (doom-themes-org-config))

;; modeline
(use-package simple-modeline
  :ensure t
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
