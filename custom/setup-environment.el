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
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "KaiTi" :size 22))))
(when *is-mac*
  (setq system-time-locale "zh_CN.UTF-8")
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend) 
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Kaiti SC" :size 18))))

;; coding
(prefer-coding-system 'utf-8-unix)
(when *is-win*
  (setq locale-coding-system 'gb18030)    ;此句保证中文字体设置有效
  (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
  (setq file-name-coding-system 'gb18030) ; 设置文件名的编码为gb18030
  )

;; theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme cc-theme t)
  (doom-themes-visual-bell-config)
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


;; parrot
(use-package parrot
  :ensure t
  :config
  (parrot-mode))


;; shell
(use-package aweshell
  :load-path "~/.emacs.d/elpa/aweshell")

;; auto-save
(use-package auto-save
  :load-path "~/.emacs.d/elpa/auto-save"
  :config
  (setq auto-save-silent t)
  (auto-save-enable))

;; server-mode
(server-mode 1)


(provide 'setup-environment)
