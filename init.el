;; os
;; see https://github.com/redguardtoo/emacs.d/blob/master/init.el
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-win* (eq system-type 'windows-nt))
(setq *is-cygwin* (eq system-type 'cygwin) )
(setq *is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

;; package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
(add-to-list 'load-path "~/.emacs.d/custom/")

(package-initialize)

(require 'setup-config)
(require 'setup-environment)
(require 'setup-ivy)
(require 'setup-programming)
(require 'setup-org)
(require 'setup-window)


(setq gc-cons-threshold 4000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   (quote
    (org-protocol org-expiry org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (org-brain use-package simple-modeline org-drill request lsp-ui flycheck company-lsp lsp-mode company-ctags company-go go-mode org-super-agenda htmlize neotree company-web company web-mode js2-mode doom-themes projectile which-key counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


