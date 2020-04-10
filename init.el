(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


(add-to-list 'load-path "~/.emacs.d/custom/")

(setq debug-on-error t)

(require 'setup-config)
(require 'setup-environment)
(require 'setup-ivy)
(require 'setup-programming)
(require 'setup-org)
(require 'setup-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   (quote
    (org-protocol org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (org-drill request lsp-ui flycheck company-lsp lsp-mode company-ctags company-go go-mode org-super-agenda htmlize neotree company-web company web-mode js2-mode doom-themes projectile which-key counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


