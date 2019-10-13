
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))



;;; Custom
(add-to-list 'load-path "~/.emacs.d/custom/")

(setq debug-on-error t)

(require 'setup-environment)
(require 'setup-ivy)
(require 'setup-web-mode)
(require 'setup-company)
(require 'setup-capture)
(require 'setup-org)
(require 'setup-cpp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dumb-jump company-go go-mode org-super-agenda htmlize neotree company-web js2-refactor company web-mode js2-mode doom-themes projectile which-key counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


