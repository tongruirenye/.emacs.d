;;
;; Edit
;;

;; ivy
(use-package counsel
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-c k" . counsel-rg)
         ("C-c b" . counsel-bookmark)
         ("C-c d" . counsel-etags-find-tag-at-point)
         ("C-c o" . counsel-etags-grep)
         ("C-c g" . counsel-git))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode)))

;; avy
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))


;; linenum
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;; hight-line
(use-package hl-line
  :hook (after-init . global-hl-line-mode))

;; which-key
(use-package which-key
  :hook (after-init . which-key-mode))

;; elec-pair
(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; del selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; paren show
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; misc
(setq make-backup-files nil)
(prefer-coding-system 'utf-8-unix)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(provide 'setup-edit)
