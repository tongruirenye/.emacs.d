;;
;; Edit
;;

;; ivy
(use-package counsel
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-c b" . counsel-bookmark)
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
  :ensure t
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

;; mwim
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))


;; misc
(setq make-backup-files nil)
(prefer-coding-system 'utf-8-unix)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defun cc-register-save ()
  (interactive)
  (point-to-register ?\a))

(defun cc-register-jump ()
  (interactive)
   (jump-to-register ?\a))

(global-set-key (kbd "C-x l") 'cc-register-save)
(global-set-key (kbd "C-x m") 'cc-register-jump)
(global-set-key (kbd "C-l") 'set-mark-command)


(when (not (display-graphic-p))
  (global-set-key (kbd "RET") 'newline-and-indent))

(provide 'setup-edit)
