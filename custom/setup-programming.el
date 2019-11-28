(add-hook 'after-init-hook 'global-company-mode)

;; web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html company-css))
                          (company-mode t)))


;; go
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(add-hook 'go-mode-hook 'display-line-numbers-mode)
(add-hook 'go-mode-hook 'flycheck-mode)

(add-hook 'before-save-hook 'gofmt-before-save)


;; elisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-elisp))
			  (company-mode)))


;; c/cxx
(add-hook 'c-mode-common-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-etags))
			  (company-mode)))
(add-hook 'c-mode-common-hook 'display-line-numbers-mode)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'setup-company)
