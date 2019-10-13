(add-hook 'after-init-hook 'global-company-mode)


(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html company-css))
                          (company-mode t)
			  (dumb-jump-mode)))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(provide 'setup-company)
