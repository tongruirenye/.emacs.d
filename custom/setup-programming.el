;;
;; Programming
;;

;; shell
(when *is-mac*
  (use-package exec-path-from-shell
    :ensure t)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOPROXY"))


;; project
(use-package find-file-in-project
  :ensure t)
(global-set-key (kbd "C-c p f") 'find-file-in-project)
(global-set-key (kbd "C-c p d") 'find-file-in-current-directory)


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))


;; protobuf
(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; yaml
(use-package yaml-mode
  :ensure t)
(add-hook 'yaml-mode-hook
      '(lambda ()
         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; web
(use-package web-mode
  :ensure t
  :mode "\\.\\(html?\\|css?\\|tm?pl\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


;; js
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

;; go
(use-package go-mode
  :ensure t)

;; citre
(use-package citre
    :ensure t
    :bind (("C-x c j" . citre-jump+)
           ("C-x c k" . citre-jump-back)
           ("C-x c p" . citre-peek)
           ("C-x c a" . citre-ace-peek)
           ("C-x c u" . citre-update-this-tags-file))
    :init
    (require 'citre-config)
    (setq citre-auto-enable-citre-mode-modes '(c++-mode))

    (defun citre-jump+ ()
      "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions)))))

;; cpp
(use-package modern-cpp-font-lock
  :ensure t)

;; lsp
(use-package lsp-mode
  :ensure t
  :commands (lsp-enable-which-key-integration lsp-format-buffer)
  :hook ((go-mode . (lambda ()
                      (lsp-deferred)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)))
         (python-mode . (lambda ()
                          (lsp-deferred)))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)
                       (add-hook 'before-save-hook #'lsp-format-buffer t t))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-guess-root nil
        lsp-flycheck-live-reporting nil
        lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-signature-auto-activate nil

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil))


(provide 'setup-programming)
