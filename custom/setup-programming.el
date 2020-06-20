;;
;; Programming
;;

(use-package protobuf-mode
  :ensure nil)

;; lsp
(use-package lsp-mode
  :ensure t
  :commands (lsp-enable-which-key-integration lsp-format-buffer)
  :hook ((prog-mode . (lambda ()
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

;; web
(use-package web-mode
  :ensure t
  :mode "\\.\\(html?\\|css?\\|tm?pl\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

(provide 'setup-programming)
