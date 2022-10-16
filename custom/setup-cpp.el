;;;
;;; cpp
;;;
;;;

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :ensure t
    :init (modern-c++-font-lock-global-mode t)))

(use-package citre
  :ensure t
  :bind (("C-x c j" . citre-jump+)
         ("C-x c k" . citre-jump-back)
         ("C-x c p" . citre-peek)
         ("C-x c a" . citre-ace-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  (setq citre-auto-enable-citre-mode-modes '(prog-mode))

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  :config
  (with-no-warnings
    (with-eval-after-load 'projectile
      (setq citre-project-root-function #'projectile-project-root))
    ))

(provide 'setup-cpp)
