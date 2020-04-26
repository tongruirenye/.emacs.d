;;
;; Package
;;

(require 'setup-config)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(load (setq custom-file (expand-file-name (concat user-emacs-directory ".emacs-custom.el"))) t t)

(provide 'setup-package)

