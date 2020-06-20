
;;  _______           _______  _        _______           _______  _______ 
;; (  ____ \\|\\     /|(  ____ \\( (    /|(  ____ \\|\\     /|(  ___  )(  ___  )
;; | (    \\/| )   ( || (    \\/|  \\  ( || (    \\/| )   ( || (   ) || (   ) |
;; | |      | (___) || (__    |   \\ | || |      | (___) || (___) || |   | |
;; | |      |  ___  ||  __)   | (\\ \\) || |      |  ___  ||  ___  || |   | |
;; | |      | (   ) || (      | | \\   || |      | (   ) || (   ) || |   | |
;; | (____/\\| )   ( || (____/\\| )  \\  || (____/\\| )   ( || )   ( || (___) |
;; (_______/|/     \\|(_______/|/    )_)(_______/|/     \\||/     \\|(_______)


;; Os
(setq *is-mac* (eq system-type 'darwin))
(setq *is-win* (eq system-type 'windows-nt))
(setq *is-cygwin* (eq system-type 'cygwin) )
(setq *is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

(add-to-list 'load-path "~/.emacs.d/custom/")

(require 'setup-config)
(require 'setup-package)
(require 'setup-environment)
(require 'setup-window)
(require 'setup-edit)
(require 'setup-company)
(require 'setup-org)
;;(require 'setup-programming)

(setq gc-cons-threshold 4000000)


