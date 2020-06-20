;;
;; UML
;;

(use-package plantuml-mode
  :ensure t
  :init
  ;; Sample jar configuration
  (setq org-plantuml-jar-path cc-plantuml-jar-file)
  (setq plantuml-default-exec-mode 'jar)

  ;; Sample executable configuration
  ;; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
  ;; (setq plantuml-default-exec-mode 'executable)

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))
