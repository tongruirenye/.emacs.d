;;;
;;; Work
;;;


(require 'dired)
(defun cc-dired-open-config-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "et.exe" nil 0 nil file)))

(defun cc-open-work-config-dir ()
  "In dired, open the file named on this line."
  (interactive)
  (dired "E:/XZN2/trunk/schedule/config/"))

(defun cc-open-work-protocol-dir ()
  "In dired, open the file named on this line."
  (interactive)
  (dired "E:/XZN2/trunk/soft/common/protocol/"))

(defun cc-open-work-doc-dir ()
  "In dired, open the file named on this line."
  (interactive)
  (dired "E:/XZN2/trunk/soft/server/doc/"))

(defun cc-open-work-ucenter-dir ()
  "In dired, open the file named on this line."
  (interactive)
  (dired "E:/XZN2/trunk/soft/server/ucenter/api/"))


(defun cc-insert-cal()
  (interactive)
  (let ((h (org-heading-components)))
    (org-set-property "SUMMARY" (nth 4 h))
    (org-set-property "DESCRIPTION" "")
    (org-id-get-create)
    (org-set-property "VCREATE" "")
    (org-set-property "VSTART" "")
    (org-set-property "VEND" "")))



(define-key dired-mode-map (kbd "C-c p") 'cc-dired-open-config-file)
(global-set-key (kbd "C-c w c") 'cc-open-work-config-dir)
(global-set-key (kbd "C-c w p") 'cc-open-work-protocol-dir)
(global-set-key (kbd "C-c w d") 'cc-open-work-doc-dir)
(global-set-key (kbd "C-c w u") 'cc-open-work-ucenter-dir)




(provide 'setup-work)
