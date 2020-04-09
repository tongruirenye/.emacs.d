
;; stolen from ank-editor

(require 'ox)
(require 'request)

(defconst my-html-backend
  (org-export-create-backend
	   :parent 'html))

(defun my-get-content()
  (interactive)
  (let* ((field-heading (org-element-at-point))
	 (field-name (substring-no-properties
		      (org-element-property
		       :raw-value
		       field-heading)))
	 (content-begin (org-element-property :contents-begin field-heading))
	 (content-end (org-element-property :contents-end field-heading)))
    (request
     "http://127.0.0.1:8765"  ;;8756
     :type "POST"
     :data (json-encode `(("action" . "addNote")
			  ("version" . 6)
			  ("params" ("note"
				     ("deckName" . "Default")
				     ("modelName" . "Basic")
				     ("fields"
				      ("Front" . ,field-name)
				      ("Back" . ,(org-export-string-as
						 (buffer-substring
						  content-begin
						  content-end)
						 my-html-backend
						 t)))
				     ("options"
				      ("allowDuplicate" . t))
				     ("tags" . ["abc"])
				     ))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (message "I send: %S" data)))
     :error (cl-function
	     (lambda (&key _ &key error-thrown &allow-other-keys)
	       (message "I send error: %S" (string-trim (cdr error-thrown)))))
     :sync t)))

(provide 'setup-window)

;; (json-encode '(("action" . "addNode")
;; 	       ("params" . ("dddd" . ("abc")))))

;; (json-read-from-string "{\"action\": false,\"params\": {\"note\": {\"deck\": \"hello\", \"a\": \"b\", \"d\":[\"c\"]}}}")

;; (setq abc "dddd")

;; (json-encode `(("action" . "addNode")
;; 			  ("version" . 6)
;; 			  ("params" ("note"
;; 				     ("dectName" . "Default")
;; 				     ("modelName" . "Basic")
;; 				     ("fields"
;; 				      ("Front" . ,abc)
;; 				      ("Back" . "back"))
;; 				     ("options"
;; 				      ("allowDuplicate" . t))
;; 				     ("tags" . ["abc"])
;; 				     ))))


;; (org-export-string-as "1. abc"
;; 		      my-html-backend
;; 		      t)


	       
