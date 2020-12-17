;;
;; Customization
;;

(defgroup cc nil
  "CC emacs customization"
  :group 'convenience)


(defcustom cc-org-dir "d:/mydata/org/"
  "CC org dir"
  :group 'cc
  :type 'string)

(defcustom cc-windows-toast-file "C:/Users/chenchao/AppData/Roaming/.emacs.d/misc/notify.ps1"
  "CC toast script for org-pomodoro on windows10"
  :group 'cc
  :type 'string)

(defcustom cc-plantuml-jar-file "C:/Users/chenchao/AppData/Roaming/.emacs.d/misc/plantuml.jar"
  "cc plantuml jar file"
  :group 'cc
  :type 'string)

(defcustom cc-project-dir (concat cc-org-dir "roam/project/")
  "CC project dir"
  :group 'cc
  :type 'string)

(defcustom cc-note-dir (concat cc-org-dir "note/")
  "CC note dir"
  :group 'cc
  :type 'string)

(defcustom cc-read-file (concat cc-project-dir "personal.org")
  "CC journal dir"
  :group 'cc
  :type 'string)

(defcustom cc-roam-dir (concat cc-org-dir "roam/")
  "CC roam dir"
  :group 'cc
  :type 'string)

(defcustom cc-package-archives-list
  `(,(cons 'melpa `(,(cons "gnu" "https://elpa.gnu.org/packages/")
                    ,(cons "melpa" "https://melpa.org/packages/")))
    ,(cons 'tsinghua `(,(cons "gnu" "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                       ,(cons "melpa" "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
  "CC package archives list"
  :group 'cc
  :type '(alist :key-type (symbol :tag "Archive source")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "Archive url"))))

(defcustom cc-package-archives 'tsinghua
  "CC package archive"
  :group 'cc
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives (alist-get value cc-package-archives-list)))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (list 'const :tag (symbol-name (car item)) (car item)))
                    cc-package-archives-list)))

(defcustom cc-theme 'doom-one-light
  "CC theme"
  :group 'cc
  :type '(choice
          (const :tag "Doom One" doom-one)
          (const :tag "Doom One light" doom-one-light)))

;;;###autoload
(defun cc-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively
     (cond ((featurep 'ivy) #'counsel-find-file)
           (#'find-file)))))

;;;###autoload
(defun cc-open-user-emacs-directory ()
  (interactive)
  (unless (file-directory-p user-emacs-directory)
    (message "no user emacs directory at :%s" user-emacs-directory))
  (cc-project-browse user-emacs-directory))


(provide 'setup-config)


