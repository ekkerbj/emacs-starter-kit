
;; Load shared code
(load-file (concat dotfiles-dir "shared.el"))

;; My KAZ Section!!!
(defun kaz-kill-all-buffers ()
  "Kill all buffers and deletes other windows"
  (interactive)
  (mapcar (function kill-buffer) (buffer-list))
  (delete-other-windows))

(defvar kaz-dictionary-url "http://dictionary.reference.com/search?q=%s"
   "URL used to lookup words.  `%s' is replaced with the word queried")
   (defun kaz-lookup-word ()
   "Look up word at point in the dictionary specified by the 
 `kaz-dictionary-url' variable"
   (interactive)
   (save-excursion
     (let ((url (format kaz-dictionary-url (car (ispell-get-word nil)))))
      (browse-url url))))
(autoload 'ispell-get-word  "ispell" "Get word at point using ispell"   t)
(iswitchb-mode 1)
(setq read-buffer-function
          'iswitchb-read-buffer)

;; VMS Section
;; Modified by dtkim@calab.kaist.ac.kr
;; Because some VMS ftp servers convert filenames to lower case
;; we allow a-z in the filename regexp. I'm not too happy about this.
(setq ange-ftp-vms-filename-regexp "\\([^ ]+ -> [^ ]+\\|[^ ]+\\)$")
(setq ange-ftp-vms-host-regexp "^qdss21.qg.com$")
(setq ange-ftp-vms-host-regexp "^qdss20.qg.com$")


;; Major Modes

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
	  '(lambda ()
	     (require 'groovy-electric)
	     (groovy-electric-mode)))

;; Velocity
(autoload 'vtl-mode "vtl" "Velocity editing mode." t)
(add-hook 'html-mode-hook 'vtl-mode t t)
(add-hook 'xml-mode-hook 'vtl-mode t t)
(add-hook 'text-mode-hook 'vtl-mode t t)
(add-to-list 'auto-mode-alist '("\.vm$" . vtl-mode))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/vtl.el"))
(require 'vtl)

;; Maven Section
(require 'compile)
(setq compile-command "mvn clean install")
(add-to-list
  'compilation-error-regexp-alist-alist
   '(mvn "^\\[ERROR\\] \\(/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist 'mvn)
(add-to-list
  'compilation-error-regexp-alist-alist
   '(mvn-warning "^\\[WARNING\\] \\(/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 1 1))
(add-to-list 'compilation-error-regexp-alist 'mvn-warning)

;; Java Section
(require 'java-mode-indent-annotations)
(defun apache-jakarta-mode ()
  "The Java mode specialization for Apache Jakarta projects."
  (if (not (assoc "apache-jakarta" c-style-alist))
      ;; Define the Apache Jakarta cc-mode style.
      (c-add-style "apache-jakarta" '("java" (indent-tabs-mode . nil))))

  (c-set-style "apache-jakarta")
  (java-mode-indent-annotations-setup)
  (c-set-offset 'substatement-open 0 nil)
  (setq mode-name "Apache Jakarta")
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (message "newline-indent function executed"))

(add-hook 'java-mode-hook 'apache-jakarta-mode)
(load-library "junit")

(load-file (concat dotfiles-dir "/vendor/epresent.el"))

;; save the session on exit
(desktop-save-mode 0)
