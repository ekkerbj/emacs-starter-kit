 ;; DESCRIPTION: topfunky settings
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/treetop-mode"))
(require 'treetop-mode)

;; Commands
(require 'unbound)

;; Minor Modes

;;mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

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

(require 'whitespace)

;; rspec
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rspec-mode"))
(require 'rspec-mode)

;; cucumber
(add-to-list 'load-path (concat dotfiles-dir "/vendor/cucumber.el"))
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))


;; Major Modes

;; Javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; Rinari
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
;; (require 'rinari)

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
(define-key haml-mode-map [(control meta left)] 'haml-up-list)
(define-key haml-mode-map [(control meta right)] 'haml-down-list)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.sake\\'" . ruby-mode))

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/groovy-mode.el"))
(require 'groovy-mode)

(autoload 'vtl-mode "vtl" "Velocity editing mode." t)
(add-hook 'html-mode-hook 'vtl-mode t t)
(add-hook 'xml-mode-hook 'vtl-mode t t)
(add-hook 'text-mode-hook 'vtl-mode t t)
(add-to-list 'auto-mode-alist '("\.vm$" . vtl-mode))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/vtl.el"))
(require 'vtl)


;; Font
;;(set-default-font "-apple-consolas-medium-r-normal--0-0-0-0-m-0-iso10646-1")
;;(set-default-font "-microsoft-Consolas-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)

(require 'line-num)

;; Full screen toggle
;;(defun toggle-fullscreen ()
;;  (interactive)
;; (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
;;                                          nil
;;                                        'fullboth)))
;;global-set-key (kbd "M-n") 'toggle-fullscreen)


;; Keyboard

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)

;; Some Mac-friendly key counterparts
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)

;; Keyboard Overrides

(define-key textile-mode-map (kbd "M-s") 'save-buffer)
(define-key text-mode-map (kbd "M-s") 'save-buffer)

(global-set-key [(meta up)] 'beginning-of-buffer)
(global-set-key [(meta down)] 'end-of-buffer)

(global-set-key [(meta shift right)] 'ido-switch-buffer)
(global-set-key [(meta shift up)] 'recentf-ido-find-file)
(global-set-key [(meta shift down)] 'ido-find-file)
(global-set-key [(meta shift left)] 'magit-status)

(global-set-key [(meta H)] 'delete-other-windows)

(global-set-key [(meta D)] 'backward-kill-word) ;; (meta d) is opposite

(global-set-key [(meta N)] 'cleanup-buffer)

(global-set-key [(control \])] 'indent-rigidly)

;; Other
(prefer-coding-system 'utf-8)

;; Maven Section
;;(require 'compile)
(setq compile-command "mvn clean install")
(add-to-list
  'compilation-error-regexp-alist-alist
   '(mvn "\\(^/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist 'mvn)
(add-to-list
  'compilation-error-regexp-alist-alist
   '(mvn-warning "^\\[WARNING\\] \\(/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 1 1))
(add-to-list 'compilation-error-regexp-alist 'mvn-warning)


;; save the session on exit
(desktop-save-mode 1)

;; Activate theme
;; (load-file "~/.emacs.d/vendor/color-theme-twilight.el")
(load-file "~/.emacs.d/vendor/color-theme-vibrant-ink.el")
(color-theme-vibrant-ink)
