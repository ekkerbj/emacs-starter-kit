
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
(define-key haml-mode-map [(control meta left)] 'haml-up-list)
(define-key haml-mode-map [(control meta right)] 'haml-down-list)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; Revert annoying indent behavior of DEL.
(add-hook 'haml-mode-hook (lambda () (define-key haml-mode-map (kbd "<backspace>") 'delete-backward-char)))
(add-hook 'sass-mode-hook (lambda () (define-key sass-mode-map (kbd "<backspace>") 'delete-backward-char)))

(provide 'topfunky/haml)
