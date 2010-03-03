(defun apache-jakarta-mode ()
  "The Java mode specialization for Apache Jakarta projects."
  (if (not (assoc "apache-jakarta" c-style-alist))
      ;; Define the Apache Jakarta cc-mode style.
      (c-add-style "apache-jakarta" '("java" (indent-tabs-mode . nil))))

  (c-set-style "apache-jakarta")
  (c-set-offset 'substatement-open 0 nil)
  (setq mode-name "Apache Jakarta")
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (message "newline-indent function executed"))

(add-hook 'java-mode-hook 'apache-jakarta-mode)

(load-library "junit")
