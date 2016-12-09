(require 'js2-mode)

;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
(require 'mmm-mode)

(mmm-add-classes
 '((jsx
    :submode web-mode
    :front "\\((\\)[[:space:]\n]*<"
    :front-match 1
    :back ">[[:space:]\n]*\\()\\)"
    :back-match 1)))

(setq mmm-global-mode 'maybe)

(mmm-add-mode-ext-class 'js2-mode "\\.jsx\\'" 'jsx)
