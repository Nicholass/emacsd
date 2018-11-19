(require 'js2-mode)
(require 'js2-refactor)

(require 'emmet-mode)

;; (add-hook 'js2-mode-hook (lambda ()
;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))
;;        (tern-mode)
        (company-mode)
        (emmet-mode)
        (rainbow-delimiters-mode)))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))

(setq emmet-expand-jsx-className? t) ;; for React
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-element-highlight t)

(setq scss-indent-offset 2)
(setq css-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; TODO:
;;(require 'company)
;(require 'company-tern)
;; (add-to-list 'company-backends 'company-tern)


;; Disable completion keybindings, as we use xref-js2 instead
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

;; (js2r-add-keybindings-with-prefix "C-c C-r")
;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
;; (define-key js-mode-map (kbd "M-.") nil)
