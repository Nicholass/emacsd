;;; emacs config for golang
(require 'go-mode)
(setenv "GOPATH" "~/opt/src/go")

(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "~/opt/src/go")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
