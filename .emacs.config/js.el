(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;; tern ----------------------------
;; (add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (setq tern-command (append tern-command '("--no-port-file")))


;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
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
;; (defun my-js2-indent-function ()
        ;;   (interactive)
        ;;   (save-restriction
                ;;     (widen)
                ;;     (let* ((inhibit-point-motion-hooks t)
                        ;;            (parse-status (save-excursion (syntax-ppss (point-at-bol))))
                        ;;            (offset (- (current-column) (current-indentation)))
                        ;;            (indentation (espresso--proper-indentation parse-status))
                        ;;            node)

                        ;;       (save-excursion

                                ;;         ;; I like to indent case and labels to half of the tab width
                                ;;         (back-to-indentation)
                                ;;         (if (looking-at "case\\s-")
                                        ;;             (setq indentation (+ indentation (/ espresso-indent-level 2))))

                                ;;         ;; consecutive declarations in a var statement are nice if
                                ;;         ;; properly aligned, i.e:
                                ;;         ;;
                                ;;         ;; var foo = "bar",
                                ;;         ;;     bar = "foo";
                                ;;         (setq node (js2-node-at-point))
                                ;;         (when (and node
                                        ;;                    (= js2-NAME (js2-node-type node))
                                        ;;                    (= js2-VAR (js2-node-type (js2-node-parent node))))
                                        ;;           (setq indentation (+ 4 indentation))))

                        ;;       (indent-line-to indentation)
                        ;;       (when (> offset 0) (forward-char offset)))))


(eval-after-load 'tern
   '(progn
	   (require 'tern-auto-complete)
      (tern-ac-setup)))
