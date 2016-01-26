;; ~/.emacs
;; this is free software
(defun system-is-linux ()
    "Linux system checking."
    (interactive)
    (string-equal system-type "gnu/linux"))

(defun system-is-mac ()
    "Mac OS X system checking."
    (interactive)
    (string-equal system-type "darwin"))

(defun system-is-windows ()
    "MS Windows system checking."
    (interactive)
    (string-equal system-type "windows-nt"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Start Emacs server. Require Midnight
(unless (system-is-windows)
    (require 'server)
    (unless (server-running-p)
        (server-start)))
;;(require 'midnight)

;; Use CL
(require 'cl)
(require 'ag)

;; User name and e-mail
(setq user-full-name   "Nicholay Korobko")
(setq user-mail-adress "korobko.nikolay@gmail.com")

;; Package manager:
;; Initialise package and add Melpa repository
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" magit )))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Package list
(defvar muscimol/required-packages
  '(;graphviz-dot-mode
    ;haskell-mode
    ;inf-ruby
    js2-mode
    ;lua-mode
    ;markdown-mode
    ;mmm-mode
    ;php-mode
    ;protobuf-mode
    ;python-mode
    ;rspec-mode
    ;ruby-end
    ;wc-mode
    ;web-mode
    ;yaml-mode

    ;flymake
    ;flymake-easy
    ;flymake-lua
    ;flymake-ruby
    ;flymake-haskell-multi

    ;org-present

    ack
    auto-complete
    ;dash-at-point
    expand-region
    highlight
    ;hlinum
    ;ido-ubiquitous
    magit
    minimap
    neotree
    paredit
    smex
    htmlize

    ir-black-theme
    solarized-theme
    twilight-theme
    underwater-theme
   )
)
;; autoinstall pkgs
(dolist (p muscimol/required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t)
(setq imenu-use-popup-menu nil)
(global-set-key (kbd "C-x TAB") 'imenu)

;; Some i-stuff
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ido)
(ido-mode t)

;; Display the name of the current buffer in the title
(setq frame-title-format "%b - emacs")

;; Org-mode
(require 'org)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "acd48beaecc038c0c990e8ac11a4a80e72f6b57a3c43f4b97d8f69ade64ff294" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-enter-indents-newline t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)

 '(org-directory "/home/nicholass/.org"))

;; ("org-notes"
;;  :base-directory "~/Dropbox/org/"
;;  :base-extension "org"
;;  :publishing-directory "~/Dropbox/Public/"
;;  :recursive t
;;  :publishing-function org-html-publish-to-html
;;  :headline-levels 4             ; Just the default for this project.
;;  :auto-preamble t
;;  )
(add-to-list 'auto-mode-alist '("\\.org\\'". org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-tags-column 100)

;; Configure GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode -1)
(setq use-dialog-box     nil)
(setq redisplay-dont-pause t)
(setq pop-up-frames nil)
(setq ring-bell-function 'ignore)
(setq visible-bell t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(global-highlight-changes-mode t)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#382f2f")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


(defun highlight-changes-remove-after-save ()
  "Remove previous changes after save."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
		(highlight-changes-remove-highlight (point-min) (point-max)))))

(add-hook 'after-save-hook 'highlight-changes-remove-after-save)

;; tabbar
;(require 'tabbar)
;    (tabbar-mode t)
;(global-set-key [M-left] 'tabbar-backward-tab)
;(global-set-key [M-right] 'tabbar-forward-tab)

;; Tabbar settings
;(setq tabbar-background-color "#959A79") ;; the color of the tabbar background
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t :inherit shadow)))
 '(neo-dir-link-face ((t :inherit dired-directory)))
 '(neo-expand-btn-face ((t :inherit button)))
 '(neo-file-link-face ((t :inherit default)))
 '(neo-header-face ((t :inherit shadow)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil))))

;; Fringe
(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Window size. Set font
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 30))
(when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono-11" nil t))

;; Scrolling
(setq scroll-step               1)
(setq scroll-margin            5)
(setq scroll-conservatively 10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global clipboard
(setq x-select-enable-clipboard t)
(global-set-key (kbd "C-M-y") 'x-clipboard-yank)
(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing

;; Turn off auto indent
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)
(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-delay 0)
(setq show-paren-style 'expression) ; highlight entire bracket expression

;; File saving options
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 't)

;; Linum plug-in
(require 'linum)
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d")
(global-visual-line-mode 1) ; корректный автоперенос

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")


;; load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'zenburn t)
(load-theme 'afternoon t)

;;(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;; tern ----------------------------
;; (add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (setq tern-command (append tern-command '("--no-port-file")))


;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

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

;; git ------------------------------
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/git/")
;(require 'git)
(require 'git-blame)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Uset functions
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(define-key help-map "\M-f" 'show-file-name)
(setq make-backup-files nil)
(setq auto-save-default nil) ; stop creating #autosave# files


;; PM stuff -------------------------
;; Helm framework
(require 'helm-config)

;; projctile to file management in projects
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'default)
(setq projectile-indexing-method 'git)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme 'nerd)


;; Got from here - http://seancribbs.com/emacs.d/#sec-2-4


(setq neo-vc-integration '(face char))

    ;; Patch to fix vc integration
    (defun neo-vc-for-node (node)
      (let* ((backend (vc-backend node))
             (vc-state (when backend (vc-state node backend))))
        ;; (message "%s %s %s" node backend vc-state)
        (cons (cdr (assoc vc-state neo-vc-state-char-alist))
              (cl-case vc-state
                (up-to-date       neo-vc-up-to-date-face)
                (edited           neo-vc-edited-face)
                (needs-update     neo-vc-needs-update-face)
                (needs-merge      neo-vc-needs-merge-face)
                (unlocked-changes neo-vc-unlocked-changes-face)
                (added            neo-vc-added-face)
                (removed          neo-vc-removed-face)
                (conflict         neo-vc-conflict-face)
                (missing          neo-vc-missing-face)
                (ignored          neo-vc-ignored-face)
                (unregistered     neo-vc-unregistered-face)
                (user             neo-vc-user-face)
                (t                neo-vc-default-face)))))

;; emacs ends here

(put 'dired-find-alternate-file 'disabled nil)