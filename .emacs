;; ~/.emacs
;; this is free software
(prefer-coding-system 'utf-8)

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

;; save session
(desktop-save-mode 1)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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
(add-to-list 'package-archives
             '("SC"  . "http://joseito.republika.pl/sunrise-commander/") t)
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
   ;; minimap
   ;; neotree
    paredit
   ;; smex
    htmlize

    ;; ir-black-theme
    ;; solarized-theme
    ;; twilight-theme
    ;; underwater-theme
   )
)
;; autoinstall pkgs
(dolist (p muscimol/required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)
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
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "acd48beaecc038c0c990e8ac11a4a80e72f6b57a3c43f4b97d8f69ade64ff294" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(magit-branch-read-upstream-first t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-directory "/home/nicholass/Dropbox/org"))

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
(line-number-mode 1)
(column-number-mode 1)

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

;; Fringe
(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Window size. Set font-family-list
(set-default-font "DejaVu Sans Mono-10" nil t)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10" )
(set-frame-font "DejaVu Sans Mono-10" nil t)
(set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-10")
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 30))
;; (when (member "Roboto" (font-family-list))
;;     (set-frame-font "Roboto" nil t)) ;

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

;; Indent
;;(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;; (electric-indent-mode +1)
;; (setq-default electric-indent-inhibit t)
(require 'auto-indent-mode)
(auto-indent-global-mode)
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
(line-number-mode   -1)
(global-linum-mode  -1)
(column-number-mode t)
(setq linum-format " %d")
(global-visual-line-mode 1) ; корректный автоперенос

;; ;; Move lines
;; (require 'move-lines)
;; (move-lines-binding)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")


;; load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-dark t)
;(load-theme 'afternoon t)

;;(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

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

;; Term
(global-set-key (kbd "C-x t t") 'multi-term)

(require 'company)
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)

;; PM stuff -------------------------
;; Helm framework
(require 'helm)
(require 'helm-config)
(require 'helm-ag)
(require 'projectile)
(require 'helm-swoop)
(require 'helm-projectile)
(require 'helm-company)
(autoload 'helm-company "helm-company")
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-indexing-method 'git)

;; overset standard M-x with helm and turn fuzzymatch on
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

;; overset standard 'finde file' with helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Show kill-ring instead just cycling
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Overset C-x b
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; swoop
;; (global-set-key (kbd "C-c h s") 'helm-swoop)
;; (global-set-key (kbd "C-c h S") 'helm-multi-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;; search in buffer
(global-set-key (kbd "C-c h o") 'helm-occur)

;; C-c h i to use semantic mode over buffer
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(helm-projectile-on)

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no -o ConnectTimeout=1")
(require 'tramp)
(helm-mode 1)
(helm-autoresize-mode 1)

;; to move text
(require 'drag-stuff)
(drag-stuff-global-mode 1)
;; Got from here - http://seancribbs.com/emacs.d/#sec-2-4

(load-file "~/.emacs.d/go.el")
(load-file "~/.emacs.d/js.el")


;; emacs ends here

(put 'dired-find-alternate-file 'disabled nil)
