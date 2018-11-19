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
      browse-url-generic-program "firefox %u")

;; Start Emacs server. Require Midnight
(unless (system-is-windows)
    (require 'server)
    (unless (server-running-p)
        (server-start)))
;;(require 'midnight)

;; save session
(desktop-save-mode 1)
(setq savehist-additional-variables
      '(kill-ring
        global-mark-ring
        search-ring
        regexp-search-ring
        file-name-history
        shell-command-history
        set-variable-value-history
        regexp-history
        compile-history
        w3m-input-url-history
        pyvenv-workon-history
        ))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Use CL
(require 'cl)
(require 'ag)

;; User name and e-mail
(setq user-full-name   "Mykola Korobko")
(setq user-mail-adress "korobko.nikolay@gmail.com")

;; Package manager:
;; Initialise package and add Melpa repository
(require 'package)
(package-initialize)


(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
(setq dired-listing-switches "-agG")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
(eval-after-load 'image-dired+ '(image-diredx-adjust-mode 1))
(define-key image-dired-thumbnail-mode-map "\C-n" 'image-diredx-next-line)
(define-key image-dired-thumbnail-mode-map "\C-p" 'image-diredx-previous-line)
(define-key image-dired-thumbnail-mode-map "g" 'revert-buffer)
(define-key image-dired-thumbnail-mode-map "x" 'image-diredx-flagged-delete)
(setq image-dired-track-movement nil)
(eval-after-load 'image '(require 'image+))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1dd7b369ab51f00e91b6a990634017916e7bdeb64002b4dda0d7a618785725ac" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "acd48beaecc038c0c990e8ac11a4a80e72f6b57a3c43f4b97d8f69ade64ff294" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(js-indent-level 2)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(magit-branch-read-upstream-first t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-directory "/home/nicholass/Dropbox/org")
 '(package-selected-packages
   (quote
    (org-cliplink orgnav org-download steam autopair  emmet-mode helm-emmet js2-refactor rainbow-delimiters xref-js2 toml-mode eimp ada-ref-man ox-hugo ada-mode geiser ace-window 0blayout helm-ag-r golden-ratio helm-cider cider clojure-mode clojure-snippets haskell-mode haskell-snippets yaml-mode web-mode underwater-theme twilight-theme sunrise-x-tree sunrise-x-mirror sunrise-x-loop solarized-theme smex scss-mode restclient-helm react-snippets pyvenv python-mode pug-mode powerline php-mode paredit ox-mediawiki ox-gfm org-present oauth2 nvm neotree multi-term move-line mode-icons mo-git-blame mmm-mode minimap material-theme markdown-preview-eww markdown-mode magit-gh-pulls lua-mode jsx-mode json-mode js-comint ir-black-theme ipython import-js image-dired+ image+ htmlize highlight-indentation highlight helm-swoop helm-projectile helm-proc helm-etags-plus helm-company helm-ag go-stacktracer go-scratch go-projectile go-dlv go-autocomplete find-file-in-project expand-region exec-path-from-shell es-windows es-lib engine-mode emojify emamux ein editorconfig drag-stuff company-web color-theme-solarized color-theme-sanityinc-tomorrow auto-indent-mode ansible alert ag ack ac-etags)))
 '(safe-local-variable-values (quote ((org-hugo-auto-export-on-save . t))))
 '(scheme-program-name "mit-scheme")
 '(web-mode-code-indent-offset 2))

;; ("org-notes"
;;  :base-directory "~/Dropbox/org/"
;;  :base-extension "org"
;;  :publishing-directory "~/Dropbox/Public/"
;;  :recursive t
;;  :publishing-function org-html-publish-to-html
;;  :headline-levels 4             ; Just the default for this project.
;;  :auto-preamble t
;;  )

;; Configure GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(horizontal-scroll-bar-mode -1)
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
(setq scroll-preserve-screen-position t)
(setq switch-to-buffer-preserve-window-point t)

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


(require 'autopair)
(autopair-global-mode)

(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-delay 0)
(setq show-paren-style 'mixed) ; highlight entire bracket expression

;; File saving options
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 't)

;; Linum plug-in
(require 'linum)
(line-number-mode   t)
(global-linum-mode  -1)
(column-number-mode t)
(setq linum-format " %6d")

(global-visual-line-mode 1) ; корректный автоперенос

;; ;; Move lines
;; (require 'move-lines)
;; (move-lines-binding)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-x t t") 'multi-term)

;; load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'material t)
;(load-theme 'afternoon t)

;;(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; git ------------------------------
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/git/")
;(require 'git)
;(require 'git-blame)
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



(require 'company)
(setq company-dabbrev-downcase nil)
;(add-hook 'after-init-hook 'global-company-mode)

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
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop-projectile)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
(setq help-swoop-pattern "")
(setq helm-swoop-use-line-number-face t)

;; search in buffer
(global-set-key (kbd "C-c h o") 'helm-occur)

;; C-c h i to use semantic mode over buffer
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(helm-projectile-on)
(global-set-key (kbd "C-c p") 'projectile-command-map)

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no -o ConnectTimeout=1")
(require 'tramp)
(helm-mode 1)
(helm-autoresize-mode 1)

;; YAS
(require 'yasnippet)
(yas-global-mode 1)

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (yas-next-field))))
          (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

;; (global-set-key [tab] 'tab-indent-or-complete)
;; (global-set-key (kbd "TAB") 'tab-indent-or-complete)
;; (global-set-key [(control return)] 'company-complete-common)

;; (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
;; (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

;; (define-key yas-minor-mode-map [tab] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

;; (define-key yas-keymap [tab] 'tab-complete-or-next-field)
;; (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
;; (define-key yas-keymap [(control tab)] 'yas-next-field)
;; (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)



(add-hook 'term-mode-hook (lambda()
            (yas-minor-mode -1)))


;; to move text
(require 'drag-stuff)
(drag-stuff-global-mode 1)
;; Got from here - http://seancribbs.com/emacs.d/#sec-2-4

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;;from enberg on #emacs auto close compilation buffer if OK
(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

(load-file "~/.emacs.config/go.el")
(load-file "~/.emacs.config/js.el")
(load-file "~/.emacs.config/py.el")
(load-file "~/.emacs.config/org.el")

;; emacs ends here

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Input method and key binding configuration.
(setq alternative-input-methods
      '(("russian-computer" . [?\C-\\])
        ("ukrainian-computer" . [?\C-|])
;        ("german-postfix"   . [?\C-\M-|])
))

(setq default-input-method
      (caar alternative-input-methods))

(defun toggle-alternative-input-method (method &optional arg interactive)
  (if arg
      (toggle-input-method arg interactive)
    (let ((previous-input-method current-input-method))
      (when current-input-method
        (deactivate-input-method))
      (unless (and previous-input-method
                   (string= previous-input-method method))
        (activate-input-method method)))))

(defun reload-alternative-input-methods ()
  (dolist (config alternative-input-methods)
    (let ((method (car config)))
      (global-set-key (cdr config)
                      `(lambda (&optional arg interactive)
                         ,(concat "Behaves similar to `toggle-input-method', but uses \""
                                  method "\" instead of `default-input-method'")
                         (interactive "P\np")
                         (toggle-alternative-input-method ,method arg interactive))))))

(reload-alternative-input-methods)

;; Network stuff
(setq ping-program-options '("-v"))
(setq gnus-select-method
      '(nnimap "bootless.info"))
(setq gnus-read-active-file nil)
