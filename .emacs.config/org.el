;; Org-mode
(require 'subr-x)
(require 'org)
(require 'helm-org)
(require 'ox-latex)
(require 'org-cliplink)
(setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))

(defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location "Headline" nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line))

(add-to-list 'auto-mode-alist '("\\.org\\'". org-mode))

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/organizer.org")
(set-register ?o (cons 'file org-default-notes-file))
;; configure org-capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+function "~/org/todo.org" org-ask-location)
         "* TODO %?\n  %i\n  "
         :prepend t)
        ("m" "Music" entry (file+function '"~/org/music.org" org-ask-location)
         "*  %?\n Идея %U  | %^g \n "
         :empty-lines 1)
        ("b" "Bootless.info" entry (file+function "~/booless/bootless.info.org" org-ask-location)
         "* %? \n"
         :empty-lines 1)
        ("d" "Digg" entry (file+function "~/org/digg.org" org-ask-location)
         "* PLANED %? \n "
        ("s" "Servers" entry (file+function "~/org/devops.org" org-ask-location)
         "*  TODO | %U | %^g \n %? \n"
         :empty-lines 1)
        )))



(setq org-agenda-files (list org-directory))
      ;; (mapcar (lambda (x)  (concat org-directory x))
      ;;         (list "work"
      ;;               "disser"
      ;;               "digg"
      ;;               "digg")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ci" 'org-cliplink)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cv" 'org-capture-goto-last-stored)
(global-set-key "\C-co" 'helm-org-in-buffer-headings)


;; Place tags close to the right-hand side of the window
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 70 (window-width)))
  (org-agenda-align-tags))
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)

(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output."
  (when (and (string=
              "python"
              (org-element-property :language (org-element-at-point)))
             (string-match
              ":session"
              (org-element-property :parameters (org-element-at-point))))

    (save-excursion
      (when (org-babel-where-is-src-block-result)
        (goto-char (org-babel-where-is-src-block-result))
        (end-of-line 1)
        ;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
        (while (re-search-forward
                "\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\|In \\[\\d+\\]:)"
                (org-element-property :end (org-element-at-point))
                t)
          (replace-match "")
          ;; this enables us to get rid of blank lines and blank : >>>
          (beginning-of-line)
          (when (looking-at "^$")
            (kill-line)))))))

(add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(setq geiser-active-implementations '(mit))
(custom-set-variables
 '(scheme-program-name "mit-scheme"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t)
   (python .t)
   (shell . t)
   (scheme . t)))

(setq org-babel-python-command "ipython3 --simple-prompt")

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("C" "clojure" "sh" "python" "scheme"))))

(setq org-confirm-babel-evaluate nil)


(setq org-display-inline-images t)
(defun nicholass-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'nicholass-fix-inline-images)

(defun nicholass-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate nil ) ;'nicholass-org-confirm-babel-evaluate)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(require 'ox-hugo-auto-export)
;; List of additional LaTeX packages
;; (add-to-list 'org-export-latex-packages-alist '("" "cmap" t))
;; (add-to-list 'org-export-latex-packages-alist '("english,russian,ukrainian" "babel" t))
;;(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
