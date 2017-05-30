;; Org-mode
(require 'org)
(require 'helm-org)

(add-to-list 'auto-mode-alist '("\\.org\\'". org-mode))

(setq org-directory "/home/nicholass/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/organizer.org"))
(set-register ?o (cons 'file org-default-notes-file))

(setq org-agenda-files (list org-directory))
      ;; (mapcar (lambda (x)  (concat org-directory x))
      ;;         (list "work"
      ;;               "disser"
      ;;               "digg"
      ;;               "digg")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key  "\C-cc" 'org-capture)
(global-set-key  "\C-co" 'helm-org-in-buffer-headings)

(setq org-tags-column (- 4 (window-width)))
;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags)
  )

;; configure org-capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "TODO")
         "* TODO %?\n  %i\n  "  :prepend t)
        ("m" "Music" entry (file+headline (concat org-directory "/music.org") "Ideas")
             "*  %?\n Идея %U  | %^g \n " :empty-lines 1)
        ("d" "Digg" entry (file (concat org-directory "/digg.org"))
         "*  PLANNED %?  %^g \n ")
         ("s" "Servers" entry (file (concat org-directory "/servplan.org") "Задачи")
          "*  TODO | %U | %^g \n %? \n" :empty-lines 1)
         ))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t))
 )


(defun nicholass-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'nicholass-fix-inline-images)

(defun nicholass-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'nicholass-org-confirm-babel-evaluate)
