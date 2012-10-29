(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-default-notes-file "m:/org/notes.org")
(setq org-agenda-files (list "m:/org/projects.org" "m:/org/notes.org" "m:/org/main.org"))
;;(define-key global-map "\C-cc" 'org-capture)