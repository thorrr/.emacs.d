(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-M-RET-may-split-line nil)
(setq org-src-fontify-natively 't)
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;misc setup
(add-hook 'org-mode-hook (lambda ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (sh . t)
      (python . t)
      (emacs-lisp . t)
      (C . t)
      ))
  ;; you have to use org-defkey to override conflicting org mode keybindings
  (org-defkey org-mode-map (kbd "M-h") 'my-iflipb-next-buffer)
  ;; new ob-ipython stuff - need to require org-mode first
  (require 'ob-ipython)
))



