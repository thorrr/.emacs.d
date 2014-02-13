(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-M-RET-may-split-line nil)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(org-babel-do-load-languages
 'org-babel-load-languages
  '( (sh . t)
     (python . t)
     (emacs-lisp . t)   
   ))

;; you have to use org-defkey to override conflicting org mode keybindings
(org-defkey org-mode-map (kbd "M-h") 'my-iflipb-next-buffer)


