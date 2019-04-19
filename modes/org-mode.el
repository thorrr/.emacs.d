(use-package org
  :commands org-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  :custom
  (org-M-RET-may-split-line nil)
  (org-src-fontify-natively 't)
  (org-confirm-babel-evaluate nil)

  :config
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  ;;misc setup
  (add-hook 'org-mode-hook (lambda ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '( (sh . t)
        (python . t)
        (emacs-lisp . t)
        (C . t)))
  ;; you have to use org-defkey to override conflicting org mode keybindings
  (org-defkey org-mode-map (kbd "M-h") 'my-iflipb-next-buffer))))

;; new ob-ipython stuff - need to require org-mode first
(use-package ob-ipython)
