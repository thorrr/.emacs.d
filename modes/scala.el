(use-package scala-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

  :config
  (defun scala-turnoff-indent-tabs-mode () (setq indent-tabs-mode nil))
  (add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode))



