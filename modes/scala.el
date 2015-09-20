(add-to-list 'load-path (concat (getenv "SCALA_HOME") "/misc/scala-tool-support/emacs"))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(defun scala-turnoff-indent-tabs-mode () (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)
(defun bind-autocomplete () (local-set-key "\M-'" 'auto-complete))


;; Ensime
(setq ensime-sbt-program-name "sbtem.bat")
(add-to-list 'load-path (concat shared-externals "/ensime/src/main/elisp/"))
(add-hook 'scala-mode-hook (lambda ()
  (require 'ensime)))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ensime-mode-hook 'bind-autocomplete)

