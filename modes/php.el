(use-package php-mode
  :disabled
  :config
  (php-set-style "psr2")
)
(use-package flymake-php
  :init
  (add-hook 'php-mode-hook 'flymake-php-load))

(use-package flymake-phpcs
  :init
  (add-hook 'php-mode-hook 'flymake-phpcs-load))

(use-package phpcbf
  :custom
  (phpcbf-standard "PSR2"))

(use-package php-auto-yasnippets
  :after yasnippet)
