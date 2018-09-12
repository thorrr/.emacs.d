(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

(require 'flymake-phpcs)
(add-hook 'php-mode-hook 'flymake-phpcs-load)

(add-hook 'php-mode-hook (lambda () (php-set-style "psr2")))
(require 'phpcbf)
(setq phpcbf-standard "PSR2")
