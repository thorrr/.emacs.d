(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

(require 'flymake-phpcs)
(add-hook 'php-mode-hook 'flymake-phpcs-load)

(php-set-style "psr2")
(require 'phpcbf)
(setq phpcbf-standard "PSR2")
