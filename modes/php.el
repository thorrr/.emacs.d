(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

(require 'flymake-phpcs)
(add-hook 'php-mode-hook 'flymake-phpcs-load)

(require 'phpcbf)
(setq phpcbf-standard "PSR2")
