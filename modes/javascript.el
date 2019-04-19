(use-package js2-mode
  :commands js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :bind (:map js2-mode-map
         ("C-k" . js2r-kill)
         ;; this makes js2-mode work with the indent-region from rjsx-mode
         ("M-q" . indent-region))
  :config
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  ;; buffer-local override for C-right in smartparens-mode-map
  (let ((oldmap (cdr (assoc 'smartparens-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(smartparens-mode . ,newmap) minor-mode-overriding-map-alist))
  ;; speedbar view customizations
  (require 'speedbar)
  (speedbar-add-supported-extension ".js")
  (add-to-list 'speedbar-fetch-etags-parse-list
               '("\\.js" . speedbar-parse-c-or-c++tag)))

(use-package js2-refactor
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :config
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(use-package rjsx-mode
  :commands rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

;; ac-js2 seems to work decently well
(use-package ac-js2
  :after (:all js2-mode auto-complete-mode)
  :hook (auto-complete-mode . ac-js2-mode)
  :custom
  (ac-js2-evaluate-calls 't)  
  :config
  ;; ac-js2-jump-to-definition is broken
  (define-key ac-js2-mode-map (kbd "M-.") 'js2-jump-to-definition)
  (define-key ac-js2-mode-map (kbd "M-,") 'xref-pop-marker-stack))

;; pug templates are awesome
(use-package pug-mode
  :custom
  (pug-tab-width 2))

