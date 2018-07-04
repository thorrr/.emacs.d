(require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) ;; replace with rjsx-mode?

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(add-hook 'js2-mode-hook (lambda ()
  ;; this makes js2-mode work with the indent-region from rjsx-mode
  (define-key js-mode-map (kbd "M-q") 'indent-region)

  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  ;; buffer-local override for C-right in smartparens-mode-map
  (let ((oldmap (cdr (assoc 'smartparens-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(smartparens-mode . ,newmap) minor-mode-overriding-map-alist))
))

(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


(require 'speedbar)
(speedbar-add-supported-extension ".js")
(add-to-list 'speedbar-fetch-etags-parse-list
             '("\\.js" . speedbar-parse-c-or-c++tag))


;; ac-js2 seems to work decently well
(require 'ac-js2)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'auto-complete-mode)
;; ac-js2-jump-to-definition is broken
(eval-after-load "ac-js2" (lambda ()
  (define-key ac-js2-mode-map (kbd "M-.") 'js2-jump-to-definition)
  (define-key ac-js2-mode-map (kbd "M-,") 'xref-pop-marker-stack)))

(setq ac-js2-evaluate-calls 't)

;; refactor prefix
(js2r-add-keybindings-with-prefix "C-=")

;; pug templates are awesome
(require 'pug-mode)
(setq pug-tab-width 2)
