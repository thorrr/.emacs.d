(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook #'paredit-mode)

;; this version roughly tracks "master"
(add-to-list 'package-pinned-packages '(cider . "melpa") 't)
