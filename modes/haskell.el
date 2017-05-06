(setq _agda-mode-tried nil)
(add-to-list 'auto-mode-alist '("\\.agda\\'" . (lambda ()
   ;; agda (and some haskell) stuff doesn't display correctly without unicode-fonts to
   ;; dynamically pick correct fonts for "double-struck capital N', etc.
   (require 'unicode-fonts)
   (unicode-fonts-setup)
   (if (not _agda-mode-tried) (progn
     ;;load agda-mode from the installation if it's there
     (setq _agda-mode-tried 't)
     (with-demoted-errors
       ;; load agda2-mode if it's on our path
       (load-file (let ((coding-system-for-read 'utf-8))
                    (shell-command-to-string "agda-mode locate")))
       (require 'agda2-mode)
       (agda2-mode))
     )))))

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

;; kind of like paredit mode
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode) ;; first do 'stack install structured-haskell-mode'

;; (add-hook 'haskell-mode-hook 'intero-mode)  ;; uses ghc under the hood

(require 'ghc)  ;; before this you must do 'stack install ghc-mod --no-system-ghc'
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(if (eq system-type 'windows-nt) (progn ;; add ghc-mod and ghc-modi to path
  (setq exec-path (append exec-path (list (concat (getenv "APPDATA") "\\local\\bin"))))))
