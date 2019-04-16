(use-package unicode-fonts
  :disabled
  :config
  (unicode-fonts-setup)
)

(use-package agda2-mode
  :disabled
  :init
  (setq _agda-mode-tried nil)
  (add-to-list 'auto-mode-alist '("\\.agda\\'" . (lambda ()
   ;; agda (and some haskell) stuff doesn't display correctly without unicode-fonts to
   ;; dynamically pick correct fonts for "double-struck capital N', etc.
    (if (not _agda-mode-tried) (progn
      ;;load agda-mode from the installation if it's there
      (setq _agda-mode-tried 't)
      (with-demoted-errors
          ;; load agda2-mode if it's on our path
          (load-file (let ((coding-system-for-read 'utf-8))
                       (shell-command-to-string "agda-mode locate")))
        (require 'agda2-mode)
        (agda2-mode))
      ))))))


(use-package haskell-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

;; kind of like paredit mode
(use-package shm
  :init
   ;; first do 'stack install structured-haskell-mode'
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

(use-package ghc
  :disabled
  :init
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  :config
  (ghc-debug)
  (when (eq system-type 'windows-nt) ;; add ghc-mod and ghc-modi to path
    (setq exec-path (append exec-path (list (concat (getenv "APPDATA") "\\local\\bin")))))

  :ensure-system-package
  ((ghc-mod . "stack install ghc-mod --no-system-ghc"))
)



