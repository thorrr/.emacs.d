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
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)


