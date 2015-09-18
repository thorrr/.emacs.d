;; agda (and some haskell) stuff doesn't display correctly without unicode-fonts to dynamically pick correct
;; fonts for "double-struck capital N', etc.
(add-hook 'haskell-mode-hook (lambda ()
  (require 'unicode-fonts)
  (unicode-fonts-setup)))

;; load agda2-mode if it's on our path
(ignore-errors
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

