;; agda (and some haskell) stuff doesn't display correctly without unicode-fonts to dynamically pick correct
;; fonts for "double-struck capital N', etc.
(require 'unicode-fonts)

;; load agda2-mode if it's on our path
(with-demoted-errors
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

