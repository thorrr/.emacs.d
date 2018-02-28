;; create a minor mode to override any major-mode keygrabs
(defvar keymaps-mode-map (make-sparse-keymap)
  "Keymap for 'keymaps-mode'.")

(define-minor-mode keymaps-mode
  "Override major mode keybindings"
  :init-value 't
  :lighter " keymaps-mode"
  :keymap keymaps-mode-map
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add new non-overrideable keymaps here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key keymaps-mode-map (kbd "C-c f") 'insert-foo)
(define-key keymaps-mode-map "\M-g" 'goto-line)
(define-key keymaps-mode-map (kbd "M-n") 'duplicate-line)
(define-key keymaps-mode-map (kbd "M-'") 'quote-wrap-forward-word)
(define-key keymaps-mode-map [C-prior] 'rotate-frame-anticlockwise)
(define-key keymaps-mode-map [C-next] 'rotate-frame-clockwise)
(require 'move-text)
(define-key keymaps-mode-map [M-up] 'move-text-up)
(define-key keymaps-mode-map [M-down] 'move-text-down)
(define-key keymaps-mode-map "\M-j" 'join-next-line)
(define-key keymaps-mode-map [f5] 'write-last-macro-to-messages)
(define-key keymaps-mode-map [f8] 'sr-speedbar-toggle)
(define-key keymaps-mode-map [S-f12] 'toggle-camelcase-at-point)
(define-key keymaps-mode-map (kbd "M-h") 'my-iflipb-next-buffer)
(define-key keymaps-mode-map (kbd "M-H") 'my-iflipb-previous-buffer)
(define-key keymaps-mode-map (kbd "<C-tab>") 'my-iflipb-next-buffer)
(define-key keymaps-mode-map (kbd "<C-S-tab>") 'my-iflipb-previous-buffer)
;; use hippie-expand instead of dabbrev
(define-key keymaps-mode-map (kbd "M-/") 'hippie-expand)
(define-key keymaps-mode-map (kbd "C-M-/") 'undo-tree-visualize)
;; replace buffer-menu with ibuffer
(define-key keymaps-mode-map (kbd "C-x C-b") 'ibuffer)
;; add a more convenient "brace" character than RAlt+( on Linux
(if (not (eq system-type 'windows-nt)) (progn
  (define-key keymaps-mode-map (kbd "C-(") "{")
  (define-key keymaps-mode-map (kbd "C-)") "}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End add keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(define-globalized-minor-mode global-keymaps-mode keymaps-mode keymaps-mode)

;; ;; The keymaps in `emulation-mode-map-alists' take precedence over
;; ;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((keymaps-mode . ,keymaps-mode-map)))


(defun turn-off-keymaps-mode ()
  "Turn off keymaps-mode."
  (keymaps-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-keymaps-mode)

(provide 'keymaps-mode)