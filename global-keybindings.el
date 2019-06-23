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
;;(define-key keymaps-mode-map [M-up] 'move-text-up)
;;(define-key keymaps-mode-map [M-down] 'move-text-down)
(define-key keymaps-mode-map "\M-j" 'join-next-line)
(define-key keymaps-mode-map [f5] 'write-last-macro-to-messages)
(define-key keymaps-mode-map [f8] 'sr-speedbar-toggle)
(define-key keymaps-mode-map [S-f11] 'toggle-camelcase-at-point)
(define-key keymaps-mode-map (kbd "M-h") 'iflipb-next-buffer)
(define-key keymaps-mode-map (kbd "M-H") 'iflipb-previous-buffer)
(define-key keymaps-mode-map (kbd "<C-tab>") 'my-iflipb-next-buffer)
(define-key keymaps-mode-map (kbd "<C-S-tab>") 'my-iflipb-previous-buffer)
;; use hippie-expand instead of dabbrev
(define-key keymaps-mode-map (kbd "M-/") 'hippie-expand)
(define-key keymaps-mode-map (kbd "C-M-/") 'undo-tree-visualize)
;; replace buffer-menu with ibuffer
(define-key keymaps-mode-map (kbd "C-x C-b") 'ibuffer)

(define-key keymaps-mode-map (kbd "M-=") 'er/expand-region) ;; don't need contract because
                                                            ;; the hotkeys are triggered
                                                            ;; on the first press

;; dumb-jump is awesome
(defun dumb-jump-go-or-override ()
  ;; allow overriding dumb-jump with your mode's own jump function
  (interactive)
  (if (boundp 'dumb-jump-go-override)
      (funcall dumb-jump-go-override) (dumb-jump-go)))

(defun dumb-jump-back-or-override ()
  (interactive)
  (if (boundp 'dumb-jump-back-override)
      (funcall dumb-jump-back-override) (dumb-jump-back)))

(define-key keymaps-mode-map (kbd "M-.") 'dumb-jump-go-or-override)
(define-key keymaps-mode-map (kbd "M-,") 'dumb-jump-back-or-override)
(define-key keymaps-mode-map (kbd "C-M-.") 'dumb-jump-quick-look)

;; ace jump stuff
(define-key keymaps-mode-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key keymaps-mode-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key keymaps-mode-map [(meta s)] 'ace-jump-mode)

(define-key keymaps-mode-map (kbd "C-c q") 'auto-fill-mode)
(define-key keymaps-mode-map (kbd "C-x C-x") 'my-exchange-point-and-mark)
(define-key keymaps-mode-map (kbd "C-c m") 'minimap-toggle)
(define-key keymaps-mode-map (kbd "<f2>") 'toggle-line-numbers)
(define-key keymaps-mode-map (kbd "M-S-<down>") 'mc/mark-next-like-this)
(define-key keymaps-mode-map (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(define-key keymaps-mode-map (kbd "M-S-<next>") 'mc/mark-all-like-this)
(define-key keymaps-mode-map (kbd "M-S-<prior>") 'mc/mark-all-like-this)
(define-key keymaps-mode-map (kbd "M-?") 'auto-complete)
(define-key keymaps-mode-map (kbd "TAB") 'smart-auto-complete)
(define-key keymaps-mode-map [(meta f11)] 'recentf-open-files)
(define-key keymaps-mode-map (kbd "C-+") 'sane-hs-toggle-hiding)
(define-key keymaps-mode-map [C-kp-add] 'sane-hs-toggle-hiding)
(define-key keymaps-mode-map (kbd "M-d") 'subword-forward-delete)
(define-key keymaps-mode-map (kbd "M-DEL") 'subword-backward-delete)

;; add a more convenient "brace" character than RAlt+( on Linux attempt to properly rebind
;; C-( and C-) to make braces but keep brace overrides.  This is not turn-offable via our
;; keymaps-mode.  This should bind "deeper" than the following method which just maps C-(
;; to the "{" macro
(define-key input-decode-map [?\C-\(] (kbd "{"))
(define-key input-decode-map [?\C-\)] (kbd "}"))

;; (if (not (eq system-type 'windows-nt))
;;     (progn
;;       (define-key keymaps-mode-map (kbd "C-(") "{")
;;       (define-key keymaps-mode-map (kbd "C-)") "}")))


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
(add-hook 'magit-setup-buffer-hook #'turn-off-keymaps-mode)

(provide 'keymaps-mode)
