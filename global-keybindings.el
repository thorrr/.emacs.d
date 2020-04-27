;; create a minor mode to override any major-mode keygrabs
(defvar keymaps-mode-map (make-sparse-keymap)
  "Keymap for 'keymaps-mode'.")

(define-minor-mode keymaps-mode
  "Override major mode keybindings"
  :init-value 't
  :lighter " keymaps-mode"
  :keymap keymaps-mode-map
)
;;;###autoload
(define-globalized-minor-mode global-keymaps-mode keymaps-mode keymaps-mode)

;; auto-turn-off for some modes
(add-hook 'minibuffer-setup-hook (lambda () (keymaps-mode -1)))
(add-hook 'magit-mode-hook (lambda () (keymaps-mode -1)))

;; The keymaps in `emulation-mode-map-alists' take precedence over `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((keymaps-mode . ,keymaps-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add new non-overrideable keymaps here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key keymaps-mode-map "\M-g" 'goto-line)
(define-key keymaps-mode-map (kbd "M-n") 'duplicate-line)
(define-key keymaps-mode-map (kbd "M-'") 'quote-wrap-forward-word)
(define-key keymaps-mode-map [C-prior] 'rotate-frame-anticlockwise)
(define-key keymaps-mode-map [C-next] 'rotate-frame-clockwise)
;;(define-key keymaps-mode-map [M-up] 'move-text-up)
;;(define-key keymaps-mode-map [M-down] 'move-text-down)
(define-key keymaps-mode-map "\M-j" 'join-next-line)
(define-key keymaps-mode-map [f5] 'write-last-macro-to-messages)
(define-key keymaps-mode-map [f8] 'cider-view-toggle)
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
(define-key keymaps-mode-map (kbd "M-.") 'dumb-jump-go)
(define-key keymaps-mode-map (kbd "M-,") 'dumb-jump-back)
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
(define-key keymaps-mode-map (kbd "C-x g") 'magit-status)
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

;; Override specific keybindings.  We must define a macro because e.g. define-minor-mode
;; doesn't accept a symbol as an argument
(defmacro keymaps-mode-override (major-mode &rest body)
  (let* ((mode-name (concat "-keymaps-mode-" (symbol-name major-mode)))
         (map-name (concat "keymaps-mode-" (symbol-name major-mode) "-map"))
         (mode-symbol (intern mode-name))
         (mode-map (intern map-name))
         (major-mode-hook (intern (concat (symbol-name major-mode) "-hook")))
         (keys '()))
    ;; first, build define-key list
    (while body
      (add-to-list 'keys `(define-key ,mode-map ,(car body) ,(cadr body)))
      (setq body (cddr body)))
    ;; here's the macro return value
    `(progn
       (defvar ,mode-map (make-sparse-keymap))
       (set-keymap-parent ,mode-map keymaps-mode-map)
       (define-minor-mode ,mode-symbol
         :init-value nil
         :lighter ""
         :keymap ,mode-map)
       (provide ',mode-symbol)
       ;; add the override keymap to the front of the emulation-mode-map list; the new mode
       ;; has precedence
       (add-to-list 'emulation-mode-map-alists
                    (list (cons ',mode-symbol ,mode-map)))
       ;; turn on new mode by default
       (add-to-list ',major-mode-hook (lambda () (,mode-symbol 1)))
       ;; insert define-key entries
       ,@keys
       ;; toggle new keymaps-mode when the base mode is toggled
       (add-hook 'keymaps-mode-off-hook (lambda () (,mode-symbol -1)))
       (add-hook 'keymaps-mode-on-hook (lambda () (,mode-symbol 1)))
       ;; done
       t
       )))

(keymaps-mode-override term-mode (kbd "TAB") 'term-send-raw)

(provide 'keymaps-mode)
