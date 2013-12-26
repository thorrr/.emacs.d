;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shift arrow buffer navigation
(when (fboundp 'windmove-default-keybindings)
       (windmove-default-keybindings))
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "M-n") 'duplicate-line)
(global-set-key (kbd "M-'") 'quote-wrap-forward-word)
(global-set-key [C-prior] 'rotate-frame-anticlockwise)
(global-set-key [C-next] 'rotate-frame-clockwise)
(require 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key "\M-j" 'join-next-line)
(global-set-key [f5] 'write-last-macro-to-messages)
(global-set-key [f12] 'toggle-camelcase-at-point)
(global-set-key (kbd "M-h") 'my-iflipb-next-buffer)
(global-set-key (kbd "M-H") 'my-iflipb-previous-buffer)
(global-set-key (kbd "<C-tab>") 'my-iflipb-next-buffer)
(global-set-key (kbd "<C-S-tab>") 'my-iflipb-previous-buffer)
;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-/") 'undo-tree-visualize)
; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; occur is cool
(global-set-key (kbd "C-c o") 'my-occur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-comphist-file (concat emacs-savefile-dir "ac-comphist.dat"))
(define-key ac-completing-map "\e" 'ac-stop)
(setq ac-stop-flymake-on-completing t)
;;fixes for autopair mode which obliterates the mapping for both kbd "RET" and [return]
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map [return] 'ac-complete)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-start 3) ;;don't automatically start auto-complete until this many characters have been typed
(setq ac-dwim t)
(global-set-key (kbd "M-?") 'auto-complete)
(ac-config-default)

;; Flymake
(when (load "flymake" t) (setq flymake-allowed-file-name-masks nil))  ;;otherwise flymake runs for everything
(add-hook 'find-file-hook 'flymake-find-file-hook)
(eval-after-load 'flymake '(require 'flymake-cursor))
(setq flymake-no-changes-timeout 5);; Only run flymake if I've not been typing for 5 seconds
;;get rid of existing overlay properties
(face-spec-reset-face 'flymake-errline)
(face-spec-reset-face 'flymake-warnline)
(face-spec-reset-face 'flymake-infoline)
;; customize the flymake overlay.
(set-face-attribute 'flymake-errline nil :inherit nil :background "HotPink4")
(set-face-attribute 'flymake-warnline nil :inherit nil :background "#3D4D3B")
(set-face-attribute 'flymake-infoline nil :inherit nil :background "grey30")
(add-hook 'flymake-mode-hook (lambda () 
                               (local-set-key (kbd "M-P") 'flymake-goto-prev-error)
                               (local-set-key (kbd "M-N") 'flymake-goto-next-error)
                               ))

;; show-paren customizations
(face-spec-reset-face 'show-paren-match)
(set-face-attribute 'show-paren-match nil
                    :foreground nil
                    :weight 'normal
                    :background  "#3D5169"
                    )

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)

;; hide/show mode
(defun sane-hs-toggle-hiding ()
  (interactive)
  (save-excursion (end-of-line)(hs-toggle-hiding))
  )
(global-set-key (kbd "C-+") 'sane-hs-toggle-hiding)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'ess-mode-hook        'hs-minor-mode)
(add-hook 'scala-mode-hook      'hs-minor-mode)
(add-hook 'ensime-mode-hook     'hs-minor-mode)
(add-hook 'ensime-scala-mode-hook 'hs-minor-mode)

;; project-root
(require 'project-root)

(global-set-key (kbd "C-c g r")
                (lambda ()
                  (interactive)
                  (with-project-root
                      (call-interactively 'rgrep)
                      )))

(defun my-paredit-wrap-around ()(interactive)(save-excursion (forward-word) (backward-word) (paredit-wrap-round)))
(defun my-paredit-wrap-square ()(interactive)(save-excursion (forward-word) (backward-word) (paredit-wrap-square)))
(defun my-paredit-wrap-curly ()(interactive)(save-excursion (forward-word) (backward-word) (paredit-wrap-curly)))

;; paredit customizations
(add-hook 'paredit-mode-hook (lambda ()
    (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
    (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
    (local-set-key (kbd "M-(") 'my-paredit-wrap-around)
    (local-set-key (kbd "M-[") 'my-paredit-wrap-square)
    (local-set-key (kbd "M-{") 'my-paredit-wrap-curly)
    (local-set-key (kbd "C-M-x") 'slime-eval-defun)  ;;just like elisp
    ))
	    
;; make M-z evaluate "this" sexp
(defun slime-evaluate-this-sexp ()
  (interactive)
  (save-excursion
    (paredit-forward-up)
    (slime-eval-last-expression)
    )
  )


;; make M-Z evaluate EOL sexp
(defun slime-evaluate-EOL-sexp ()
  (interactive)
  (save-excursion
    (paredit-forward-up)
    (move-end-of-line nil)
    (slime-eval-last-expression)
    )
  )

;;buffer flipping
(require 'iflipb)
(setq iflipb-wrap-around t)

;; auto off function iflipb'ing
(setq my-iflipb-timeout 0.8)
(setq my-iflipb-timer-object nil)
(defun my-iflipb-timer ()
  (cancel-timer my-iflipb-timer-object)
  (setq my-iflipb-timer-object nil))

(defun my-iflipb-next-buffer (arg)
  (interactive "P")
  (iflipb-next-buffer arg)
  ;;reset timer if it's running
  (if my-iflipb-timer-object
      (cancel-timer my-iflipb-timer-object))
  (setq my-iflipb-timer-object (run-with-idle-timer my-iflipb-timeout nil 'my-iflipb-timer)))

(defun my-iflipb-previous-buffer ()
  (interactive)
  (iflipb-previous-buffer)
  ;;reset timer if it's running
  (if my-iflipb-timer-object
      (cancel-timer my-iflipb-timer-object))
  (setq my-iflipb-timer-object (run-with-idle-timer my-iflipb-timeout nil 'my-iflipb-timer)))

(defun iflipb-first-iflipb-buffer-switch-command ()
  "Override existing function - add check for my-iflipb-timer-object"
  (not (and (or (eq last-command 'my-iflipb-next-buffer)
                (eq last-command 'my-iflipb-previous-buffer))
            my-iflipb-timer-object)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (concat emacs-savefile-dir "ido.hist")
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)
(setq ido-create-new-buffer 'always)
(set-default 'imenu-auto-rescan t)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(setq ido-max-prospects 4)
;;the following tweak doesn't work on old emacs versions
(if (>= emacs-major-version 24) (setq ido-completion-buffer nil))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; don't want to open stuff automatically
(setq ido-confirm-unique-completion 't)
(add-hook 'ido-setup-hook
	  (lambda ()
              ;;need these for vertical results mode
	      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
	      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
	      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
	      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
              ;;map m-bsp to just bsp because that's how it acts in ido mode
	      (define-key ido-completion-map (kbd "M-<del>") 'ido-delete-backward-updir)
	      (define-key ido-completion-map (kbd "M-<backspace>") 'ido-delete-backward-updir)
              ;;get rid of annoying "kill file" function
	      (define-key ido-completion-map (kbd "C-k") nil)
              ;;C-a goes to the front of the directory tree (home directory)
	      (define-key ido-completion-map (kbd "C-a") (lambda () (interactive)
                    (ido-set-current-home)
                    (setq refresh t) (setq ido-exit 'refresh) (exit-minibuffer)))
	      ))

;; rotate windows within a frame
(require 'transpose-frame)

;;ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map [(meta s)] 'ace-jump-mode)
(setq ace-jump-mode-case-fold t) ;; case insensitive
(setq ace-jump-mode-move-keys
      (loop for i from ?a to ?z collect i)) ;;only lowercase jump characters

;; eshell customizations
;;   fix colors
(require 'ansi-color)
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
(setq ansi-color-map (ansi-color-make-color-map))
;;   make sure eshell is cleanly machine local
(setq eshell-directory-name (concat emacs-savefile-dir  "eshell/"))
(setq eshell-aliases-file (concat emacs-savefile-dir  "eshell/alias"))
;;   add this line to the bottom of .bash_aliases to parse your bash
;;   aliases into an eshell "alias" file:
;;   alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" > ~/.local-emacs/auto-save-list/eshell/alias