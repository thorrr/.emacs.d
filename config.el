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
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `((".*" . ,emacs-savefile-dir)))
;;this won't work until 24.3
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 3))
    (setq undo-tree-auto-save-history 't) 
    (setq undo-tree-auto-save-history nil))
(add-hook 'write-file-hooks 'undo-tree-save-history-hook)
(add-hook 'find-file-hook 'undo-tree-load-history-hook)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; disable slowwww git vc backend on Windows
(setq vc-handled-backends (quote (SVN)))

;; make sure scratch buffer tries to open files in home
(with-current-buffer "*scratch*"
  (setq default-directory "~/"))

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

;;keep server file out of our pristine .emacs.d directory
(if (eq system-type 'windows-nt)
    (setq server-auth-dir (concat (getenv "APPDATA") "\\.emacs.d\\server")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up inconsolata by default if we're on linux
(if (not (eq system-type 'windows-nt))
    (if (ignore-errors
    (let ((retval (set-face-attribute 'default nil :font "Inconsolata"))) ;;set-face-attribute returns nil on success
      (if (not retval) 't retval)))
        'inconsolata-good
      (message "*** sudo apt-get install ttf-inconsolata\nsudo fc-cache -fv to make inconsolata font work on linux")))
;; Consolas is the best font
(if (ignore-errors
    (let ((retval (set-face-attribute 'default nil :font "Consolas")))
      (if (not retval) 't retval)))
        'consolas-good
  (message "*** Consolas font not available on this system.  Install it using the package manager if you want to use it."))
(set-face-attribute 'default nil :height 80)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;misc
(global-subword-mode)
(setq column-number-mode t)
(require 'uniquify) ;;globally unique buffer names
(global-font-lock-mode t)
(tool-bar-mode -1)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
;;show-paren customizations
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

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
(setq recentf-save-file (concat emacs-savefile-dir ".recentf"))
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

;; save point location
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat emacs-savefile-dir "saved-places"))

;; ignore ^M in mixed dos/unix files
(add-hook 'find-file-hook (lambda () (if (fboundp 'remove-dos-eol) (remove-dos-eol)))) ;; protect ourselves if there's a .emacs file problem

;; Save all backup files in this directory (no ~files lying around) 
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir 't))
(setq auto-save-list-file-prefix (concat emacs-savefile-dir ".saves-"))
(setq backup-directory-alist `((".*" . ,emacs-savefile-dir)))
(setq auto-save-file-name-transforms
          `((".*" ,emacs-savefile-dir t)))

;; Enable versioning with default values
(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files t
   version-control t)       ; use versioned backups

(require 'ntcmd)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . ntcmd-mode))

;; fix eshell problems on windows
(if (eq system-type 'windows-nt) (setenv "CDPATH" nil))

;; http://www.emacswiki.org/emacs/EshellCompletion
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))

(add-to-list 'ac-modes 'eshell-mode)
;; make auto-complete start sooner in eshell
(add-hook 'eshell-mode-hook #'(lambda ()
  (setq-local ac-auto-start 3)
  (define-key eshell-mode-map [tab] 'auto-complete)
))

;;el-doc for lisp languages
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
