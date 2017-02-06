;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shift arrow buffer navigation
;;
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
;; (global-set-key (kbd "C-c o") 'my-occur)
;; press C-o to do an occur buffer cduring an interactive search
(define-key isearch-mode-map (kbd "C-o") 'run-occur-during-interactive-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `((".*" . ,emacs-savefile-dir)))
;;undo-tree-save-history fn doesn't like being interruped to ask about encodings
(add-hook 'temp-buffer-setup-hook (lambda ()
  (prefer-coding-system 'utf-8)))
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

(require 'yasnippet)
;;(yas-global-mode 1)

(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-comphist-file (concat emacs-savefile-dir "ac-comphist.dat"))
(define-key ac-completing-map "\e" 'ac-stop)
(setq ac-stop-flymake-on-completing t)

;;fixes for autopair mode which obliterates the mapping for both kbd "RET" and [return]
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map [return] 'ac-complete)

(global-auto-complete-mode 't)
(setq ac-use-menu-map 't)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-start 1) ;;don't automatically start auto-complete until this many characters have been typed
(setq ac-delay .3) ;; crucial to fix typing latency gaps
(setq ac-dwim t)
(setq ac-auto-show-menu 't)  ;;if we want a delay, change this to 0.5, for example
(global-set-key (kbd "M-?") 'auto-complete)
(ac-config-default)

(add-to-list 'ac-sources 'ac-source-yasnippet)

;;keep server file out of our pristine .emacs.d directory
(setq server-auth-dir (expand-file-name "~/.local-emacs/server"))
(server-start)

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

;; unicode-fonts means we display weird unicode chars that aren't in the default font.  crucial.
(setq pcache-directory "~/.local-emacs/var/pcache")
(make-directory pcache-directory 't)
;; unicode-fonts is crucial, but slow at startup so moved to haskell-mode-hook
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)
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
(setq flymake-allowed-file-name-masks nil)  ;;otherwise flymake runs for everything
(add-hook 'find-file-hook 'flymake-find-file-hook)
(eval-after-load 'flymake '(require 'flymake-cursor))
(setq flymake-no-changes-timeout 5);; Only run flymake if I've not been typing for 5 seconds
;; flymake-cursor - turn off before every command
(add-hook 'find-file-hook (lambda ()
  (make-local-variable 'flymake-cursor-is-activated)
  (setq flymake-cursor-is-activated 't)
  (add-hook 'pre-command-hook (lambda ()
    (if (and (boundp 'flymake-cursor-is-activated)
             flymake-cursor-is-activated)
        (progn
          (setq flymake-cursor-is-activated nil)
          (flymake-cursor-mode -1)))))
  ;;turn flymake-cursor back on when we're idle
  (run-with-idle-timer flymake-cursor-error-display-delay 't (lambda ()
    (if (and (boundp 'flymake-cursor-is-activated)
             (not flymake-cursor-is-activated))
        (progn
          (setq flymake-cursor-is-activated 't)
          (flymake-cursor-mode +1)
          (flymake-cursor-show-errors-at-point-pretty-soon)))))))
;;get rid of existing overlay properties
;; (face-spec-reset-face 'flymake-errline)
;; (face-spec-reset-face 'flymake-warnline)
;; (face-spec-reset-face 'flymake-infoline)
;; ;; customize the flymake overlay.
;; (set-face-attribute 'flymake-errline nil :inherit nil :background "HotPink4")
;; (set-face-attribute 'flymake-warnline nil :inherit nil :background "#3D4D3B")
;; (set-face-attribute 'flymake-infoline nil :inherit nil :background "grey30")
;; (add-hook 'flymake-mode-hook (lambda () 
;;                                (local-set-key (kbd "M-P") 'flymake-goto-prev-error)
;;                                (local-set-key (kbd "M-N") 'flymake-goto-next-error)
;;                                ))


;; ;; show-paren customizations
;; (face-spec-reset-face 'show-paren-match)
;; (set-face-attribute 'show-paren-match nil
;;                     :foreground nil
;;                     :weight 'normal
;;                     :background  "#3D5169"
;;                     )

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
  (let ((found-hs-overlay nil))
    (save-excursion
      (ignore-errors (left-char 1))
      ;; if there's a hide-show overlay one character to the left, bump point to the line
      ;; beginning so it doesn't get swept to the end of the block when we toggle hiding
      (if (ignore-errors (overlay-get (car (overlays-at (point))) 'hs))
          (setq found-hs-overlay 't)))
    (if found-hs-overlay (move-beginning-of-line nil))
    (save-excursion
      (move-beginning-of-line nil) (end-of-line) (hs-toggle-hiding))
    ;; now put point back at the end of the line since that's where it started visually
    (if found-hs-overlay (end-of-line))))

(global-set-key (kbd "C-+") 'sane-hs-toggle-hiding)
(global-set-key [C-kp-add] 'sane-hs-toggle-hiding)

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
(add-hook 'python-mode-hook 'hs-minor-mode)

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
(if (not (and (>= emacs-major-version 24) (>= emacs-minor-version 4))) (progn
  (defun revert-buffer-keep-history (&optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
    (interactive)
    ;; from http://stackoverflow.com/q/4924389
    ;; tell Emacs the modtime is fine, so we can edit the buffer
    (clear-visited-file-modtime)
    
    ;; insert the current contents of the file on disk
    (widen)
    (delete-region (point-min) (point-max))
    (insert-file-contents (buffer-file-name))
    
    ;; mark the buffer as not modified
    (not-modified)
    (set-visited-file-modtime))
  
  (setq revert-buffer-function 'revert-buffer-keep-history)
  (defun ask-user-about-supersession-threat (fn)
    "blatantly ignore files that changed on disk"
    )
))

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
(require 'ido-vertical-mode)
(setq ido-vertical-show-count 't)
(ido-vertical-mode 't)
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
(require 'cl)
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (cl-letf (((symbol-function 'insert-and-inherit) (lambda (&rest args))))
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

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;;paredit for things that want it
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;;el-doc for lisp languages
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;shell improvements for windows
(if (eq system-type 'windows-nt) (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; bash-completion-dynamic-complete plus tweaks
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (autoload 'bash-completion-dynamic-complete "bash-completion" "BASH completion hook")
    (require 'cl-lib)

    ;; make bash-completion-require-process work with fakecygpty
    (setq bash-completion-prog "fakecygpty") ;;full command will be fakecygpty bash --noediting
    (defun bash-completion-require-process-around (orig-fun &rest args)
      (cl-letf* (;;save definition of start-process
                 ((symbol-function 'this-fn) (symbol-function 'start-process))
                 ;;now override it to let us add our own program args
                 ((symbol-function 'start-process)
                  (lambda (name buffer program &rest program-args)
                    ;; use "apply" since the last argument is "spread" as the
                    ;; remaining arguments.  Otherwise we could call this-fn directy, like
                    ;; (this-fun name buffer prgram ...)
                    (apply #'this-fn name buffer program "bash" program-args))))
        (apply orig-fun args)))

    ;;turn off shell-quote-argument, it's adding unnecessary quotes around the completion candidates
    (defun bash-completion-escape-candidate-around (orig-fun &rest args)
      (cl-letf* (((symbol-function 'shell-quote-argument) #'identity))
        (apply orig-fun args)))

    ;;turn off completion when hitting "enter".  Otherwise the completion will needlessly
    ;;timeout on an empty match
    (defun completion-in-region--postch-around (orig-fun &rest args)
      (cl-letf (((symbol-function 'bash-completion-send) #'ignore))
        (apply orig-fun args)))

    ;; turn off the "Bash completion..." message
    (defun bash-completion-dynamic-complete-0-around (orig-fun &rest args)
      (cl-letf (((symbol-function 'message) #'format))
        (apply orig-fun args)))

    ;; activate all of our advices
    (advice-add 'bash-completion-require-process :around #'bash-completion-require-process-around)
    (advice-add 'bash-completion-escape-candidate :around #'bash-completion-escape-candidate-around)
    (advice-add 'completion-in-region--postch :around #'completion-in-region--postch-around)
    (advice-add 'bash-completion-dynamic-complete-0 :around #'bash-completion-dynamic-complete-0-around)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; End bash-completion-dynamic tweaks
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun bash-setup-hook ()
      ;; give this a sane name
      (rename-buffer "*Bash*" 't)
      ;; fix colors and diff output
      (process-send-string (get-buffer-process (current-buffer)) "export TERM=xterm\n")
      ;; fix man
      (process-send-string (get-buffer-process (current-buffer)) "export LANG=en_US.UTF-8\n")
      ;; unset our directory change shortcuts because it confuses bash-completion
      (process-send-string (get-buffer-process (current-buffer)) "alias cd=cd\n")
      (process-send-string (get-buffer-process (current-buffer)) "alias ..=\n")
      (process-send-string (get-buffer-process (current-buffer)) "alias cd..=cd..\n")
      ;; make bash-completion start in a sane location
      (setq default-directory (getenv "USERPROFILE"))
      ;; add a command "emacs" to edit a file
      (process-send-string (get-buffer-process (current-buffer)) (concat
        "alias emacs='" invocation-directory "/emacsclientw.exe -n -q --server-file " server-auth-dir "/server" "'\n"))
      )

    ;; finally, the function that actually starts a bash shell
    (defun bash ()
      (interactive)
      (add-hook 'shell-mode-hook 'bash-setup-hook)
      (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
      (async-shell-command (concat "fakecygpty.exe bash --login -i -c "
                                   "\"mkpasswd -c > /etc/passwd; mkgroup -c > /etc/group; "
                                   "cd ~/; exec /bin/bash\""))
      ;;turn off bash-completion
      (remove-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
      ;;turn off bash stuff so cmd still works
      (remove-hook 'shell-mode-hook 'bash-setup-hook))
    )) ;;end windows-nt bash stuff

;;general shell-mode tweaks
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'shell-mode-hook (lambda ()
  (set (make-local-variable 'comint-scroll-to-bottom-on-input) 't) ;; jump to bottom when you start typing
  (set (make-local-variable 'comint-scroll-to-bottom-on-output) 't) ;; jump to bottom when there's output
  (set (make-local-variable 'comint-scroll-show-maximum-output) 't) ;; scroll to show max possible output
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil) ;; don't ask about live bash's
; make completion buffers disappear after n seconds.
  (add-hook 'completion-setup-hook
            (lambda () (run-at-time 3 nil
              (lambda () (delete-windows-on "*Completions*")))))
  ;; fakecygpty makes unprintable characters visible, get rid of them
  (if (eq system-type 'windows-nt)
  (add-hook 'comint-preoutput-filter-functions (lambda (output)
            (replace-regexp-in-string  "\^\[.+\^G" "" output))))
  ;; make C-z background the running process, not Emacs itself.  Send a C-z then RET:
  (local-set-key (kbd "C-z") 'self-insert-command)
  (local-set-key (kbd "C-c") 'self-insert-command)
  (local-set-key (kbd "C-d") 'self-insert-command)
  (local-set-key (kbd "<up>") (lambda () (interactive) (end-of-buffer) (comint-previous-input 1)))
  (local-set-key (kbd "<down>") (lambda () (interactive) (end-of-buffer) (comint-next-input 1)))
  (local-set-key (kbd "M-i") (lambda () (interactive) (end-of-buffer)))
  (add-hook 'post-self-insert-hook (lambda ()
    ;; make these commands send a RET automatically
    (if (= last-command-event 26) (comint-send-input)) ;; C-z is "26"
    (if (= last-command-event 3) (comint-send-input))  ;; C-c is "3"
    (if (= last-command-event 4) (comint-send-input))  ;; C-d is "4"
    ))
  ))

;; M-d and M-DEL shouldn't save to the kill ring.
(defun preserve-kill-ring (fn-s &optional arg)
  (let ((old-kill-ring kill-ring)
        (old-kill-ring-yank-pointer kill-ring-yank-pointer)
        (last-command nil))  ;;keep kill-region from appending multiple kills
    (if arg
        (funcall fn-s arg)
      (funcall fn-s))
    (setq kill-ring old-kill-ring)
    (setq kill-ring-yank-pointer old-kill-ring-yank-pointer)))

(defun subword-backward-delete (arg)
  (interactive "p")
  (preserve-kill-ring 'subword-backward-kill arg))

(defun subword-forward-delete (arg)
  (interactive "p")
  (preserve-kill-ring 'subword-kill arg))

(global-set-key (kbd "M-d") 'subword-forward-delete)
(global-set-key (kbd "M-DEL") 'subword-backward-delete)
;;preserve old definitions of paredit-*-word and override with preserve-kill-ring
(add-hook 'paredit-mode-hook (lambda ()
  (if (not (fboundp 'paredit-forward-kill-word-orig))
      (fset 'paredit-forward-kill-word-orig (symbol-function 'paredit-forward-kill-word)))
  (if (not (fboundp 'paredit-backward-kill-word-orig))
      (fset 'paredit-backward-kill-word-orig (symbol-function 'paredit-backward-kill-word)))
   (defun paredit-forward-kill-word ()
     (interactive)
     (preserve-kill-ring 'paredit-forward-kill-word-orig))
   (defun paredit-backward-kill-word ()
     (interactive)
     (preserve-kill-ring 'paredit-backward-kill-word-orig))))

;; electric-indent-mode is automatic in emacs 24.4+. Swap C-j and RET to get the old behavior
(if (and (>= emacs-major-version 24)
         (>= emacs-minor-version 4)) (progn
  (global-set-key (kbd "<RET>") 'electric-indent-just-newline)
  (global-set-key (kbd "C-j") 'newline)))
