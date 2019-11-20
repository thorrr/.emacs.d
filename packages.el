;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package definitions
;;
;; All external packages and setup logic goes below here.  Put mode-specific packages in
;; modes/<mode>.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :custom
  (ace-jump-mode-case-fold t) ;; case insensitive
  (ace-jump-mode-move-keys
   (loop for i from ?a to ?z collect i)) ;;only lowercase jump characters  
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package auto-complete
  :init
  ;; must be defined before :bind section
  (defun ac-select-current ()
    (interactive)
    (if (or (eq last-command 'ac-previous)
            (eq last-command 'ac-next)
            (eq last-command 'ac-expand)
            (eq last-command 'ac-expand-previous)
            (eq last-command 'ac-expand-common)
            (eq last-command 'ac-complete)
            (eq last-command 'smart-auto-complete)
            (eq last-command 'self-insert-command))
        ;; disable all "RET" functions
        (cl-letf (((symbol-function 'newline) (lambda (&optional arg interactive) (interactive "*P\np")))
                  ((symbol-function 'newline-and-indent) (lambda () (interactive "*"))))
          (ac-complete))))

  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-modes 'eshell-mode)

  :custom
  (ac-comphist-file (concat emacs-savefile-dir "ac-comphist.dat"))
  (ac-stop-flymake-on-completing t)
  (global-auto-complete-mode)
  (ac-use-menu-map 't)
  (ac-auto-start 1) ;; don't automatically start auto-complete until this many characters have been typed
  (ac-delay .3)     ;; crucial to fix typing latency gaps
  (ac-quick-help-delay 0.2) ;; pop up help stuff a little faster than default
  (ac-dwim t)
  (ac-auto-show-menu 't) ;; if we want a delay, change this to 0.5, for example

  :hook ((prog-mode . (lambda () (ac-config-default))) 
         (yas-minor-mode . (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
         ;; turn off company mode when auto-complete-mode is activated
         (auto-complete-mode . (lambda () (if (bound-and-true-p company-mode) (company-mode -1)))))  

  :bind (:map ac-completing-map
              ("\e" . ac-stop)
              ("RET" . ac-select-current)
              ([return] . ac-select-current)
              ("C-n" . ac-next)
              ("C-p" . ac-previous)
              ([backtab] . ac-previous)
              :map ac-menu-map
              ("RET" . ac-select-current)
              ([return] . ac-select-current)))

(el-get-bundle auto-save-buffer
  :url "https://github.com/thorrr/auto-save-buffer.git")
(use-package auto-save-buffer
  :ensure nil ;; use el-get version
  :custom
  (auto-save-buffer-interval 3)
  (auto-save-buffer-only-after-regular-save 't)
  (auto-save-buffer-messaging nil)
  :init
  (add-hook 'find-file-hooks 'turn-on-auto-save-buffer) ;; real-auto-save every single file
  ;;(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-save-buffer)  ;; mode specific auto-save
)

(el-get-bundle awesome-tab
  :url "https://github.com/manateelazycat/awesome-tab.git")
(use-package awesome-tab
  :ensure nil ;; use el-get version
)

(el-get-bundle blackout
  :url "https://github.com/raxod502/blackout.git")
(use-package blackout
  :ensure nil ;; use-el-get version
  :config
  ;;(blackout 'auto-complete-mode)
  (blackout 'eldoc-mode)
  (blackout 'flymake-mode)
  (add-hook 'hs-minor-mode-hook (lambda ()
            (blackout 'hs-minor-mode)))
  (add-hook 'ycmd-mode-hook (lambda ()
            (blackout 'ycmd-mode))))

(use-package company
  ;;:blackout t
  :init
  ;; these must be in :init because they're used in :bind
  (defun company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))
  (defun company-smart-complete ()
    "complete with tab if we've specifically selected a completion.
     Otherwise select next."
    (interactive)
    (if (or (eq last-command 'company-select-next)
            (eq last-command 'company-select-previous))
        (company-complete)
      (company-select-next-if-tooltip-visible-or-complete-selection)))

  :custom
  ;; the following variables + keybindings make company-mode behave similarly to
  ;; auto-complete-mode

  (company-require-match nil)
  (company-auto-complete #'company-visible-and-explicit-action-p)
  (company-frontends '(company-echo-metadata-frontend
                       company-pseudo-tooltip-unless-just-one-frontend-with-delay
                       company-preview-frontend))

  :bind (:map company-active-map
              ([tab] . company-smart-complete)
              ("TAB" . company-smart-complete)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("S-TAB" . company-select-previous)
              ("<backtab>" . company-select-previous))

  :hook (company-mode
         . (lambda ()
             ;; turn off auto-complete-mode when company mode is activated
             (if (bound-and-true-p auto-complete-mode)
                 (auto-complete-mode -1))))) 


(use-package codesearch)

(use-package counsel)

(use-package counsel-codesearch)

(use-package counsel-projectile)

(use-package delight)

(use-package diminish)

(use-package dired-k
  :init
  (add-hook 'dired-initial-position-hook 'dired-k))

(use-package dumb-jump)

(use-package elsa)

(use-package expand-region)

(use-package fill-column-indicator
  :custom
  (fci-always-use-textual-rule 't)
  (fci-handle-truncate-lines nil)
  (fci-rule-color "gray35")

  :config
  ;; turn on only for non-special buffers
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode))
           (not (eq major-mode 'magit-status-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode)))

  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; turn off fci-mode when buffer is too narrow
  (defvar i42/fci-mode-suppressed nil)
  (make-variable-buffer-local 'i42/fci-mode-suppressed)

  (defun fci-width-workaround (&optional frame)
    (let ((fci-enabled (symbol-value 'fci-mode))
          (fci-column (if fci-rule-column fci-rule-column fill-column))
          (current-window-list (window-list frame 'no-minibuf)))
      (dolist (window current-window-list)
        (with-selected-window window
          (if i42/fci-mode-suppressed
              (when (and (not fci-enabled)
                         fci-column
                         (< fci-column
                            (+ (window-width) (window-hscroll))))
                (setq i42/fci-mode-suppressed nil)
                (turn-on-fci-mode))
            (when (and fci-enabled fci-column
                       (>= fci-column
                           (+ (window-width) (window-hscroll))))
              (setq i42/fci-mode-suppressed t)
              (turn-off-fci-mode)))))))
  (add-hook 'window-size-change-functions 'fci-width-workaround)
  (add-hook 'window-configuration-change-hook 'fci-width-workaround)

  ;; fix for company-mode completion bugs
  (defun on-off-fci-before-company(command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company))

(use-package flycheck
  :blackout t
  :commands flycheck-mode
  :init
  (defun flymake-mode-turn-off ()
    (flymake-mode 0))
  (add-hook 'flycheck-mode-hook #'flymake-mode-turn-off)
  (add-hook 'eglot--managed-mode-hook #'flymake-mode-turn-off))

(use-package flycheck-elsa)

(use-package flymake
  :ensure nil ;; use builtin version
  :delight
  :init
  (defun flycheck-mode-turn-off ()
    (flycheck-mode 0))
  :custom
  (flymake-allowed-file-name-masks nil) ;;otherwise flymake runs for everything
  (flymake-no-changes-timeout 5) ;; Only run flymake if I've not been typing for 5 seconds
  ;; don't run flycheck at the same time
  (advice-add 'flymake-mode :after #'flycheck-mode-turn-off)
  :hook
  (find-file . flymake-find-file-hook))

(el-get-bundle flymake-cursor
  ;; this fork has emacs 26+ compatibility
  :url ;;"https://github.com/akash-akya/emacs-flymake-cursor.git"
  ;; use this until the pull request is merged
  "https://github.com/thorrr/emacs-flymake-cursor.git"
  )
(use-package flymake-cursor
  :ensure nil ;; use el-get-bundle version
  :after (flymake)
  :hook
  ;; flymake-cursor - turn off before every command to fix fast scrolling
  (find-file . (lambda ()
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
             (not flymake-cursor-is-activated)
             flymake-mode) ;; this logic will break if flymake-cursor-mode is deactivated independently
        (progn
          (setq flymake-cursor-is-activated 't)
          (flymake-cursor-mode +1)
          (flymake-cursor-show-errors-at-point-pretty-soon))))))))

(use-package git-gutter-fringe
  :blackout git-gutter-mode
  :custom
  (git-gutter:handled-backends '(git svn))
  :config
  (global-git-gutter-mode +1)
  ;;auto-save-buffer calls write-file which doesn't naturally call the git-gutter refresh fn
  (defadvice write-file (after write-file-git-gutter-mode activate) (git-gutter)))

(use-package ido-vertical-mode
  :init
  ;; setup the ido builtin
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-completion-buffer nil
        ;; don't want to open stuff automatically
        ido-confirm-unique-completion 't
        ido-save-directory-list-file (concat emacs-savefile-dir "ido.hist")
        ido-default-file-method 'selected-window)

  (icomplete-mode +1)
  (setq ido-create-new-buffer 'always)
  (set-default 'imenu-auto-rescan t)
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
  (add-hook 'ido-setup-hook
    (lambda ()
      ;;need these for vertical results mode
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
      ;;map m-bsp to just bsp because that's how it acts in ido mode
      (define-key ido-completion-map (kbd "M-<del>") 'ido-delete-backward-word-updir)
      (define-key ido-completion-map (kbd "M-<backspace>") 'ido-delete-backward-word-updir)
      ;;get rid of annoying "kill file" function
      (define-key ido-completion-map (kbd "C-k") nil)
      ;;C-a goes to the front of the directory tree (home directory)
      (define-key ido-completion-map (kbd "C-a")
        (lambda ()
          (interactive)
          (ido-set-current-home)
          (setq refresh t) (setq ido-exit 'refresh) (exit-minibuffer)))))
  
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
  (ido-vertical-mode t)

  :custom
  (ido-vertical-show-count 't)
  (ido-max-prospects 4)
  (ido-completion-buffer nil)
  ;; don't want to open stuff automatically
  (ido-confirm-unique-completion 't))

(use-package ivy)

(use-package ivy-hydra)

(use-package ivy-prescient)

(use-package ivy-yasnippet)

(use-package ivy-ycmd)

(use-package counsel )

(el-get-bundle iflipb
  :url "https://github.com/emacsmirror/iflipb.git")
(use-package iflipb
  :ensure nil ;; use el-get version
  :custom
  ;; turn off iflip coming all the way around (the default)
  (iflipb-wrap-around nil)
  :config
  (defun __iflibp_init ()
    ;; don't flip to buffers that are showing
    (defun buffer-showing? (b)
      (let ((showing-buffer-names
             (mapcar (lambda (w) (buffer-name (window-buffer w)))
                     (window-list (car (visible-frame-list)))))) ;;assuming single frame...
        ;; remove current buffer - it's eligible to be switched
        (setq showing-buffer-names
              (delq (buffer-name (current-buffer)) showing-buffer-names))
        (memq b showing-buffer-names)))
    
    ;; This var is smart - if it's a list it filters using every elt
    (setq iflipb-always-ignore-buffers (list
      ;; default value
      (car (get 'iflipb-always-ignore-buffers 'standard-value))
      ;; also use our filtering function
      (symbol-function 'buffer-showing?)))
    ;; set a timer so that pausing also resets the flip
    (setq my-iflipb-timeout 0.8)
    (setq my-iflipb-timer-object nil)
    
    ;; timer methods
    (defun my-iflipb-timer-cancel ()
      (cancel-timer my-iflipb-timer-object)
      (setq my-iflipb-timer-object nil))
    
    (defun my-iflipb-timer-restart (arg)
      (if my-iflipb-timer-object
          ;; kill the running timer
          (cancel-timer my-iflipb-timer-object))
      (setq my-iflipb-timer-object
            (run-with-idle-timer my-iflipb-timeout nil 'my-iflipb-timer-cancel)))
    
    (defun my-iflipb-timer-expired ()
      (not my-iflipb-timer-object))
    
    ;; pretend this is the first flip if timer is expired even if this predicate is nil
    (advice-add 'iflipb-first-iflipb-buffer-switch-command
                :after-until 'my-iflipb-timer-expired)
    ;; reset timer after each flip
    (advice-add 'iflipb-next-buffer :after 'my-iflipb-timer-restart)
    (advice-add 'iflipb-previous-buffer :after 'my-iflipb-timer-restart))
  ;; finally, run the init function
  (__iflibp_init))

(el-get-bundle single-dired
  ;; reuse a single buffer for dired
  :url "http://www.emacswiki.org/emacs/download/joseph-single-dired.el")
(use-package joseph-single-dired
  :ensure nil ;; use el-get version
  :init
  (eval-after-load 'dired '(progn (require 'joseph-single-dired))))

(use-package keychain-environment
  ;; cygwin only - helper for ssh-agent
  :if (memq system-type '(cygwin windows-nt))
  :config
  (defun shims-first-in-path (orig-fun &rest args)
    "move /shims to beginning of path temporarily"
    (let ((old-path (getenv "PATH")))
      (unwind-protect
          (progn
            (setenv "PATH" (concat (getenv "CAROBY_DIR") "\\packages\\cygwin\\shims" path-separator old-path))
            (apply orig-fun args))
        ;; cleanup
        (setenv "PATH" old-path))))
  (advice-add 'keychain-refresh-environment :around #'shims-first-in-path))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :custom
  (keyfreq-file (concat emacs-savefile-dir ".emacs.keyfreq"))
  (keyfreq-file-lock (concat emacs-savefile-dir ".emacs.keyfreq.lock")))

(use-package linum
  :custom
  (linum-eager nil)  ;;else linum tries to synchronously update after every (next-line)

  :config
  ;;run linum-update-current when we're idle to cover gaps caused bylinum-eager='nil
  (run-with-idle-timer 2 't 'linum-update-current)
  (defun toggle-line-numbers ()
    (interactive)
    (require 'linum)
    ;; (face-spec-reset-face 'linum)
    ;; (set-face-attribute 'linum nil :inherit nil :background "#4f4f4f"
    ;;                     :foreground "slate gray")
    (let ((linum-format (concat "%" (format "%s" (ceiling (log10 (line-number-at-pos (point-max))))) "d")))
      (if linum-mode
          ;;turn off linum-mode
          (progn (linum-mode 0) (set-fringe-style nil))
        (progn
          ;;turn on linum-mode and setup fringe correctly
          (if (boundp 'git-gutter:enabled)
              (if (or global-git-gutter-mode git-gutter:enabled)
                  ;;special fringe style for git-gutter
                  (set-fringe-style '(8 . 10)))
            ;;fringe style without git-gutter:  thin right border for line numbers
            (set-fringe-style '(2 . 10)))
          ;;turn linum-on after fringe is set
          (linum-on))))))

(use-package loccur
  ;; press C-o to do an occur buffer during an interactive search
  :commands (run-occur-during-interactive-search)
  :bind (:map loccur-mode-map
        ("C-g" . (lambda ()
                   (interactive)
                   (if (bound-and-true-p loccur-current-search)
                       (if loccur-current-search (loccur-current))
                     (keyboard-quit))))
         :map isearch-mode-map
        ("C-o" .  loccur-current)
        ("C-g" . (lambda ()
                   (interactive)
                   (if (bound-and-true-p loccur-current-search)
                       (loccur-current)
                     (keyboard-quit))))))

(use-package magit
  :commands (magit-status)
  :custom
  (magit-last-seen-setup-instructions "1.4.0")
  (magit-bury-buffer-function #'magit-kill-buffers)
  (transient-levels-file (concat emacs-savefile-dir "transient/levels.el"))
  (transient-values-file (concat emacs-savefile-dir "transient/values.el"))
  (transient-history-file (concat emacs-savefile-dir "transient/history.el"))
  :config
  (defadvice magit-expand-git-file-name
      (before magit-expand-git-file-name-cygwin activate)
    "Handle Cygwin directory names such as /cygdrive/c/*
    by changing them to C:/*"
    (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
      (setq filename (concat (match-string 1 filename) ":/"
                             (match-string 2 filename)))))
  (defun magit-kill-buffers (param)
    "Restore window configuration and kill all Magit buffers."
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (defun un-cygwin-buffer-file-name ()
    (when (string-match "^\\([a-z]\\):/cygdrive/\\([a-z]\\)/\\(.*\\)" buffer-file-name)
      ;; assertion:  filename should look like "c:/cygwin/c/Users..." i.e. the drive is repeated
      (if (equal (match-string 1 buffer-file-name) (match-string 2 buffer-file-name)) (progn
        (set-visited-file-name
         (concat (match-string 1 buffer-file-name) ":/"
                 (match-string 3 buffer-file-name)) 't)))))
  (add-hook 'git-commit-mode-hook 'un-cygwin-buffer-file-name))

(el-get-bundle markdown-mode
  :url "https://github.com/jrblevin/markdown-mode.git")

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction .1)
  (minimap-recenter-type 'free)
  (minimap-update-delay 1.0)
  (minimap-dedicated-window 't)

  :config
   ;; override because this function is broken in minimap
  (defun minimap-sync-overlays () ))

(use-package move-text)

(use-package multiple-cursors
  :after region-bindings-mode
  :custom
  (mc/list-file (concat emacs-savefile-dir ".mc-lists.el"))
  :bind (:map region-bindings-mode-map
        ("a" . 'mc/mark-all-like-this)
        ("p" . 'mc/mark-previous-like-this)
        ("n" . 'mc/mark-next-like-this)
        ;; press "m" then press "right" to skip the next new cursor or "down" to accept it
        ("m" . 'mc/mark-more-like-this-extended)))

(el-get-bundle ntcmd
  :url "http://www.emacswiki.org/emacs/download/ntcmd.el")
(use-package ntcmd
  :ensure nil ;; use el-get version
  :init
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . ntcmd-mode))
  :config
  ;; use proper dos line endings for .bat and .cmd files
  (add-hook 'ntcmd-mode-hook (lambda () (setq buffer-file-coding-system 'utf-8-dos))))

(use-package paredit
  :config
  ;;preserve old definitions of paredit-*-word and override with preserve-kill-ring
  (add-hook 'paredit-mode-hook (lambda ()
    (if (not (fboundp 'paredit-forward-kill-word-orig))
        (fset 'paredit-forward-kill-word-orig (symbol-function 'paredit-forward-kill-word)))
    (if (not (fboundp 'paredit-backward-kill-word-orig))
        (fset 'paredit-backward-kill-word-orig (symbol-function 'paredit-backward-kill-word)))
    ;; TODO - use advice syntax here
    (defun paredit-forward-kill-word ()
      (interactive)
      (preserve-kill-ring 'paredit-forward-kill-word-orig))
    (defun paredit-backward-kill-word ()
      (interactive)
      (preserve-kill-ring 'paredit-backward-kill-word-orig)))))

(use-package parinfer
  :custom
  (parinfer-auto-switch-indent-mode 't)
  (parinfer-auto-switch-indent-mode-when-closing 't)
  (parinfer-extensions
   '(defaults                 ; should be included.
      paredit                 ; Introduce some paredit commands.
      smart-tab               ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank ))           ; Yank behavior depend on mode.
  :bind (("C-," . parinfer-toggle-mode)))

(use-package phi-search
  ;; compatible with multiple-cursors
  :after multiple-cursors)

(use-package projectile
  :init
  ;; due to f586312, must be before projectile-global-mode
  (setq projectile-known-projects-file (concat emacs-savefile-dir "projectile-bookmarks.eld"))

  :config
  (projectile-mode)
  (use-package ag)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package region-bindings-mode
  :config
  (region-bindings-mode-enable))

(use-package smartparens
  :delight
  :init
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  )

(use-package sr-speedbar
  :custom
  (sr-speedbar-right-side nil)
  (speedbar-use-images nil)
  (speedbar-show-unknown-files 't))

(el-get-bundle transpose-frame
  :url "https://github.com/emacsmirror/transpose-frame.git")
(use-package transpose-frame
  :ensure nil ;; use el-get version
  )

(el-get-bundle undo-tree-0.6.6
  :url "https://github.com/emacsmirror/undo-tree.git")
(use-package undo-tree
  :ensure nil ;; use el-get version
  :config
  (global-undo-tree-mode)
  ;;undo-tree-save-history fn doesn't like being interruped to ask about encodings
  (add-hook 'temp-buffer-setup-hook
            (lambda () (prefer-coding-system 'utf-8)))

  ;; TODO - port this to new advice syntax
  (defun undo-tree-basename (filename)
    "calculate a unique filename for a file's undo tree history"
    (let* ((hash (substring (secure-hash 'md5 filename) 0 10))
           (basename (file-name-nondirectory filename))
           (undo-tree-basename (concat basename "-" hash "-ut")))
      undo-tree-basename))

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value
          (let ((dir (file-name-directory ad-return-value)) ;;use the computed undo-tree save directory
                (buffer-filename (ad-get-arg 0)))
            (concat dir (undo-tree-basename buffer-filename)
                    ;; ".gz" ;;comment out this line to turn off compression
                    ;; TODO:  defcustom to switch this on and off
                    ))))
  ;; unfortunate interaction with auto-save-buffer
  (defadvice undo-tree-save-history (around undo-tree-save-history-no-message activate)
    (let ((auto-save-buffer-messaging nil))
      ad-do-it))

  :custom
  (undo-tree-history-directory-alist `((".*" . ,emacs-savefile-dir)))
  (undo-tree-auto-save-history 't)
  (undo-limit 78643200)
  (undo-outer-limit 104857600)
  (undo-strong-limit 157286400)
  (undo-tree-enable-undo-in-region nil)

  :hook ((write-file . undo-tree-save-history)
         (find-file-hook . undo-tree-load-history))

  :bind ("C-M-/" . undo-tree-visualize))

(el-get-bundle yascroll
  :url "https://github.com/m2ym/yascroll-el.git")
(use-package yascroll
  :ensure nil ;; use el-get version
  :custom
  (yascroll:scroll-bar 'right-fringe)
  (yascroll:delay-to-hide nil)
  :config
  (toggle-scroll-bar -1)
  (global-yascroll-bar-mode))

(use-package yaml-mode)

(use-package yasnippet
  :blackout yas-minor-mode
  :defer 3 ;; too many hooks to list in :commands
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package zenburn-theme)
