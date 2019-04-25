;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin configuration
;;
;; Customizations of the base emacs installation go here.  Put mode-specific customiztion
;; in modes/<mode>.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sanitize user customized var
(setq emacs-savefile-dir (expand-file-name emacs-savefile-dir))

;; shift arrow buffer navigation
(when (fboundp 'windmove-default-keybindings)
       (windmove-default-keybindings))

;; occur is cool
;; press C-o to do an occur buffer cduring an interactive search
(define-key isearch-mode-map (kbd "C-o") 'run-occur-during-interactive-search)

;; use unix line endings on everything
(setq-default buffer-file-coding-system 'utf-8-unix)
;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; disable slowwww git vc backend on Windows
(setq vc-handled-backends (quote (SVN)))

;; make sure scratch buffer tries to open files in home
(with-current-buffer "*scratch*"
  (setq default-directory "~/"))

(setq tab-always-indent 't) ;; indent, don't tab or autocomplete.  Turns off default TAB->complete

;;keep server file out of our pristine .emacs.d directory
;; (setq server-auth-dir (expand-file-name "~/.local-emacs/server"))
;; (require 'server)
;; (or (server-running-p)
;;     (server-start))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(use-package recentf
  :ensure nil ;; it's a builtin
  :commands recentf-open-files
  :custom
  (recentf-save-file (concat emacs-savefile-dir ".recentf"))
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 60)
  :config
  (recentf-mode 1))
;; (require 'recentf)

;; hide/show mode
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
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

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; eshell customizations
;;   fix colors
(require 'ansi-color)
(setq eshell-scroll-to-bottom-on-input t)
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
(setq ansi-color-map (ansi-color-make-color-map))
;;   make sure eshell is cleanly machine local
(setq eshell-directory-name (concat emacs-savefile-dir  "eshell/"))
(setq eshell-aliases-file (concat emacs-savefile-dir  "eshell/alias"))
(add-hook 'eshell-mode-hook (lambda ()
    (local-set-key (kbd "M-i") (lambda () (interactive) (end-of-buffer)))
    ))
;;   add this line to the bottom of .bash_aliases to parse your bash
;;   aliases into an eshell "alias" file:
;;   alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" > ~/.local-emacs/auto-save-list/eshell/alias

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

;; Save all backup files in this directory (no ~files lying around) 
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir 't))
(setq auto-save-list-file-prefix (concat emacs-savefile-dir ".saves-"))
(setq backup-directory-alist `((".*" . ,emacs-savefile-dir)))
(setq auto-save-file-name-transforms
          `((".*" ,emacs-savefile-dir t)))

;; save point location
(if (fboundp 'save-place-mode)
  (setq save-place-file (concat emacs-savefile-dir "saved-places"))
  (save-place-mode +1)
  (setq-default save-place t))

;; Enable versioning with default values
(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files t
   version-control t)       ; use versioned backups

;;el-doc for lisp languages
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;;shell improvements for windows
(if (eq system-type 'windows-nt) (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; bash-completion-dynamic-complete plus tweaks
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package bash-completion)                               
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



;; electric-indent-mode is automatic in emacs 24.4+.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; TODO - use advice syntax
(defun emacs-session-filename (session-id)
  "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use `no-littering'
directories."
  (expand-file-name session-id emacs-savefile-dir))

;; enable "undo" of window shape changes
(winner-mode 1)

;;misc
(setq delete-by-moving-to-trash t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New stuff
;;
;;   Try out new stuff below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; glasses mode
(require 'glasses)
(setq glasses-original-separator "") ;;prevent separators from being accidentally saved
(setq glasses-separator "ˈ")
(setq glasses-separator "")
(setq glasses-separate-parentheses-p nil)
(setq glasses-face 'bold)
(setq glasses-face 'bold-italic)
(add-hook 'python-mode-hook (lambda () (glasses-mode)))
(add-hook 'inferior-python-mode-hook (lambda () (glasses-mode)))
(add-hook 'ess-mode-hook (lambda () (glasses-mode)))
(add-hook 'inferior-ess-mode-hook (lambda () (glasses-mode)))

;;Comint tweak
(setq-default comint-input-ring-size 5000)

(setq-default indent-tabs-mode nil)

;; wrapper for ssh on nt using fakecygpty
(if (eq system-type 'windows-nt)
    (defun ssh (hostname port &optional flags)
      "Start an SSH session in a shell window. 
  C-u <port number> M-x ssh
to specify a custom port"
      (interactive "MSSH to host: \nP")
      (let* ((buf (concat "*SSH:" hostname "*"))
             (port (if port port 22))
             (port-flag (concat "-p " (format "%d " port))))
        (if (and (get-buffer buf) (get-buffer-process buf))
            (switch-to-buffer-other-window buf)
          (async-shell-command (concat "fakecygpty ssh " port-flag flags (when flags " ") hostname) buf)))))

;; tramp customizations 
(setq tramp-persistency-file-name "~/.local-emacs/tramp")
(if (eq system-type 'windows-nt)
    (progn
      ;; add a function that asks for a password
      (defun ssh-add-process-filter (process string)
        (save-match-data
          (if (string-match ":\\s *\\'" string)
              (process-send-string process (concat (read-passwd string) "\n"))
            (message "%s" string))))
      (defun ssh-add (key-file)
        "Run ssh-add to add a key to the running SSH agent. Let
        Emacs prompt for the passphrase."
        (interactive "fAdd key: \n")
        (let ((process-connection-type t)
              process)
          (unwind-protect
              (progn
                (setq process (start-process "ssh-add" nil
                                             "ssh-add" (expand-file-name key-file)))
                (set-process-filter process 'ssh-add-process-filter)
                (while (accept-process-output process)))
            (if (eq (process-status process) 'run)
                (kill-process process)))))

      ;; must use fakecygpty so ssh process doesn't hang
      (eval-after-load "tramp"
        '(progn
           (add-to-list 'tramp-methods
                        (mapcar
                         (lambda (x)
                           (cond
                            ((equal x "sshx") "cygssh")
                            ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
                            (t x)))
                         (assoc "sshx" tramp-methods)))
           (setq tramp-default-method "cygssh")))))

(setq tramp-backup-directory-alist backup-directory-alist)

;; ffap will aggressively try and open files if there's a url.  Don't let it.
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil)         ; disable URL features in ffap

;; customize isearch behavior
(defun isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'isearch-with-region)
