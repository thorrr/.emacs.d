;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Path Variables
(defvar emacs-savefile-dir "~/.local-emacs/auto-save-list/")
(defvar shared-externals "~/.local-emacs/externals/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Projects not in elpa
(defvar git-projects '())
(defvar hg-projects '())
(defvar make-projects '())

(setq git-projects (append git-projects '(
    ("python" "https://github.com/fgallina/python.el.git")  ;;this should be part of emacs24 but I don't see it in my distro                       
    ("Pymacs" "https://github.com/pinard/Pymacs.git")
    ("ensime" "https://github.com/aemoncannon/ensime.git")
    ("emacs-flymake" "https://github.com/illusori/emacs-flymake.git")
    ("emacs-flymake-cursor" "https://github.com/illusori/emacs-flymake-cursor.git")
    ("iflipb" "https://github.com/emacsmirror/iflipb.git")
    ("transpose-frame" "https://github.com/emacsmirror/transpose-frame.git")
)))

(setq hg-projects (append hg-projects '(
    ("ropemacs" "https://bitbucket.org/agr/ropemacs")
    ("rope" "https://bitbucket.org/agr/rope")
    ("ropemode" "https://bitbucket.org/agr/ropemode")
    ("project-root" "https://bitbucket.org/piranha/project-root")
)))
;;  Misc commands to run in the externals subdirectory
(setq make-projects (append make-projects '(
  "cd Pymacs && make"
)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; Emacs Packaging
(require 'package)
(nconc package-archives '(
    ("melpa" . "http://melpa.milkbox.net/packages/"))
    )
(setq package-user-dir shared-externals)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '())
;; shared package list
(setq my-packages (append my-packages
             '(auto-complete autopair paredit undo-tree ace-jump-mode
               idle-highlight-mode ess hideshow org move-text
               clojure-mode clojure-test-mode clojurescript-mode 
               rainbow-delimiters
               scala-mode haskell-mode slime yasnippet
               solarized-theme zenburn-theme inkpot-theme
               anti-zenburn-theme xml)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; clean up dirnames
(setq emacs-savefile-dir (expand-file-name emacs-savefile-dir))
(setq shared-externals (expand-file-name shared-externals))

;; change default elpa directory

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Utility functions that all subsequent files can rely on
(setq emacs-config-root (file-name-directory load-file-name))
(load (concat emacs-config-root "elisp-utils.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp Artifacts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(unless (file-exists-p shared-externals)
  (make-directory shared-externals 't))
      
(defun git-clone (project-name project-url)
  (unless (file-exists-p (concat (expand-file-name default-directory) project-name))
    (let ((cmd (concat "git clone -q " project-url " " project-name)))
      (message (concat "Running git for project " project-name " in directory " default-directory))
      (shell-command-to-string cmd))))

(defun git-update (project-name project-url)
    (let ((cmd (concat "git pull -q"))
          (dir (concat default-directory project-name)))
      (if (file-exists-p dir)
          (let ((default-directory (expand-file-name dir)))
            (shell-command-to-string cmd)))))

(defun hg-clone (project-name project-url)
  (unless (file-exists-p (concat (expand-file-name default-directory) project-name))
    (let ((cmd (concat "hg clone -q " project-url " " project-name)))
      (message (concat "Running hg for project " project-name " in directory " default-directory))
      (shell-command-to-string cmd))))

(defun hg-update (project-name project-url)
    (let ((cmd (concat "hg pull -q"))
          (dir (concat default-directory project-name)))
      (if (file-exists-p dir)
          (let ((default-directory (expand-file-name dir)))
            (shell-command-to-string cmd)))))

(defun run-local-package-commands (list-of-commands)
  (let ((default-directory (expand-file-name shared-externals)))
    (mapcar 'shell-command-to-string list-of-commands)))

;; Install packages from git and hg
(let ((default-directory (expand-file-name shared-externals)))
  (mapcar (lambda (e) (git-clone (car e) (cadr e))) git-projects)
  (mapcar (lambda (e) (hg-clone (car e) (cadr e))) hg-projects)
)
;; Run commands in the externals directory
(run-local-package-commands make-projects)

;; Add externals to load path
(mapcar (lambda (e)
          (add-to-list 'load-path (expand-file-name (concat shared-externals (car e)))))
        (append git-projects hg-projects))

(add-to-list 'load-path (concat emacs-config-root "misc-packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(server-start)
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
(set-face-foreground 'show-paren-match-face "#94bff3")
(set-face-attribute 'show-paren-match-face nil 
                    :weight 'bold :underline nil :overline nil :slant 'normal :inherit nil)

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

;; save point location
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat emacs-savefile-dir "saved-places"))

;; set 'proficient coder' color scheme
(load-theme 'zenburn t nil)
;;(load-theme 'solarized-light t)
;;(load-theme 'anti-zenburn t)
;;(load-theme 'inkpot t)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `((".*" . ,emacs-savefile-dir)))
(setq undo-tree-auto-save-history nil) ;;this won't work until 24.3
(add-hook 'write-file-hooks 'undo-tree-save-history-hook)
(add-hook 'find-file-hook 'undo-tree-load-history-hook)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; disable slowwww git vc backend on Windows
(setq vc-handled-backends (quote (SVN)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End Global Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(global-set-key [S-f5] 'start-kbd-macro)
(global-set-key [S-f6] 'end-kbd-macro)
(global-set-key [S-f7] 'call-last-kbd-macro)
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

;; Package Config
(load (concat emacs-config-root "package-config.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Language Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat emacs-config-root "modes/python.el"))
(load (concat emacs-config-root "modes/scala.el"))
(load (concat emacs-config-root "modes/clojure.el"))
(load (concat emacs-config-root "modes/r-project.el"))
(load (concat emacs-config-root "modes/cpp.el"))
(load (concat emacs-config-root "modes/elisp.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  General Interactive Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  
(load (concat emacs-config-root "commands.el"))

;;;;;;;;;;
;; New mode stuff
;;;;;;;;;
                             
(load (concat emacs-config-root "modes/undo-tree.el"))
(load (concat emacs-config-root "newstuff.el"))
