;;  Path Variables
(defcustom emacs-savefile-dir "~/.local-emacs/auto-save-list/" 
  "Put all autosave files, save point, and undo-tree backups here")

(defcustom shared-externals "~/.local-emacs/externals/"
  "Download all emacs packages here.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom my-packages '()
  "List of packages for the local machine")
(defcustom git-projects '()
  "List of git project URLs for the local machine")
(defcustom hg-projects '()
  "List of mercurial project URLs for the local machine")
(defcustom wget-projects '()
  "List of projects to be fetched via wget for the local machine")
(defcustom make-projects '()
  "Additional actions to be run in the externals directory")

(setq my-packages (append my-packages '(
    auto-complete autopair auctex paredit undo-tree ace-jump-mode
    idle-highlight-mode ess org move-text minimap
    clojure-mode clojure-test-mode clojurescript-mode 
    rainbow-delimiters
    scala-mode haskell-mode slime yasnippet
;;  inkpot-theme solarized-theme anti-zenburn-theme
    zenburn-theme)
))

(setq git-projects (append git-projects '(
    ("Pymacs" "https://github.com/pinard/Pymacs.git")
    ("ensime" "https://github.com/aemoncannon/ensime.git")
    ("emacs-flymake" "https://github.com/illusori/emacs-flymake.git")
    ("emacs-flymake-cursor" "https://github.com/illusori/emacs-flymake-cursor.git")
    ("iflipb" "https://github.com/emacsmirror/iflipb.git")
    ("transpose-frame" "https://github.com/emacsmirror/transpose-frame.git")
    ("visible-mark" "https://github.com/emacsmirror/visible-mark.git")
    ("region-bindings-mode" "https://github.com/fgallina/region-bindings-mode.git")
    ("multiple-cursors" "https://github.com/emacsmirror/multiple-cursors.git")
    ("zenburn-emacs23" "https://github.com/dbrock/zenburn-el.git")
    ("yascroll" "https://github.com/m2ym/yascroll-el.git")
    ("markdown-mode" "http://jblevins.org/git/markdown-mode.git")
    ("auto-save-buffer" "https://github.com/thorrr/auto-save-buffer.git")
)))

(setq git-projects (append git-projects
    (if (< emacs-major-version 24)
      '(("python-emacs23" "-b emacs23 https://github.com/fgallina/python.el.git"))
      '(("python-emacs24" "-b emacs-24 https://github.com/fgallina/python.el.git"))
        )))

(setq hg-projects (append hg-projects '(
    ("ropemacs" "https://bitbucket.org/agr/ropemacs")
    ("rope" "https://bitbucket.org/agr/rope")
    ("ropemode" "https://bitbucket.org/agr/ropemode")
    ("project-root" "https://bitbucket.org/piranha/project-root")
)))

(setq wget-projects (append wget-projects '(
    ("ac-python" "http://chrispoole.com/downloads/ac-python.el")
    ("single-dired" "http://www.emacswiki.org/emacs/download/joseph-single-dired.el")
    ("color-theme-6.6.0" "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip")
    ("sr-speedbar" "http://www.emacswiki.org/emacs/download/sr-speedbar.el")
)))

(setq make-projects (append make-projects '(
  ;; the "make install" part seems to contaminate your site packages and
  ;; seems to not be necessary if you add the Pymacs directory to the PYTHONPATH                                          
  ;;  "cd Pymacs && make && make install"
  "cd Pymacs && make"
  "cd color-theme-6.6.0 && unzip color-theme-6.6.0.zip && rm color-theme-6.6.0.zip && cd .. &&
    mv color-theme-6.6.0 color-theme-tmp && cd color-theme-tmp && mv color-theme-6.6.0 .. &&
    cd .. && rmdir color-theme-tmp"
)))
(setq emacs-config-root (file-name-directory load-file-name))
(load (concat emacs-config-root "packages.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean up dirnames
(setq emacs-savefile-dir (expand-file-name emacs-savefile-dir))

;; make sure scratch buffer tries to open files in home
(with-current-buffer "*scratch*"
  (setq default-directory "~/"))

;;keep server file out of our pristine .emacs.d directory
(if (eq system-type 'windows-nt)
    (setq server-auth-dir (concat (getenv "APPDATA") "\\.emacs.d\\server")))
;;server mode is good
;;(load "server")
;;(unless (server-running-p) (server-start))

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

(global-subword-mode)
(setq column-number-mode t)
(require 'uniquify) ;;globally unique buffer names
(global-font-lock-mode t)
(tool-bar-mode -1)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(require 'sr-speedbar)
(add-hook 'speedbar-mode-hook (lambda () (save-current-buffer (set-buffer "*SPEEDBAR*") (visual-line-mode)))) ;;word wrapping for deep directories

(setq speedbar-use-images nil)
;;show-paren customizations
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; ignore ^M in mixed dos/unix files
(add-hook 'find-file-hook (lambda () (if (fboundp 'remove-dos-eol) (remove-dos-eol)))) ;; protect ourselves if there's a .emacs file problem

;; Save all backup files in this directory (no ~files lying around) 
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir 't))
(setq auto-save-list-file-prefix (concat emacs-savefile-dir ".saves-"))
(setq backup-directory-alist `((".*" . ,emacs-savefile-dir)))
(setq auto-save-file-name-transforms
          `((".*" ,emacs-savefile-dir t)))

;; put the .recentf file in the autosave directory
(setq recentf-save-file (concat emacs-savefile-dir ".recentf"))
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

;; all face customizations must come after load-theme
(if (>= emacs-major-version 24) (progn
   (load-theme 'zenburn t nil)
   ;;(load-theme 'solarized-light t)
   ;;(load-theme 'anti-zenburn t)
   ;;(load-theme 'inkpot t)
   ) (progn
       (require 'color-theme)
       (add-to-list 'load-path (concat shared-externals "zenburn-emacs23"))
       (require 'zenburn)
       (color-theme-zenburn)
))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End Global Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package Config
(load (concat emacs-config-root "config.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Language Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat emacs-config-root "modes/python-goodies.el"))
(load (concat emacs-config-root "modes/python.el"))
(load (concat emacs-config-root "modes/scala.el"))
(load (concat emacs-config-root "modes/clojure.el"))
(load (concat emacs-config-root "modes/r-project.el"))
(load (concat emacs-config-root "modes/cpp.el"))
(load (concat emacs-config-root "modes/elisp.el"))
(load (concat emacs-config-root "modes/org-mode.el"))
(load (concat emacs-config-root "modes/markdown.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  General Interactive Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  
(load (concat emacs-config-root "commands.el"))

;;;;;;;;;;
;; New mode stuff
;;;;;;;;;
                             
(load (concat emacs-config-root "modes/undo-tree.el"))
(load (concat emacs-config-root "newstuff.el"))
