(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold 499999999) ;;speed up init by turning off gc
(setq debug-on-error 't)

;;  Path Variables
(defcustom emacs-savefile-dir "~/.local-emacs/auto-save-list/" 
  "Put all autosave files, save point, and undo-tree backups here")

(defcustom shared-externals
  (format "~/.local-emacs/externals-emacs-%d/" emacs-major-version)
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
(defcustom make-project-commands '()
  "Additional actions to be run in the externals directory")

(setq my-packages (append my-packages '(
    auto-complete autopair auctex paredit undo-tree ace-jump-mode
    idle-highlight-mode org move-text minimap loccur dired-k
    clojure-mode multiple-cursors unicode-fonts fill-column-indicator
    rainbow-delimiters htmlize ido-vertical-mode shm nlinum deferred
    scala-mode haskell-mode slime yasnippet paredit git-gutter-fringe
    solarized-theme zenburn-theme multi-web-mode base16-theme
    bash-completion ahk-mode git-timemachine ghc ;;intero
    py-yapf magit yaml-mode emmet-mode load-relative
    emmet-mode js2-mode js2-refactor rjsx-mode
    projectile flx-ido ag
    )
))

(setq git-projects (append git-projects `(
    ("Pymacs" "https://github.com/pinard/Pymacs.git")
    ("rope" "https://github.com/python-rope/rope.git")
    ("ropemacs" "https://github.com/python-rope/ropemacs.git")
    ("ropemode" "https://github.com/python-rope/ropemode.git")
    ("ensime" "https://github.com/aemoncannon/ensime.git")
    ("emacs-flymake" "https://github.com/illusori/emacs-flymake.git")
    ("emacs-flymake-cursor" "https://github.com/illusori/emacs-flymake-cursor.git")
    ("iflipb" "https://github.com/emacsmirror/iflipb.git")
    ("transpose-frame" "https://github.com/emacsmirror/transpose-frame.git")
    ("region-bindings-mode" "https://github.com/fgallina/region-bindings-mode.git")
    ("zenburn-emacs23" "https://github.com/dbrock/zenburn-el.git")
    ("yascroll" "https://github.com/m2ym/yascroll-el.git")
    ("markdown-mode" "http://jblevins.org/git/markdown-mode.git")
    ("auto-save-buffer" "https://github.com/thorrr/auto-save-buffer.git")
    ("python-goodies" "https://github.com/thorrr/python-goodies.git")
    ,(if (< emacs-major-version 24)
         '("python-emacs23" "-b emacs23 https://github.com/fgallina/python.el.git")
;         '("python-emacs24" "-b emacs-24 https://github.com/fgallina/python.el.git")
        )
    ("ac-python-async" "https://github.com/thorrr/ac-python-async.git")
    ("python-yapf" "https://github.com/galeo/python-yapf.el.git")
    ("parinfer-mode" "https://github.com/edpaget/parinfer-mode.git")
)))

(setq hg-projects (append hg-projects '(
    ("project-root" "https://bitbucket.org/piranha/project-root")
)))

(setq wget-projects (append wget-projects '(
    ("single-dired" "http://www.emacswiki.org/emacs/download/joseph-single-dired.el")
    ("color-theme-6.6.0" "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip")
    ("sr-speedbar" "http://www.emacswiki.org/emacs/download/sr-speedbar.el")
    ("ntcmd" "http://www.emacswiki.org/emacs/download/ntcmd.el")
)))


(setq make-project-commands (append make-project-commands (list
   (lambda () (if (not (file-exists-p (concat default-directory "Pymacs/build")))
    (shell-command-to-string 
     (concat "cd Pymacs && make" (if (eq system-type 'windows-nt) " && make install" "")))))
   (lambda () (if (not (file-exists-p "rope/build"))
     (shell-command-to-string
      (concat  "cd rope" (if (eq system-type 'windows-nt) "&& python setup.py install" "")))))
  (lambda () (if (not (file-exists-p "ropemacs/build"))
    (shell-command-to-string
     (concat  "cd ropemacs" (if (eq system-type 'windows-nt) "&& python setup.py install" "")))))
  (lambda () (if (not (file-exists-p "ropemode/build"))
    (shell-command-to-string
     (concat  "cd ropemode" (if (eq system-type 'windows-nt) "&& python setup.py install" "")))))
  ;; this command will only load color-theme on emacs < 24, load-theme supercedes it
  (lambda () (if (not (functionp 'load-theme)) 
    (shell-command-to-string (concat
     "cd color-theme-6.6.0 && unzip color-theme-6.6.0.zip && rm color-theme-6.6.0.zip"
     " && cd .. && mv color-theme-6.6.0 color-theme-tmp && cd color-theme-tmp"
     " && mv color-theme-6.6.0 .. && cd .. && rmdir color-theme-tmp"))))
  )))

(if load-file-name
    (setq emacs-config-root (file-name-directory load-file-name))
    ;;else hardcode .emacs.d for esup
    (setq emacs-config-root (expand-file-name "~/.emacs.d/")))
(load (concat emacs-config-root "packages.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all face customizations must come after load-theme
(if window-system 
    (if (>= emacs-major-version 24)
        (progn
          ;; 4th arg = nil:  actually load
          (load-theme 'zenburn 't nil)
          ;; (load-theme 'base16-mocha-dark 't nil)
          ;; these themes are switchable but not loaded by default
          ;; (load-theme 'solarized-light 't 'no-enable)
          ;; (load-theme 'solarized-dark 't 'no-enable)
          ;; (load-theme 'inkpot 't 'no-enable)
          )
      (progn
        (require 'color-theme)
        (add-to-list 'load-path (concat shared-externals "zenburn-emacs23"))
        (require 'zenburn)
        (color-theme-zenburn)
        )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emacs-savefile-dir (expand-file-name emacs-savefile-dir))
(load (concat emacs-config-root "config.el"))
(load (concat emacs-config-root "global-keybindings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Language Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat emacs-config-root "modes/python.el"))
(load (concat emacs-config-root "modes/scala.el"))
(load (concat emacs-config-root "modes/clojure.el"))
;; (load (concat emacs-config-root "modes/r-project.el"))
(load (concat emacs-config-root "modes/cpp.el"))
(load (concat emacs-config-root "modes/elisp.el"))
(load (concat emacs-config-root "modes/org-mode.el"))
(load (concat emacs-config-root "modes/markdown.el"))
(load (concat emacs-config-root "modes/haskell.el"))
(load (concat emacs-config-root "modes/javascript.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  General Interactive Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat emacs-config-root "commands.el"))

;;;;;;;;;;
;; New mode stuff
;;;;;;;;;
                             
(load (concat emacs-config-root "modes/undo-tree.el"))
(load (concat emacs-config-root "newstuff.el"))

(setq gc-cons-threshold gc-cons-threshold-orig) ;;end temporary rebind of gc-cons-threshold
(setq debug-on-error nil)
