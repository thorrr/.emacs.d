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
    idle-highlight-mode org move-text minimap
    clojure-mode clojure-test-mode
    rainbow-delimiters htmlize ido-vertical-mode
    scala-mode haskell-mode slime yasnippet paredit git-gutter-fringe
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
    ("python-goodies" "https://github.com/thorrr/python-goodies.git")
    ("autohotkey-syntax" "https://github.com/ahkscript/AutoHotkey-Editors.git")
    ("ob-ipython" "https://github.com/thorrr/ob-ipython.git")
)))

(setq git-projects (append git-projects
    (if (< emacs-major-version 24)
      '(("python-emacs23" "-b emacs23 https://github.com/fgallina/python.el.git"))
;      '(("python-emacs24" "-b emacs-24 https://github.com/fgallina/python.el.git"))
        )))

(setq hg-projects (append hg-projects '(
    ("rope" "https://bitbucket.org/jbell9999/rope")
    ("ropemacs" "https://bitbucket.org/agr/ropemacs")
    ("ropemode" "https://bitbucket.org/agr/ropemode")
    ("project-root" "https://bitbucket.org/piranha/project-root")
)))

(setq wget-projects (append wget-projects '(
    ("ac-python" "http://chrispoole.com/downloads/ac-python.el")
    ("single-dired" "http://www.emacswiki.org/emacs/download/joseph-single-dired.el")
    ("color-theme-6.6.0" "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip")
    ("sr-speedbar" "http://www.emacswiki.org/emacs/download/sr-speedbar.el")
    ("ntcmd" "http://www.emacswiki.org/emacs/download/ntcmd.el")
    ("cygwin-mount" "http://www.emacswiki.org/emacs/download/cygwin-mount.el")
    ("autohotkey-mode" "http://www.robf.de/Hacking/elisp/ahk-mode.el") 
)))

(setq make-projects (append make-projects (list
  (concat "cd Pymacs && make" (if (eq system-type 'windows-nt) "&& make install" ""))
  (concat  "cd rope" (if (eq system-type 'windows-nt) "&& python setup.py install" ""))
  (concat  "cd ropemacs" (if (eq system-type 'windows-nt) "&& python setup.py install" ""))
  (concat  "cd ropemode" (if (eq system-type 'windows-nt) "&& python setup.py install" ""))
  "cd color-theme-6.6.0 && unzip color-theme-6.6.0.zip && rm color-theme-6.6.0.zip && cd .. &&
    mv color-theme-6.6.0 color-theme-tmp && cd color-theme-tmp && mv color-theme-6.6.0 .. &&
    cd .. && rmdir color-theme-tmp"
)))
(setq emacs-config-root (file-name-directory load-file-name))
(load (concat emacs-config-root "packages.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all face customizations must come after load-theme
(if window-system 
    (if (>= emacs-major-version 24)
        (progn
          (load-theme 'zenburn t nil)
          ;;(load-theme 'solarized-light t)
          ;;(load-theme 'anti-zenburn t)
          ;;(load-theme 'inkpot t)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  General Interactive Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat emacs-config-root "commands.el"))

;;;;;;;;;;
;; New mode stuff
;;;;;;;;;
                             
(load (concat emacs-config-root "modes/undo-tree.el"))
(load (concat emacs-config-root "newstuff.el"))
(require 'ob-ipython)
