(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold 499999999) ;;speed up init by turning off gc
(setq debug-on-error 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom emacs-savefile-dir "~/.local-emacs/auto-save-list/" 
  "Put all autosave files, save point, and undo-tree backups here")

(defcustom shared-externals
  (format "~/.local-emacs/externals-emacs-%d/" emacs-major-version)
  "Download all emacs packages here.")

(defcustom my-packages '()
  "List of packages for the local machine")

(if load-file-name
    (setq emacs-config-root (file-name-directory load-file-name))
    ;;else hardcode .emacs.d for esup
    (setq emacs-config-root (expand-file-name "~/.emacs.d/")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - make the order not matter
;; load external packages first
(load (concat emacs-config-root "packages.el"))
;; all face customizations must come after load-theme
(load-theme 'zenburn 't nil)

(load (concat emacs-config-root "config.el"))
(load (concat emacs-config-root "global-keybindings.el"))
;; All interactive commands
(load (concat emacs-config-root "commands.el"))

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
(load (concat emacs-config-root "modes/php.el"))
(load (concat emacs-config-root "modes/golang.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold gc-cons-threshold-orig) ;;end temporary rebind of gc-cons-threshold
(setq debug-on-error nil)
