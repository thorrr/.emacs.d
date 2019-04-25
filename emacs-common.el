(setq gc-cons-threshold 64000000)  ;; speed up init by turning off gc
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; we don't need this set during loading of .el files
(defvar file-name-handler-alist-real file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq file-name-handler-alist file-name-handler-alist-real)))

(setq debug-on-error 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom emacs-savefile-dir "~/.local-emacs/auto-save-list/" 
  "Put all autosave files, save point, and undo-tree backups here")

(defcustom shared-externals
  (format "~/.local-emacs/externals-emacs-%d/" emacs-major-version)
  "Download all emacs packages here.")

(if load-file-name
    (setq emacs-config-root (file-name-directory load-file-name))
    ;;else hardcode .emacs.d for esup
    (setq emacs-config-root (expand-file-name "~/.emacs.d/")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load external packages first.  This has to be first so use-package is defined for
;; everyone.
(load (concat emacs-config-root "packages.el"))
;; all face customizations must come after load-theme
(load-theme 'zenburn 't nil)

(load (concat emacs-config-root "config.el"))
;; All interactive commands
(load (concat emacs-config-root "commands.el"))
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
(load (concat emacs-config-root "modes/php.el"))
(load (concat emacs-config-root "modes/golang.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)
