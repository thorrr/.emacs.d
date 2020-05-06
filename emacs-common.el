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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init packaging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make the externals directory
(setq shared-externals (expand-file-name shared-externals))
(unless (file-exists-p shared-externals)
  (make-directory shared-externals 't))

;; change default elpa directory and load packages
(setq package-user-dir shared-externals)
(setq el-get-dir shared-externals)
(package-initialize)

;; first things first - download el-get and use-package
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (package-install 'use-package)
  (require 'el-get)
  (require 'use-package))
;; so we don't have to do :ensure t in every use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)

;; workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(if (or
     (version< emacs-version "25.2")
     (string-equal system-type "windows-nt"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; uncomment to benchmark emacs startup
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; also test on command line with:
;; $ time emacs -l ~/.emacs -batch --eval '(message "Hello, world!")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End init packaging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package and configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (concat emacs-config-root "packages.el"))
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
