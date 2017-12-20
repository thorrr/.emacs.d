;;misc
(setq delete-by-moving-to-trash t)

;; glasses mode
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

(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; (require 'autopair)
;; (setq autopair-autowrap t)
;;(autopair-global-mode) ;; enable autopair in all buffers

;; autopair mode fixes.
;; (add-hook 'python-mode-hook  ;;make python work with triple quotes
;;            #'(lambda () (setq autopair-handle-action-fns (list #'autopair-default-handle-action
;;                            #'autopair-dont-if-point-non-whitespace
;;                            #'autopair-python-triple-quote-action))))

;; (defun autopair-dont-if-point-non-whitespace (action pair pos-before)
;;   (if (or (eq 'opening action) (eq 'insert-quote action) (eq 'paired-delimiter action))
;;       (let ((delete? (save-excursion
;;          ;;move forward past the paired element
;;          (goto-char (+ (point) 1))
;;          (let* ((eol? (eq (point) (line-end-position)))
;;                 (next-whitespace (save-excursion (search-forward " " (point-max) t) (point)))
;;                 (next-char-is-whitespace? (eq next-whitespace (+ (point) 1)))
;;                 (delete? (not (or eol? next-char-is-whitespace?))))
;;            delete?))))
;;         (if delete? (delete-char 1) 't))
;;     't))
           
;; (add-hook 'python-mode-hook (lambda () (autopair-mode)))

;;Comint tweak
(setq-default comint-input-ring-size 5000)

;; real-auto-save allows mode-specific auto saving
(require 'auto-save-buffer)
(setq auto-save-buffer-interval 3)
(setq auto-save-buffer-only-after-regular-save 't)
(setq auto-save-buffer-messaging nil)
(add-hook 'find-file-hooks 'turn-on-auto-save-buffer) ;; real-auto-save every single file
;;(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-save-buffer)  ;; mode specific auto-save

(setq-default indent-tabs-mode nil)

;; incomplete - add a hook to spit it into the current buffer, not the comint buffer
(defun comint-insert-history ()
  (interactive)
  (cond ((or (null comint-input-ring) (ring-empty-p comint-input-ring)) nil)
	(t (let* ((ring comint-input-ring)
                  (index (ring-length ring)))
	     (while (> index 0)
	       (setq index (1- index))
	       (insert (ring-ref ring index) comint-input-ring-separator))))))


(defun my-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark 't))  ;; turn off transient mode when switching
(global-set-key (kbd "C-x C-x") 'my-exchange-point-and-mark)

;; reuse a single buffer for dired
(require 'joseph-single-dired)

(require 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)
;; press C-o to do an occur buffer during an interactive search
(define-key isearch-mode-map (kbd "C-o") 'loccur-current)
(define-key isearch-mode-map (kbd "C-g") (lambda () (interactive) (loccur-current) (keyboard-quit)))
(define-key loccur-mode-map (kbd "C-g") (lambda () (interactive) (loccur-current) (keyboard-quit)))

(require 'minimap)
(setq minimap-window-location 'right)
(setq minimap-width-fraction .1)
(setq minimap-recenter-type 'free)
(setq minimap-update-delay 1.0)
(setq minimap-dedicated-window 't)
(defun minimap-sync-overlays () ) ;; override because this function is broken in minimap
(global-set-key (kbd "C-c m") 'minimap-toggle)

;;fringe experiments
(toggle-scroll-bar -1)
(require 'yascroll)
(setq yascroll:scroll-bar 'right-fringe)
(setq yascroll:delay-to-hide nil)
(global-yascroll-bar-mode)

;; line numbers
(require 'linum)
(setq linum-eager nil) ;;else linum tries to synchronously update after every (next-line)

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
        (linum-on)))))
(global-set-key (kbd "<f2>") 'toggle-line-numbers)

;; multiple cursors is fcking awesome
(require 'multiple-cursors)
(setq mc/list-file (concat emacs-savefile-dir ".mc-lists.el"))
(global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-S-<next>") 'mc/mark-all-like-this)
(global-set-key (kbd "M-S-<prior>") 'mc/mark-all-like-this)
;;turn off prompt for autopair function
;; (nconc mc/cmds-to-run-for-all `(autopair-insert-opening autopair-skip-close-maybe autopair-insert-or-skip-quote))

;; this is awesome - commands that work when you've selected something
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
;; press "m" then press "right" to skip the next new cursor or "down" to accept it
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

;; is speedbar good?
(require 'sr-speedbar)
;;(add-hook 'speedbar-mode-hook (lambda () (save-current-buffer (set-buffer "*SPEEDBAR*") (visual-line-mode)))) ;;word wrapping for deep directories
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files 't)

(setq magit-last-seen-setup-instructions "1.4.0")
(defadvice magit-expand-git-file-name
  (before magit-expand-git-file-name-cygwin activate)
  "Handle Cygwin directory names such as /cygdrive/c/*
by changing them to C:/*"
  (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
    (setq filename (concat (match-string 1 filename) ":/"
                           (match-string 2 filename)))))

(defun un-cygwin-buffer-file-name ()
  (when (string-match "^\\([a-z]\\):/cygdrive/\\([a-z]\\)/\\(.*\\)" buffer-file-name)
    ;; assertion:  filename should look like "c:/cygwin/c/Users..." i.e. the drive is repeated
    (if (equal (match-string 1 buffer-file-name) (match-string 2 buffer-file-name)) (progn
      (set-visited-file-name
            (concat (match-string 1 buffer-file-name) ":/"
                    (match-string 3 buffer-file-name)) 't)))))
(add-hook 'git-commit-mode-hook 'un-cygwin-buffer-file-name)

(require 'git-gutter-fringe)
(global-git-gutter-mode +1)
(setq git-gutter:handled-backends '(git svn))
;;auto-save-buffer calls write-file which doesn't naturally call the git-gutter refresh fn
(defadvice write-file (after write-file-git-gutter-mode activate) (git-gutter))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

;; fill column indicator mode - pretty good so far
(if (>= emacs-major-version 25) (progn
  (require 'fill-column-indicator)
  
  (setq fci-always-use-textual-rule 't)
  ;; turn on only for non-special buffers
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)   ;; enable globally
))

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
(setq fci-handle-truncate-lines nil)
(setq fci-rule-color "gray35")

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

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(require 'dired-k)
(add-hook 'dired-initial-position-hook 'dired-k)


;; tramp customizations 
(setq tramp-persistency-file-name "~/.local-emacs/tramp")
(if (eq system-type 'windows-nt)
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
         (setq tramp-default-method "cygssh"))))
  
(setq tramp-backup-directory-alist backup-directory-alist)
