;;misc
(setq delete-by-moving-to-trash t)

;;new visible mark stuff
(require 'visible-mark)
(global-visible-mark-mode t)
(setq visible-mark-max 2)

;; customize visible faces
(face-spec-reset-face 'visible-mark-face)
(face-spec-reset-face 'visible-mark-non-trailing-face0)
(set-face-attribute 'visible-mark-face nil :inherit nil :background "#656555")
(set-face-attribute 'visible-mark-non-trailing-face0 nil :inherit nil :underline 't  :strike-through nil :background nil
                    :overline 't :foreground "#76487A")



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

(setq project-root-storage-file (concat emacs-savefile-dir ".project-roots"))
(setq project-roots
      '(("m4-python" :root-contains-files ("atg" "rpy2"))
        ("m4-sbt" :root-contains-files ("project/build/M4Project.scala"))
        ("leiningen" :root-contains-files ("project.clj"))
        ))

;; incomplete - add a hook to spit it into the current buffer, not the comint buffer
(defun comint-insert-history ()
  (interactive)
  (cond ((or (null comint-input-ring) (ring-empty-p comint-input-ring)) nil)
	(t (let* ((ring comint-input-ring)
                  (index (ring-length ring)))
	     (while (> index 0)
	       (setq index (1- index))
	       (insert (ring-ref ring index) comint-input-ring-separator))))))


(defun ruthlessly-kill-word (arg)
  "Delete subwords but don't put them into the kill ring"
  (interactive "p")
  (let ((old-kill-ring kill-ring))
    (subword-backward-kill arg)
    (setq kill-ring old-kill-ring))) ;;this still isn't right
;;    (setq kill-ring (cdr kill-ring)))
;;(global-set-key (kbd "M-DEL") 'ruthlessly-kill-word)
(global-set-key (kbd "M-DEL") 'subword-backward-kill)


(defun my-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark 't))  ;; turn off transient mode when switching
(global-set-key (kbd "C-x C-x") 'my-exchange-point-and-mark)

;; reuse a single buffer for dired
(require 'joseph-single-dired)

;; press C-o to do an occur buffer during an interactive search
(define-key isearch-mode-map (kbd "C-o") 'run-occur-during-interactive-search)

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


;; let's try line numbers
(defun toggle-line-numbers ()
  (interactive)
  (require 'linum)
  (face-spec-reset-face 'linum)
  (set-face-attribute 'linum nil :inherit nil :background "#4f4f4f"
                      :foreground "slate gray")
  (let ((linum-format (concat "%" (format "%s" (ceiling (log10 (line-number-at-pos (point-max))))) "d")))
    (set-fringe-style '(2 . 10)) ;;TODO - save fringe style
    (if linum-mode (linum-mode 0) (linum-on))))
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
(add-hook 'speedbar-mode-hook (lambda () (save-current-buffer (set-buffer "*SPEEDBAR*") (visual-line-mode)))) ;;word wrapping for deep directories

(setq speedbar-use-images nil)

  (setq magit-last-seen-setup-instructions "1.4.0")
;; (require 'git-gutter-fringe)
;; (global-git-gutter-mode +1)
;; (git-gutter:linum-setup)
;; otherwise the second character bleeds into the first digit of the line number
;; (setq git-gutter:added-sign "+")
;; (setq git-gutter:modified-sign " ")
;; (setq git-gutter:deleted-sign "-")
;; auto-save-buffer calls write-file which doesn't naturally call the git-gutter refresh fn
;; (defadvice write-file (after write-file-git-gutter-mode activate) (git-gutter))

