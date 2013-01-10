;;misc
(setq delete-by-moving-to-trash t)

;;new visible mark stuff
(require 'visible-mark)
(global-visible-mark-mode t)
(setq visible-mark-max 4)

;; customize visible faces
(face-spec-reset-face 'visible-mark-face)
(face-spec-reset-face 'visible-mark-non-trailing-face0)
(set-face-attribute 'visible-mark-face nil :inherit nil :background "HotPink4")
(set-face-attribute 'visible-mark-non-trailing-face0 nil :inherit nil :underline 't :strike-through 't :background nil
                    :overline 't :foreground "LightPink")



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

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-M-RET-may-split-line nil)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(require 'autopair)
(setq autopair-autowrap t)
;;(autopair-global-mode) ;; enable autopair in all buffers

;; autopair mode fixes.
(add-hook 'python-mode-hook  ;;make python work with triple quotes
           #'(lambda () (setq autopair-handle-action-fns (list #'autopair-default-handle-action
                           #'autopair-dont-if-point-non-whitespace
                           #'autopair-python-triple-quote-action))))

(defun autopair-dont-if-point-non-whitespace (action pair pos-before)
  (if (or (eq 'opening action) (eq 'insert-quote action) (eq 'paired-delimiter action))
      (let ((delete? (save-excursion
         ;;move forward past the paired element
         (goto-char (+ (point) 1))
         (let* ((eol? (eq (point) (line-end-position)))
                (next-whitespace (save-excursion (search-forward " " (point-max) t) (point)))
                (next-char-is-whitespace? (eq next-whitespace (+ (point) 1)))
                (delete? (not (or eol? next-char-is-whitespace?))))
           delete?))))
        (if delete? (delete-char 1) 't))
    't))
           
(add-hook 'python-mode-hook (lambda () (autopair-mode)))

;;Comint tweak
(setq-default comint-input-ring-size 5000)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


;; real-auto-save allows mode-specific auto saving
(require 'real-auto-save)
(setq real-auto-save-interval 5)
(add-hook 'find-file-hooks 'turn-on-real-auto-save) ;; real-auto-save every single file
;;(add-hook 'emacs-lisp-mode-hook 'turn-on-real-auto-save)  ;; mode specific auto-save

;; turn off the "file saved message" because it gets annoying
(require 'turn-off-messaging)
(defadvice real-auto-save (around real-auto-save-no-message activate)
  (setq messaging-on nil) ad-do-it (setq messaging-on t))

(setq-default indent-tabs-mode nil)

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

;; minimap keybinding
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
      (minimap-create)
    (minimap-kill)))

(global-set-key (kbd "C-c m") 'minimap-toggle)
