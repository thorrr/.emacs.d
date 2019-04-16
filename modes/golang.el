(use-package go-mode
  :after (company flycheck auto-complete)
  :config
  (add-hook 'go-mode-hook 'dumb-jump-mode)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq fill-column 100)
              (keymaps-mode 0)
              (golang-minor-mode)
              (company-mode-on)
              (flymake-mode -1)
              (flycheck-mode 1)
              (auto-complete-mode -1)))
  (setq go-packages-function 'go-packages-go-list))

(use-package company-ycmd
  :after (:all company)
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :config
  (flycheck-ycmd-setup))

(defun moq ()
  (interactive)
  (let ((interface (word-at-point))
        (test-file (concat (downcase (word-at-point)) "_test.go")))
    (shell-command
     (concat "moq -out " test-file " . " interface))
    (find-file test-file)))

(defun go-coverage-here ()
  (interactive)
  (shell-command "go test . -coverprofile=cover.out")
  (go-coverage "cover.out")
  (rotate:even-horizontal))


;; can't figure out how to call-interactively xref-find-definitions (timing issue?)  so
;; define a new minor mode and put special keybindings there

(defvar golang-minor-mode-map (make-sparse-keymap)
  "Keymap while golang-temp-mode is active.")
(set-keymap-parent golang-minor-mode-map keymaps-mode-map)

(define-minor-mode golang-minor-mode
  "A temporary minor mode to be activated only in golang buffers."
  :init-value nil ;;docs say this should be nil in almost all situations
  :lighter "golang-minor-mode" ;; make this mode look like keymaps-mode
  :keymap golang-minor-mode-map)
(provide 'golang-minor-mode)

(define-key golang-minor-mode-map (kbd "M-.") 'xref-find-definitions)
(define-key golang-minor-mode-map (kbd "M-,") 'xref-pop-marker-stack)

