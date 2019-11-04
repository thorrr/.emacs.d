(use-package go-mode
  :mode "\\.go\\'"
  :after (company flycheck auto-complete)
  :config
  (add-hook 'go-mode-hook 'dumb-jump-mode)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq fill-column 100)
              (company-mode 't)
              (flycheck-mode 1)))
  (setq go-packages-function 'go-packages-go-list)
  ;; don't use dumb-jump
  (keymaps-mode-override go-mode
                       (kbd "M-.") 'xref-find-definitions
                       (kbd "M-,") 'xref-pop-marker-stack))

(use-package company-ycmd
  :after (company)
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :after (flycheck)
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
