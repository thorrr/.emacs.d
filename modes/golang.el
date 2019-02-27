(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'dumb-jump-mode)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (setq go-packages-function 'go-packages-go-list))



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
