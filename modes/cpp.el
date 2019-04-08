(require 'cc-mode)

(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

(defun sm-find-tag ()
  (interactive)
  (find-tag (funcall (or find-tag-default-function
                         (get major-mode 'find-tag-default-function)
                         'find-tag-default))))


(define-key c++-mode-map (kbd "M-.") 'sm-find-tag)
(define-key c++-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key c-mode-map (kbd "M-.") 'sm-find-tag)
(define-key c-mode-map (kbd "M-,") 'pop-tag-mark)
