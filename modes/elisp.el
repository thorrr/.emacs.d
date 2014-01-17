(add-hook 'emacs-lisp-mode-hook (lambda ()
  (define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-find-definition)
  (define-key emacs-lisp-mode-map (kbd "M-,") 'pop-current-location)
))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(defun elisp-find-definition (name)  ;;from http://lists.gnu.org/archive/html/help-gnu-emacs/2009-09/msg00669.html
  "Jump to the definition of the function (or variable) at point."
  (defun elisp-push-point-marker ()
    (require 'etags)
    (cond ((featurep 'xemacs)
           (push-tag-mark))
          (t (ring-insert find-tag-marker-ring (point-marker)))))
  (interactive (list (thing-at-point 'symbol)))
  (push-current-location)  ;;my addition
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char (+ point 1))
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))
