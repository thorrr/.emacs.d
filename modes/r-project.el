(require 'ess-site)
;;(defun myindent-ess-hook ()
;;  (setq ess-indent-level 4))
;;(add-hook 'ess-mode-hook 'myindent-ess-hook)

(add-hook 'ess-mode-hook (defun myindent-ess-hook () (setq ess-indent-level 4)))

(setq ess-ask-for-ess-directory nil)
(setq ess-fancy-comments nil)
;;(setq inferior-ess-own-frame t) ;;pop open a new window when starting R
(setq inferior-R-args "--no-save")
;;go to the last input if you're in your R buffer
(add-hook 'ess-mode-hook
	  (lambda ()
	    (define-key ess-mode-map (kbd "M-i") 'ESS-smart-switch)))
;;same binding but in the R inferior buffer itself
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (define-key inferior-ess-mode-map (kbd "M-i") 'ESS-smart-switch)))

(defun ESS-smart-switch ()
  (interactive)
  (let ((saved-point (point))
	(saved-frame (selected-frame))
	(saved-window (selected-window)))
    (ess-switch-to-end-of-ESS)
    (if (and (eq saved-point (point))
	     (eq saved-frame (selected-frame))
	     (eq saved-window (selected-window))) ;;nothing moved
	(progn
	  (raise-frame ESS-most-recent-frame)
	  (select-window ESS-most-recent-window))
      (if (not (eq saved-window (selected-window))) ;;moved to a different window
	  (progn (setq ESS-most-recent-frame saved-frame)
		 (setq ESS-most-recent-window saved-window)
		 )))
    )
  )
