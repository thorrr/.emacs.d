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

;; Make a non-standard key binding.  We can put this in
;; c-mode-base-map because c-mode-map, c++-mode-map, and so on,
;; inherit from it.
;; (defun atg-c-initialization-hook ()
;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break)
;;   (local-set-key [(f2)] 'ff-find-other-file)
;;   (local-set-key [C-f3] 'c-fill-paragraph)
;;   (local-set-key [f3] 'c-comment-line-break-function)
;;   (c-add-style "PERSONAL" atg-c-style)
;;   )

;; (add-hook 'c-initialization-hook 'atg-c-initialization-hook)

;; offset customizations not in atg-c-style
;; This will take precedence over any setting of the syntactic symbol
;; made by a style.
;; (setq c-offsets-alist '((member-init-intro . ++)))

;; Create ATG style.
;; (defconst atg-c-style
;;   '(
;;     ;; (c-tab-always-indent        . nil)
;;     ;;     (c-hanging-braces-alist     . ((substatement-open after)
;;     ;;                                    (brace-list-open)))
;;     (c-hanging-colons-alist     . ((member-init-intro before)
;;                                    (inher-intro)
;;                                    (case-label after)
;;                                    (label after)
;;                                    (access-label after)))
;;     (c-cleanup-list             . (scope-operator
;;                                    empty-defun-braces
;;                                    defun-close-semi))
;;     (c-offsets-alist            . ( ;; (arglist-close . c-lineup-arglist)
;;                                    (substatement-open . 0)
;;                                    (label             . 0)
;;                                    (case-label        . 4)
;;                                    (block-open        . 0)
;;                                    (inline-open       . 0)
;;                                    (knr-argdecl-intro . -)))
;;     (c-echo-syntactic-information-p . t)
;;     (c-doc-comment-style        . ((c-mode   . javadoc)
;;                                    (c++-mode . javadoc)))
;;     )
;;   "ATG C Programming Style")


;; Customizations for all modes in CC Mode.
;; (defun atg-c-mode-common-hook ()
;;   ;; set ATG style for the current buffer
;;   (c-set-style "PERSONAL")
;;   ;; other customizations
;;   (setq tab-width 4
;;         ;; this will make sure spaces are used instead of tabs
;;         indent-tabs-mode nil)
;;   (turn-on-auto-fill)
;;   (setq fill-column 78)
;;   (setq comment-column 58)

;;   ;; we like hungry-delete
;;   (c-toggle-hungry-state 1))

;; (add-hook 'c-mode-common-hook 'atg-c-mode-common-hook)
