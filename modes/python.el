;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python main setup file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq python-major-version 3)
(if (eq system-type 'windows-nt) (setq python-shell-prompt-detect-enabled nil))
(setq python-column-width 100)
(setq python-inferior-shell-type 'python)
(setq python-use-pyflakes 't)
(setq python-use-pep8 't)
(setq python-use-pylint 't)
(setq auto-detect-virtualenv 't)
(setq auto-python-just-source-file 't)
(setq pymacs-parent-dir shared-externals)
(require 'python-goodies)

(add-to-list 'python-indent-trigger-commands 'smart-auto-complete)

;; doesn't seem to work, completion code triggers indentation errors
(setq python-shell-completion-native-enable nil)

;; py-yapf - autoformatting
(require 'py-yapf)
(setq py-yapf-options
  `("--style" ,(concat "{"
      ;;;; construct a python dict literal for the command line
      "based_on_style: pep8, "
      "indent_width: 4, "
      (format "column_limit: %d, " python-column-width)
      "dedent_closing_brackets: False, "
      "coalesce_brackets: False, "
      "split_before_first_argument: True, "
      "split_penalty_import_names: 300, "
      ;;;;
      "}")))


(defun _thin-region-beginning ()
  "If a region borders a newline don't include that line in the region"
  (save-excursion
    (goto-char (region-beginning))
    (if (eolp)
        (1+ (region-beginning))
      (region-beginning))))

(defun _thin-region-end ()
  "If a region borders a newline don't include that line in the region"
  (save-excursion
    (goto-char (region-end))
    (if (bolp)
        (1- (region-end))
      (region-end))))

(defun py-yapf-region (&optional start-pos end-pos)
  (interactive)
  (let* ((start (line-number-at-pos
                 (if start-pos start-pos (_thin-region-beginning))))
         (end (line-number-at-pos (if end-pos end-pos (_thin-region-end))))
         (start-end-string (format "%d-%d" start end))
         (py-yapf-options (append py-yapf-options (list "-l" start-end-string))))
    ;; wrap the call to py-yapf-buffer with a nil "message" function and a nil
    ;; "write-region" function so it doesn't annoyingly print "Buffer is already yapfed"
    ;; and "Wrote /tmp/asdf.yapf".  Setting the "visit" argument to an arbitrary symbol in
    ;; "write-region" turns off the "wrote" message.
    (cl-letf (((symbol-function 'write-region-original) (symbol-function 'write-region))
              ((symbol-function 'message) (lambda (fs &rest args)))
              ((symbol-function 'write-region) (lambda
                 (start end filename &optional append visit lockname mustbenew)
                 (write-region-original start end filename append 'nomessage lockname mustbenew))))
      (py-yapf-buffer))))


;; yapf doesn't fix inline comments with bad spacing so run this afterwards
(defun python-fix-inline-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((opening-quote-on-line-p
           (lambda ()
             (or (looking-at ".*\"") (looking-at ".*'"))))
          (line-end (save-excursion (end-of-line) (point))))
      (while (funcall opening-quote-on-line-p) (progn
        ;;first, move cursor past any triple quotes that start on this line
        (if (looking-at ".*?\"\"\"") (progn
          (re-search-forward ".*?\"\"\"" line-end)
          (re-search-forward ".*?\"\"\"" line-end 'eol)))
        (if (looking-at ".*?'''") (progn
          (re-search-forward ".*?'''" line-end)
          (re-search-forward ".*?'''" line-end 'eol)))
        ;;now move past standalone single quotes or double quotes
        (if (looking-at ".*?'") (progn
          (re-search-forward ".*?'" line-end)
          (re-search-forward ".*?'" line-end 'eol)))
        (if (looking-at ".*?\"") (progn
          (re-search-forward ".*?\"" line-end)
          (re-search-forward ".*?\"" line-end 'eol)))
        ))
      ;; we've skipped all literal strings.  now look for the first # with something visible after it
      (if (re-search-forward "#" line-end 'eol)
          (if (looking-at "[[:graph:]]")
              (insert " ")))
      )))

(defun python-fix-inline-comments (&optional start-pos end-pos)
  (interactive)
  (let* ((start (line-number-at-pos
                 (if start-pos start-pos (_thin-region-beginning))))
         (region-end-line (line-number-at-pos (if end-pos end-pos (_thin-region-end))))
         (buffer-end-line (save-excursion (line-number-at-pos (goto-char (point-max)))))
         (end (min region-end-line buffer-end-line))
         (_ nil)   ;; for dotimes macro
         (__ nil)) ;; for dotimes macro
    (save-excursion
      (goto-line start)
      (dotimes (_ (1+ (- end start)) __)
        (python-fix-inline-comment)
        (forward-line)))))

(defun py-yapf-smart ()
  "run yapf then fix-inline-comment on the marked region"
  (interactive)
  (let ((start (if mark-active (_thin-region-beginning) (point)))
        (end (if mark-active (_thin-region-end) (point))))
    (py-yapf-region start end)
    (python-fix-inline-comments start end)))

;; use M-q to both yapf and fill-paragraph
(defun python-yapf-and-fill-paragraph (&optional JUSTIFY REGION)
  (interactive)
  (fill-paragraph JUSTIFY REGION) ;; this seems to be wonky
  (py-yapf-smart)
  (deactivate-mark))

(add-hook 'python-mode-hook (lambda ()
  (define-key python-mode-map (kbd "M-q") 'python-yapf-and-fill-paragraph)
))

;;;; mark something and hit "f" to auto-format it
;;;; bug - this is activated in all types of buffers, not just python
(add-hook 'python-mode-hook (lambda ()
  (require 'region-bindings-mode)
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map "f" (lambda () (interactive)
    (if (eq major-mode 'python-mode) (py-yapf-smart)
      (self-insert-command 1))))
))

;; override our global keymap's M-. and M-,
(add-hook 'python-mode-hook (lambda ()
  (setq-local dumb-jump-go-override (lambda () (interactive) (python-goodies/rope-goto-definition)))
  (setq-local dumb-jump-back-override (lambda () (interactive) (python-goodies/rope-go-backward)))
))
