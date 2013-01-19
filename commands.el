(defun push-current-location ()
  (if (not (boundp 'my-locations)) (setq my-locations '() ))
  (push (list (point-marker) (selected-frame) (selected-window)) my-locations))

(defun pop-current-location ()
  (interactive)
  (if my-locations 
      (let* ((last-location (pop my-locations))
             (last-marker (nth 0 last-location))
             (last-frame (nth 1 last-location))
             (last-window (nth 2 last-location))
             )
        (raise-frame last-frame)
        (select-window last-window)
        (switch-to-buffer (marker-buffer last-marker))
        (goto-char (marker-position last-marker)))))

;; IntelliJ-like duplicate-line function
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg)
  )

;; cute join-next-line keystroke
(defun join-next-line ()
  (interactive)
  (save-excursion (end-of-line)(next-line)(join-line))
  )

;;resize
(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

;; borrowed and enhanced from http://emacswiki.org/emacs/CamelCase
(defun camelcase-split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))
(defun camelscore (s)	
  (let ((case-fold-search nil))
    (cond ((eq (string-match-p "[A-Z]" s) 0) (lowercamelcase s))
           ((string-match-p "-" s)     (camelcase s))
           ((string-match-p "_" s)	(dasherize s))
           (t                          (underscore s)) )))
(defun camelcase  (s) (mapconcat 'capitalize (camelcase-split-name s) ""))
(defun lowercamelcase  (s) (concat (downcase (car (camelcase-split-name s)))
                                   (mapconcat 'capitalize (cdr (camelcase-split-name s)) "")))
(defun underscore (s) (mapconcat 'downcase   (camelcase-split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (camelcase-split-name s) "-"))
(defun toggle-camelcase-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;; wrap quotes around next word
(defun quote-wrap-forward-word ()
  (interactive)
  (let* ((repeated? (if (eq last-command 'quote-wrap-forward-word) 't nil))
         (quote-char (if repeated? "'" "\""))
         )
    (save-excursion
      (if repeated? (progn  ;;undo the previous quotes but don't add to the "redo" list
          (setq buffer-undo-list (primitive-undo 2 buffer-undo-list))
          (undo-boundary)))
      (forward-word)(backward-word)
      (insert quote-char)
      (forward-word)
      (insert quote-char))))

(defun cls ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)
    (goto-char (point-max))))

(defun comint-jump-to-input-ring ()
    "Jump to the buffer containing the input history."
    (interactive)
    (progn
      (comint-dynamic-list-input-ring)
      (other-window 1)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun update-external-packages ()
  (interactive)
  (mapcar (lambda (e) (git-update (car e) (cadr e))) git-projects)
  (mapcar (lambda (e) (hg-update (car e) (cadr e))) hg-projects))

(defun my-occur (&optional regexp)
  "Switch to the *Occur* buffer or run `occur'."
  (interactive)
  (if (get-buffer "*Occur*")
          (switch-to-buffer-other-window "*Occur*")
        (call-interactively 'occur)))

(defun run-occur-during-interactive-search ()
  (interactive)
   (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string)))
      (switch-to-buffer-other-window "*Occur*")))

(defun write-last-macro-to-messages ()
  "Write the last macro to *Messages*."
  (interactive)
  (with-current-buffer "*Messages*"
    (let ((name '***tmp-macro***))
      (kmacro-name-last-macro 'name)
      (goto-char (point-max))
      (newline)
      (insert-kbd-macro 'name)
       (newline))))

