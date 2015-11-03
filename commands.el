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

(require 'loccur)
;; (defun my-occur (&optional regexp)
;;   "Switch to the *Occur* buffer or run `occur'."
;;   (interactive)
;;   (if (get-buffer "*Occur*")
;;           (switch-to-buffer-other-window "*Occur*")
;;         (call-interactively 'loccur)))

(defun run-occur-during-interactive-search ()
  (interactive)
   (let ((case-fold-search isearch-case-fold-search))
      (loccur (if isearch-regexp isearch-string
                (regexp-quote isearch-string)))
      ;;(switch-to-buffer-other-window "*Occur*")
      ))

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

(defun source-shell-script (script)
  (interactive)
  (let* ((fn (expand-file-name script))
         (cmd (concat (format "sh -c \"source \"%s\" 2>&1 /dev/null && env | " fn )  ;; source the file and call 'env'
                      "sed -e 's/\\\\\\\\/\\\\\\\\\\\\\\\\/g' | " ;; change single backslashes to double backslashes
                      "sed -e 's/\\\"/\\\\\\\\\\\"/g' | " ;; escape quotes in environment values
                      "sed -e 's/\\([^=]*\\)=\\(.*\\)/(setenv \\\"\\1\\\" \\\"\\2\\\" \\)/g' " ;; create 'setenv' pairs
                      "\""                                          ;; end of argument to 'sh'
                      ))
         (string (shell-command-to-string cmd))
         (cmd-list (split-string string "\n")))
    ;;individually execute each pair
    (mapcar (lambda (arg) (message arg) (if (not (string= "" arg)) (eval (car (read-from-string arg))))) cmd-list))
  't)

(defun normalize-line-endings (buffer)
  "Delete ^M in files with a mixture of dos and unix line
   endings.  Mark buffer modified"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun set-eol-conversion (new-eol)
  "Specify new end-of-line conversion NEW-EOL for the buffer's file
   coding system. This marks the buffer as modified.
   This function is equivalent to C-x RET f [unix,dos,mac] "
  (interactive "Send-of-line conversion for visited file: \n")
  ;; Check for valid user input.
  (unless (or (string-equal new-eol "unix")
              (string-equal new-eol "dos")
              (string-equal new-eol "mac"))
    (error "Invalid EOL type, %s" new-eol))
  (if buffer-file-coding-system
      (let ((new-coding-system (coding-system-change-eol-conversion
                                buffer-file-coding-system new-eol)))
        (set-buffer-file-coding-system new-coding-system))
    (let ((new-coding-system (coding-system-change-eol-conversion
                              'undecided new-eol)))
      (set-buffer-file-coding-system new-coding-system)))
  (message "EOL conversion now %s" new-eol))

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun show-dos-eol ()
  "Show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M nil))

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
  (message "Renamed to %s." new-name)))
