(defun undo-tree-basename (filename)
  "calculate a unique filename for a file's undo tree history"
  (let* ((hash (substring (secure-hash 'md5 filename) 0 10))
        (basename (file-name-nondirectory filename))
        (undo-tree-basename (concat basename "-" hash "-ut")))
    undo-tree-basename))
        
(defadvice undo-tree-make-history-save-file-name
  (after undo-tree activate)
  (setq ad-return-value
        (let ((dir (file-name-directory ad-return-value)) ;;use the computed undo-tree save directory
              (buffer-filename (ad-get-arg 0)))
          (concat dir (undo-tree-basename buffer-filename)
;;                  ".gz" ;;comment out this line to turn off compression
                  ;; TODO:  defcustom to switch this on and off
                  ))))
(defadvice undo-tree-save-history (around undo-tree-save-history-no-message activate)
  (setq messaging-on nil) ad-do-it (setq messaging-on t))

