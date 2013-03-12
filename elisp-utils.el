;;from http://emacswiki.org/emacs/ThreadMacroFromClojure
(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))
(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun list-of-regexp (filename regexp)
  "Return matching regular subexpressions as a list"
  (defun recur ()
    (if (null (re-search-forward "^class \\(\\w+\\)(" (point-max) t))
        nil
      (goto-char (match-end 1))
      (cons (match-string-no-properties 1) (recur))))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (recur)))

(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)
