;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python main setup file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      ;;;;                 
      "based_on_style: pep8, "
      "indent_width: 4, "
      (format "column_limit: %d, " python-column-width)
      "SPLIT_PENALTY_FOR_ADDED_LINE_SPLIT: 99"
      ;;;;
      "}")))

(defun py-yapf-region (&optional start-pos end-pos)
  (interactive)
  (let* ((start (line-number-at-pos
                 (if start-pos start-pos (region-beginning))))
         (end (line-number-at-pos (if end-pos end-pos (region-end))))
         (start-end-string (format "%d-%d" start end))
         (py-yapf-options (append py-yapf-options (list "-l" start-end-string))))
    ;; wrap the call to py-yapf-buffer with a nil "message" function so it doesn't
    ;; annoyingly print "Buffer is already yapfed"
    (cl-letf (((symbol-function 'message) (lambda (fs &rest args))))
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
                 (if start-pos start-pos (region-beginning))))
         (region-end-line (line-number-at-pos (if end-pos end-pos (region-end))))
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
  (let ((start (if mark-active (region-beginning) (point)))
        (end (if mark-active (region-end) (point))))
    (py-yapf-region start end)
    (python-fix-inline-comments start end)))

;; use M-q to both yapf and fill-paragraph
(defun python-yapf-and-fill-paragraph (&optional JUSTIFY REGION)
  (interactive)
  (fill-paragraph JUSTIFY REGION) ;; this seems to be wonky
  (py-yapf-smart))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old deprecated stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom my-python-test-template
  "from test.package import AtgTestCase
import doctest

def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(%PACKAGENAME%))
    return tests

class %TESTCLASSNAME%(AtgTestCase):
    def setUp(self):
        pass
    def tearDown(self):
        pass

    def test_%PACKAGENAME%(self):
        %CURSOR-HERE%
        pass

if __name__ == '__main__':
    import unittest
    unittest.main()"
    "Python test template file"
)



(defun my-full-python-module (filename project-root)
  "Return the package followed by this filename's module.
  For example, \"~/goo/data/tsy/foo_bar.py\" returns (\"goo.data.tsy\" \"foo_bar\")"
  (let ((subtree (replace-regexp-in-string project-root "" filename t)))
    (string-match "^\\(.*\\)/\\(.\+\\).py" subtree)
    (list (replace-regexp-in-string "/" "." (match-string 1 subtree))
          (match-string 2 subtree))))

(defun my-python-test-file (filename &optional pr ptr)
  "Get the corresponding test filename or return filename if it's a valid test file"
  (if (or (null python-root) (null python-test-root))
      (message "Warning: local variable python-root or python-test-root is not defined"))
  (if (null pr) (setq pr python-root))
  (if (null ptr) (setq ptr python-test-root))
  (let* ((this-local-path (substring (replace-regexp-in-string pr "" filename) 4)) ;; "atg/" has four characters
         (this-test-file-almost (concat ptr this-local-path))
         (last-forward-slash (+ 1 (string-match-p "/\\([^/]\+\\)$" this-test-file-almost)))
         (this-test-filename (concat (substring this-test-file-almost 0 last-forward-slash)
                                     "test_" (substring this-test-file-almost last-forward-slash))))
    ;;if you're in the test subdirectory, return the filename
    (if (string-match-p (concat "^" ptr) filename) filename this-test-filename)))

(defun my-python-implementation-file (filename &optional pr ptr)
  "Get the corresponding implementation filename or return filename if it's a valid implementation file"
  (if (or (null python-root) (null python-test-root))
      (message "Warning: local variable python-root or python-test-root is not defined"))
  (if (null pr) (setq pr python-root))
  (if (null ptr) (setq ptr python-test-root))
  (let* ((this-local-path (replace-regexp-in-string ptr "" filename))
         (this-impl-file-almost (concat pr "atg/" this-local-path))
         (last-forward-slash (+ 1 (string-match-p "/\\([^/]\+\\)$" this-impl-file-almost)))
         (this-impl-filename (concat (substring this-impl-file-almost 0 last-forward-slash)
                                     (substring this-impl-file-almost (+ last-forward-slash 5))))) ;; "test_" has five characters
    ;;if you're in the test subdirectory, return the implementation filename
    (if (string-match-p (concat "^" ptr) filename) this-impl-filename filename)))

(defun my-python-class (filename)
  "Get list of Python class names from -filename-"
  (list-of-regexp filename "^class \\(\\w+\\)("))

(defun my-python-run-test-in-inferior-buffer ()
  (interactive)
  (let* ((test-filename (my-python-test-file (buffer-file-name)))
         (test-classnames (my-python-class test-filename))
         (impl-file (my-python-implementation-file (buffer-file-name)))
         (impl-python-module (my-full-python-module impl-file python-root))
         (package-name (car impl-python-module))
         (module-name (cadr impl-python-module))
         (command-string-1 "import unittest")
         (command-string-1-1 "import reimport")
         (command-string-2 (concat "from " package-name " import " module-name))
         (command-string-3 (concat "reimport.reimport(" module-name ")"))
         (command-string-4 "unittest.TextTestRunner(verbosity=1).run(___suite)  "))
    ;;first, force implementation file save because for some reason python functions will turn to 'None' if you don't
    (if (get-file-buffer impl-file)
        (with-current-buffer (get-file-buffer impl-file)
          (set-buffer-modified-p t)  
          (save-buffer)))
    ;;save test file if necessary
    (if (get-file-buffer test-filename)
        (with-current-buffer (get-file-buffer test-filename)
          (save-buffer)))
    (python-just-source-file test-filename)
    (python-shell-send-string command-string-1)
    (python-shell-send-string command-string-1-1)
    (python-shell-send-string command-string-2)
    (python-shell-send-string command-string-3)
    (mapc (lambda (classname) ;;run the commands for each class name in the test file
            (python-shell-send-string
              (concat "___suite = unittest.TestLoader().loadTestsFromTestCase(" classname ")"))
            (python-shell-send-string command-string-4))
          test-classnames)
    ))

(defun insert-test-code-into-buffer (module package &optional classname)
  (let* ((test-class-tag "%TESTCLASSNAME%")
         (package-tag "%PACKAGENAME%")
         (cursor-tag "%CURSOR-HERE%")
         (test-class-replaced
          (replace-regexp-in-string test-class-tag (concat "Test" (camelcase module)) my-python-test-template t))
         (final-string (replace-regexp-in-string package-tag module test-class-replaced t)))
    (insert (format "from %s import %s\n" package module)) ;; import the tested module
    (if classname (insert (format "from %s import %s\n" (concat package "." module) classname))) ;; also import the classname if it's given
    (insert final-string)
    ;;move to CURSOR-HERE location
    (search-backward cursor-tag nil nil)
    (replace-match "")
    ))

(defun my-python-toggle-test-implementation ()
  (interactive)
  (if (string-match-p "^test_" (buffer-name)) (my-python-switch-to-implementation)
    (my-python-switch-to-test)))

(defun my-python-switch-to-implementation ()
  (interactive)
  (let ((display-buffer-reuse-frames t))
    (pop-to-buffer (find-file-noselect (my-python-implementation-file (buffer-file-name))))))

(defun my-python-switch-to-test ()
  (interactive)
  (let* ((implementation-file (buffer-file-name))
         (full-python-module (my-full-python-module implementation-file python-root))
         (this-module-name (cadr full-python-module))
         (this-package-name (car full-python-module)))
    (let ((display-buffer-reuse-frames t))
      (pop-to-buffer (find-file-noselect (my-python-test-file (buffer-file-name)))))
    (if (eq (buffer-size) 0)
        (insert-test-code-into-buffer this-module-name this-package-name (car (my-python-class implementation-file))))
    ))

