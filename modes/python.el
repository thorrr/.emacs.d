;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python main setup file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python specific keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
            (define-key python-mode-map (kbd "C-M-<return>") 'my-python-send-buffer)
            (define-key python-mode-map (kbd "M-.") 'my-rope-goto-definition)
            (define-key python-mode-map (kbd "M-l") 'my-rope-go-backward)
            (define-key python-mode-map (kbd "M-i") 'my-python-shell-smart-switch)
            (define-key python-mode-map (kbd "C-c C-j") 'my-python-eval-line)
            (define-key python-mode-map (kbd "S-<f4>") 'my-restart-python)
            ))

(add-hook 'inferior-python-mode-hook (lambda ()
            (define-key inferior-python-mode-map (kbd "M-i") 'my-python-shell-smart-switch)
            (define-key inferior-python-mode-map [f9] 'my-python-show-graphs)
            (define-key inferior-python-mode-map [down] 'comint-next-matching-input-from-input)
            (define-key inferior-python-mode-map [up] 'comint-previous-matching-input-from-input)
            (define-key inferior-python-mode-map [f4] 'my-restart-python)
            ))

;; can't use python-shell-extra-pythonpaths because these have to be set before we require 'pymacs
 (setenv "PYTHONPATH" (concat
    (concat shared-externals "Pymacs" path-separator)
    (concat shared-externals "ropemacs" path-separator)
    (concat shared-externals "ropemode" path-separator)
    (concat shared-externals "rope" path-separator)
    (getenv "PYTHONPATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inferior Python shell setup variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom python-shell-interpreter-var "ipython"
  "Custom path to python interpreter.  Use ipython by default")
(defcustom python-shell-interpreter-args-var "-i"
  "Custom arguments for starting python interpreter.  Interactive (-i) by default")

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

(setq
 python-shell-interpreter python-shell-interpreter-var
 python-shell-interpreter-args python-shell-interpreter-args-var
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n" 
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pymacs)
(setq pymacs-auto-restart t)

(defun my-turn-on-ropemacs () (interactive)
  (setq ropemacs-enable-autoimport 't)
  (setq ropemacs-autoimport-modules `("os" "shutil"))
  (if (not (boundp 'ropemacs-mode)) (pymacs-load "ropemacs" "rope-"))
  (if (and (boundp 'ropemacs-mode) (not ropemacs-mode)) (ropemacs-mode)))

;; regenerate the import cache whenever you open a project.  this can be slow the first time
(defadvice rope-open-project (after rope-open-project-then-regenerate-import-cache activate)
  (rope-generate-autoimport-cache))

(ac-ropemacs-initialize) ;; hook rope into auto-complete

;; another source for python auto-complete that comes from the *Python* buffer or the unnamed "internal" process
(require 'ac-python)

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks - loading a python file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook (lambda ()
   (set-variable 'python-indent-offset 4)
   (set-variable 'indent-tabs-mode nil)
   (setq ropemacs-enable-autoimport t)
   ;;(setq ac-sources '())
   (add-to-list 'ac-sources 'ac-source-python)
   ;;TODO - make symbolName : packagea.packageb.packageC trigger an import statement
   (add-to-list 'ac-sources 'ac-source-ropemacs)
   ;; (add-to-list 'ac-sources 'ac-source-yasnippet)

   
   ;; an Internal Process is created for each unique configuration.
   ;; set up the virtualenv before calling this and each virtualenv
   ;; will have its own internal process
   (virtualenv-hook)
   (python-just-source-file (buffer-file-name) (python-shell-internal-get-or-create-process))
   (project-root-fetch)
   (setq ropemacs-guess-project (cdr project-details));;get all of the python subdirectories
   (local-set-key [S-f10] 'my-python-run-test-in-inferior-buffer)
   (local-set-key [f10] 'my-python-toggle-test-implementation)
   (my-turn-on-ropemacs) ;;something repeatedly calls pymacs-load "ropemacs" so you have to switch it back on
   (define-key ropemacs-local-keymap (kbd "M-?") 'ac-start)
   (define-key ropemacs-local-keymap (kbd "M-/") 'hippie-expand)
   (autopair-mode)
   (setq autopair-handle-action-fns '(autopair-default-handle-action
                                      autopair-dont-if-point-non-whitespace
                                      autopair-python-triple-quote-action))
))



(add-hook 'inferior-python-mode-hook (lambda ()
  ;; jump to the bottom of the comint buffer if you start typing
  (make-local-variable 'comint-scroll-to-bottom-on-input) (setq comint-scroll-to-bottom-on-input t)
))

;;overwrite ropemacs "lucky code assist"
(add-hook 'ropemacs-mode-hook (lambda ()
  (define-key ropemacs-local-keymap (kbd "M-?") 'ac-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;wrapper to make ac-python work with Gallina's python.el
(defun python-symbol-completions (symbol)
  (let ((process (python-get-named-else-internal-process))
         ;;this breaks the abstraction in ac-python (it's defined without referencing the cursor position
         ;;but i don't feel like changing ac-python right now
         (current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (python-shell-completion-get-completions process current-line symbol)))


;; This equivalent function doesn't exist in Gallina's code
(defun python-get-named-else-internal-process ()
  "return the current global process if there is one.  Otherwise, start an internal process and return that."
  (let* ((global-process (python-shell-get-process))
         (internal-process-state (process-live-p (python-shell-internal-get-process-name)))
         (internal-process (if internal-process-state (get-process (python-shell-internal-get-process-name))
                             nil))
         (existing-process (if global-process global-process internal-process))
         (process (if (not existing-process)
                      (progn (message "Starting inferior, unnamed Python process.")
                             (python-shell-internal-get-or-create-process))
                    existing-process)))
         process))

(defun my-rope-goto-definition ()(interactive) (push-current-location) (rope-goto-definition)) 
(defun my-rope-go-backward () (interactive) (pop-current-location))
(defun my-python-send-buffer () (interactive) (python-shell-send-buffer) (my-python-shell-smart-switch))
(defun my-python-shell-smart-switch ()
  (interactive)
  (let ((saved-point (point))
	(saved-frame (selected-frame))
	(saved-window (selected-window)))
    (if (string= (python-shell-get-process-name t) "Python") (end-of-buffer) ;;we are in the inferior buffer
      (let ((display-buffer-reuse-frames t)) (python-shell-switch-to-shell) (end-of-buffer)))
    (if (and (eq saved-point (point))
             (eq saved-frame (selected-frame))
             (eq saved-window (selected-window))) ;;nothing moved - we're at the end of the inferior buffer
        (progn
          (raise-frame my-python-most-recent-frame)
          (select-window my-python-most-recent-window))
      (if (not (eq saved-window (selected-window))) ;;moved to a different window
          (progn (setq my-python-most-recent-frame saved-frame)
                 (setq my-python-most-recent-window saved-window)
                 )))))

(defun my-restart-python () (interactive)
  (let ((process (python-shell-get-or-create-process))
        (in-repl (eq major-mode 'inferior-python-mode)))
    (if in-repl (other-window 1))
    (python-shell-send-string "quit()" process)
    (sleep-for 0.1)
    (python-shell-get-or-create-process)
    (if in-repl (sleep-for 0.1) (other-window 1) (end-of-buffer))
))

;; I never want run-python to ask me for a path
 (defun run-python (&optional a b) (interactive "ii")
   (python-shell-make-comint (python-shell-parse-command) (python-shell-get-process-name nil) t))

(defun my-python-eval-line ()
  "Evaluate the current Python line in the inferior Python process."
  (interactive) (python-shell-send-string (buffer-substring-no-properties (point) (line-end-position))
                                          (python-get-named-else-internal-process)))
(defun my-python-eval-region (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (python-shell-send-string "%paste" nil t))

(defun python-shell-send-region (start end)
  "Overridden.  Use the %paste IPython method to send copied regions to the inferior Python process."
  (interactive "r")
  (my-python-eval-region start end))

(defun python-just-source-file (filename &optional process)
  "Force process to evaluate filename but don't run __main__.
   Gallina has a similar technique for evaluating buffers in
   python-shell-send-buffer.  But his doesn't allow us to specify
   an internal Python process"
  (let ((command-string-1 "___oldName = __name__")
        (command-string-2 "__name__ = None")
        (command-string-3
         (concat "execfile( \"" filename "\", globals())"))
        (command-string-4 "__name__ = ___oldName"))
    (python-shell-send-string command-string-1 process)
    (python-shell-send-string command-string-2 process)
    (python-shell-send-string command-string-3 process)
    (python-shell-send-string command-string-4 process)
    ))


;; pyflakes flymake hook
(defun flymake-pyflakes-init () 
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                     'flymake-create-temp-inplace)) 
         (local-file (file-relative-name 
                      temp-file 
                      (file-name-directory buffer-file-name)))) 
    (list "pyflakes" (list local-file))))

;;modify pyflakes' output
(add-hook 'python-mode-hook (lambda ()
   ;; use \\| to separate multiple match criteria                              
   (setq flymake-warn-line-regexp "imported but unused\\|unable to detect undefined names")
   (setq flymake-info-line-regexp "is assigned to but never used")))


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

;;first attempt at ctrl-click
(global-set-key [C-down-mouse-1]
                (lambda (click)
                  (interactive "e")
                  (mouse-minibuffer-check click)
                  (let* ((window (posn-window (event-start click)))
                         (buf (window-buffer window)))
                    (with-current-buffer buf
                      (save-excursion
                        (goto-char (posn-point (event-start click)))
                        (my-rope-goto-definition))))))

;;first attempt at ipdb stuff.  from http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ 	]*import ipdb; ipdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)


(defun my-make-python-shell-gui-interactive ()
  (interactive) (python-shell-send-string "from matplotlib import interactive; interactive(True)"))
(defun my-python-show-graphs ()
  (interactive) (python-shell-send-string "from pylab import show; show()"))


(defun virtualenv-test (path)
  (let* ((python-subpath (if (eq system-type 'windows-nt)
                           "Scripts\\python.exe"
                           "bin/python"))
         (env-root (locate-dominating-file
                   (or env default-directory) python-subpath)))
    env-root))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenv support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom auto-detect-virtualenv 't
  "When loading a python file attempt to find its virtualenv using function detect-virtualenv.")

(defcustom current-virtualenv nil
  "Open python files using this virtualenv")

(defcustom ipython-use-with-virtualenv 't
  "Set up python-shell-interpreter-args-var correctly to
use ipython with the current virtualenv")

(defvar virtualenv-bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin"))

(defun set-current-virtualenv (dir)
  (interactive "D")
  (setq current-virtualenv (expand-file-name dir))
  't)

(defun reset-current-virtualenv ()
  (interactive)
  (setq current-virtualenv nil)
  't)

(defun detect-virtualenv (filename)
  "resets variable current-virtualenv if it can detect this
  python file has a virtualenv in its path"
  (expand-file-name "~/svn/FedtradeAutoLogin/env"))

(defun virtualenv-hook ()
  "This should be run in python-mode-hook before any comints are
run"
  (let ((used-virtualenv
         (cond (current-virtualenv
                current-virtualenv)
               (auto-detect-virtualenv
                (detect-virtualenv (buffer-file-name)))
               ('t
                default-value python-shell-virtualenv-path))))
    (setq python-shell-virtualenv-path used-virtualenv)
    (if ipython-use-with-virtualenv
        (setq python-shell-interpreter-args (format "-u %s/%s/%s" used-virtualenv
                                                    virtualenv-bin-dir
                                                    "ipython-script.py")))))

(defun python-shell-calculate-process-environment ()
  "Calculate process environment given `python-shell-virtualenv-path'.
Overridden from Gallina - his doesn't work with win32"
  
  (let ((process-environment (append
                              python-shell-process-environment
                              process-environment nil))
        (virtualenv (if python-shell-virtualenv-path
                        (directory-file-name python-shell-virtualenv-path)
                      nil)))
    (when python-shell-extra-pythonpaths
      (setenv "PYTHONPATH"
              (format "%s%s%s"
                      (mapconcat 'identity
                                 python-shell-extra-pythonpaths
                                 path-separator)
                      path-separator
                      (or (getenv "PYTHONPATH") ""))))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "PATH" (format "%s/%s%s%s"
                             virtualenv virtualenv-bin-dir
                             path-separator
                             (or (getenv "PATH") "")))
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defun python-shell-calculate-exec-path ()
  "Calculate exec path given `python-shell-virtualenv-path'.
Overridden from Gallina - his doesn't work with win32"
  (let ((path (append python-shell-exec-path
                      exec-path nil)))
    (if (not python-shell-virtualenv-path)
        path
      (cons (format "%s/%s"
                    (directory-file-name python-shell-virtualenv-path)
                    virtualenv-bin-dir)
            path))))

