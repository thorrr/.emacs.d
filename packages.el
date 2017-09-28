;; make the externals directory
(setq shared-externals (expand-file-name shared-externals))
(unless (file-exists-p shared-externals)
  (make-directory shared-externals 't))

;; If we're on version 23, download the latest package.el and load it
(if (< emacs-major-version 24)
    (let* ((default-directory (expand-file-name shared-externals))
           (package-file (concat default-directory "pkg-el23")))
      (unless (file-exists-p package-file)
        (shell-command-to-string "wget http://bit.ly/pkg-el23"))
      (load package-file)))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;; change default elpa directory and load packages
(setq package-user-dir shared-externals)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; install new packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun git-clone (project-name project-url)
  (unless (file-exists-p (concat (expand-file-name default-directory) project-name))
    (let ((cmd (concat "git clone -q " project-url " " project-name)))
      (message (concat "Running git for project " project-name " in directory " default-directory))
      (shell-command-to-string cmd))))

(defun git-update (project-name project-url)
    (let ((cmd (concat "git pull -q"))
          (dir (concat default-directory project-name)))
      (if (file-exists-p dir)
          (let ((default-directory (expand-file-name dir)))
            (shell-command-to-string cmd)))))

(defun hg-clone (project-name project-url)
  (unless (file-exists-p (concat (expand-file-name default-directory) project-name))
    (let ((cmd (concat "hg clone -q " project-url " " project-name)))
      (message (concat "Running hg for project " project-name " in directory " default-directory))
      (shell-command-to-string cmd))))

(defun hg-update (project-name project-url)
    (let ((cmd (concat "hg pull -q"))
          (dir (concat default-directory project-name)))
      (if (file-exists-p dir)
          (let ((default-directory (expand-file-name dir)))
            (shell-command-to-string cmd)))))

(defun wget-clone (project-name project-url)
  (let ((dir (concat (expand-file-name default-directory) project-name)))
  (unless (file-exists-p dir)
    (let ((cmd-1 (concat "mkdir " dir))
          (cmd-2 (concat "cd " dir "&& wget " project-url)))
      (message (concat "Running wget for project " project-name " in directory " dir))
      (shell-command-to-string cmd-1)
      (shell-command-to-string cmd-2)))))


(defun run-local-package-commands (list-of-commands)
  (let ((default-directory (expand-file-name shared-externals)))
    (mapcar 'funcall list-of-commands)))

;; Install packages from git and hg
(let ((default-directory (expand-file-name shared-externals)))
  (unless (file-exists-p shared-externals)
    (make-directory shared-externals 't))
  (mapcar (lambda (e) (git-clone (car e) (cadr e))) git-projects)
  (mapcar (lambda (e) (hg-clone (car e) (cadr e))) hg-projects)
  (mapcar (lambda (e) (wget-clone (car e) (cadr e))) wget-projects)
)

  ;; Run commands in the externals directory
(run-local-package-commands make-project-commands) 

;; Add externals to load path
(mapcar (lambda (e)
          (add-to-list 'load-path (expand-file-name (concat shared-externals (car e)))))
        (append git-projects hg-projects wget-projects))
