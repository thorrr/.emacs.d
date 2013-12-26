.emacs.d
=============
emacs config files with cleanly installed external packages.

Usage
-----
Check out the repository:

    cd && git clone https://github.com/thorrr/.emacs.d.git
      
Copy the following into .emacs:

    (setq custom-var-1 "")
    (setq custom-var-2 "")
    (load "~/.emacs.d/emacs-common.el")

External packages will go into `~/.local-emacs/externals` by default.

Local Machine Customizations
-------------
Put into `.emacs`.  File `.emacs.d/emacs-common.el` drives the rest of the configuration.


Adding packages
---------------
Programatically add git repositories with a list of 2-lists:

    (setq git-projects `(
      ("package-name-A" "https://github.com/rms/package-name-A")
      ("package-name-B" "https://github.com/rms/package-name-B")
    )

Or if your list is in ELPA, a list of package names:

    (setq my-packages `(
      "elpa-package-A" "elpa-package-B"
    )

Changing Common Functionality
-----------------------------
* `config.el` - non-programming mode customizations
* `modes/*.el` - major mode customizations
* `commands.el` - interactive commands
* `newstuff.el` - experimental things here
* `packages.el` - grungy package download handling.  You shouldn't have to touch this.
