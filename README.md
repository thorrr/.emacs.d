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
Use the use-package macro.  Put it in packages.el

Changing Common Functionality
-----------------------------
* `config.el` - non-programming mode customizations
* `modes/*.el` - major mode customizations
* `commands.el` - interactive commands
* `packages.el` - full of use-package declarations.  Put non-mode
  specific packages here.
