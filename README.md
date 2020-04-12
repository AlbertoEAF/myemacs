# About

This repo has all the configurations neatly organized into the `myemacs-2020.org` file.

To use it just add this line to your `.emacs` file:
```lisp
;; Add this to your .emacs file:
(org-babel-load-file "~/myemacs/myemacs-2020.org")
```

# Installation alternatives:

Since this provides an `init.el` file with that line you can alternatively load that `init.el` directly or even set this repo as the emacs directory to try it out without affecting your emacs:

```lisp
(setq user-emacs-directory "~/myemacs")
```
