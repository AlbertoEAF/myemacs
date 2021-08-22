# About

This repo has all the configurations neatly organized into the `myemacs-2020.org` file.

To use it just add this line to your `.emacs` file:
```lisp
;; Add this to your .emacs file:
(setq myemacs-dir "~/myemacs/")  ;; To load internal modules
(org-babel-load-file "~/myemacs/myemacs-2020.org")
```

# Installation alternatives:

Since this provides an `init.el` file with that line you can alternatively load that `init.el` directly or even set this repo as the emacs directory to try it out without affecting your emacs:

```lisp
(setq myemacs-dir "~/myemacs/")  ;; To load internal modules
(setq user-emacs-directory "~/myemacs")
```


# Also...

Don't forget that emacs has many built-ins already. Don't re-invent the wheel :)

Want to encase something in quotes, parenthesis or something else? Move the cursor behind your word / selected region and do `M-1 M-(`.
