;; Shell call typer.py

(defun typer-paste-clipboard ()
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  ;(shell-command "xdotool keydown alt key Tab; sleep 0.1; xdotool keyup alt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.6"))


(defun typer-paste-clipboard-remove-cols-csv-line ()
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  ;(shell-command "xdotool keydown alt key Tab; sleep 0.1; xdotool keyup alt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.5 -d,"))


(defun typer-paste-region ()
  "Pastes an emacs region already in the clipboard. Assumes Unix newline-style line-endings."
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.5 -d'\n' --sleep-per-char 0.01"))
