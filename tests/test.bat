
rem test cwd
emacs.exe -Q --eval "(let ((ranger-file \"~/Documents/dev/emacs/ranger/ranger.el\")) (load-file ranger-file) (find-file ranger-file) (ranger) (ranger-disable) (dired \".\"))"

rem test parent
emacs.exe -Q --eval "(let ((ranger-file \"~/Documents/dev/emacs/ranger/ranger.el\")) (load-file ranger-file) (find-file ranger-file) (ranger) (ranger-up-directory) (ranger-disable) (dired \"..\"))"


emacs.exe -Q --eval "(let ((ranger-file \"~/Documents/dev/emacs/ranger/ranger.el\")) (load-file ranger-file) (find-file ranger-file) (ranger))"
