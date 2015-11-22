rem delete byte compile file
del ranger.elc

emacs.exe -Q --eval "(let ((ranger-file \"~/Documents/dev/emacs/ranger/ranger.el\")) (byte-compile-file ranger-file))"