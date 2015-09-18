rem delete byte compile file
del ranger.elc

C:\Apps\emacs\bin\emacs.exe -Q --eval "(let ((ranger-file \"~/Documents/dev/emacs/ranger/ranger.el\")) (byte-compile-file ranger-file))"