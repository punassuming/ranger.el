rem delete byte compile file
rem del ranger.elc

emacs.exe -Q --eval "(let ((ranger-file \"..\\ranger.el\")) (byte-compile-file ranger-file))"