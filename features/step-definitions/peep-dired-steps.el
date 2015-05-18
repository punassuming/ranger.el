(Given "^I open dired buffer in the root directory$"
       (lambda ()
         (dired peep-dired-root-path)))

(And "^I place cursor on \"\\(.+\\)\" entry$"
     (lambda (filename)
       (goto-char 0)
       (while (not (string= (dired-get-filename nil t)
                            (expand-file-name filename)))
         (forward-line 1))))

(When "^I run \"\\(.+\\)\"$"
      (lambda (command )
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (And (s-lex-format "I type \"${command}\""))
        (When "I press \"RET\"")
        (And "I execute the action chain")))

(When "^I go down$"
      (lambda ()
        (forward-line 1)))

(When "^I go up$"
      (lambda ()
        (forward-line -1)))

(Then "^I should scroll down \"\\(.+\\)\" buffer in other window$"
      (lambda (buffername)
        (should (not (eq (window-start (get-buffer-window buffername)) 1)))))

(Then "^I should scroll up \"\\(.+\\)\" buffer in other window$"
      (lambda (buffername)
        (should (eq (window-start (get-buffer-window buffername)) 1))))

(Then "^the peeped buffer \"\\(.+\\)\" should be killed$"
      (lambda (buffername)
        (should (not (member buffername (mapcar 'buffer-name (buffer-list)))))))

(Then "^the peeped buffer \"\\(.+\\)\" should not be killed$"
      (lambda (buffername)
        (should (member buffername (mapcar 'buffer-name (buffer-list))))))

(Then "the only visible windows are \"\\(.+\\)\" and \"\\(.+\\)\""
      (lambda (dired-buffer peeped-buffer)
        (should (equal
                 (list
                  (get-buffer-window dired-buffer)
                  (get-buffer-window peeped-buffer))
                 (window-list)))))

(Given "^I open \"\\(.+\\)\" file$"
       (lambda (filename)
         (find-file (expand-file-name filename peep-dired-root-path))))

(Then "^the peeped buffers should be killed$"
      (lambda ()
        (should (eq () peep-dired-peeped-buffers))))

(Then "^key \"\\(.+\\)\" should be mapped to \"\\(.+\\)\"$"
      (lambda (key command)
        (should (equal (key-binding (kbd key)) (intern command)))))

(Then "^the mode is enabled for the dired buffer \"\\(.+\\)\"$"
      (lambda (buffername)
        (with-current-buffer buffername
          (should peep-dired))))
