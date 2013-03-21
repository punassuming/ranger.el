(defvar peep-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<SPC>")       'peep-dired-scroll-page-down)
    (define-key map (kbd "C-<SPC>")     'peep-dired-scroll-page-up)
    (define-key map (kbd "<backspace>") 'peep-dired-scroll-page-up)
    map)
  "Keymap for `peep-dired-mode'.")

(defvar peep-dired-mode-hook nil
  "Hook for `peep-dired-mode'.")

(defadvice dired-next-line (after peep-dired-ad disable)
  (peep-dired-display-file-other-window))

(defadvice dired-previous-line (after peep-dired-ad disable)
  (peep-dired-display-file-other-window))

(defun peep-dired-display-file-other-window ()
    (display-buffer (find-file-noselect (dired-file-name-at-point))))

(defun peep-dired-scroll-page-down ()
  (interactive)
  (scroll-other-window))

(defun peep-dired-scroll-page-up ()
  (interactive)
  (scroll-other-window '-))

(defun peep-dired-disable ()
  (ad-disable-advice 'dired-next-line 'after 'peep-dired-ad)
  (ad-activate 'dired-next-line)

  (ad-disable-advice 'dired-previous-line 'after 'peep-dired-ad)
  (ad-activate 'dired-previous-line)

  (let ((current-point (point)))
    (jump-to-register :peep_dired_before)
    (goto-char current-point)))

(defun peep-dired-enable ()
  (unless (string= major-mode "dired-mode")
    (error "Run it from dired buffer"))

  (window-configuration-to-register :peep_dired_before)

  (peep-dired-display-file-other-window)

  (ad-enable-advice 'dired-next-line 'after 'peep-dired-ad)
  (ad-activate 'dired-next-line)
  
  (ad-enable-advice 'dired-previous-line 'after 'peep-dired-ad)
  (ad-activate 'dired-previous-line)

  (run-hooks 'peep-dired-mode-hook))

(define-minor-mode peep-dired
  "A convienent way to look up file contents in other window while browsing directory in dired"
  :init-value nil
  :lighter " Peep"
  :keymap peep-dired-mode-map

  (if peep-dired
      (peep-dired-enable)
    (peep-dired-disable)
    )
  )

(provide 'peep-dired)
