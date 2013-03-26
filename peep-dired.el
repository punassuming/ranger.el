(defvar peep-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>")      'peep-dired-next-file)
    (define-key map (kbd "C-n")         'peep-dired-next-file)
    (define-key map (kbd "<up>")        'peep-dired-prev-file)
    (define-key map (kbd "C-p")         'peep-dired-prev-file)
    (define-key map (kbd "<SPC>")       'peep-dired-scroll-page-down)
    (define-key map (kbd "C-<SPC>")     'peep-dired-scroll-page-up)
    (define-key map (kbd "<backspace>") 'peep-dired-scroll-page-up)
    map)
  "Keymap for `peep-dired-mode'.")

(defvar peep-dired-mode-hook nil
  "Hook for `peep-dired-mode'.")

(defun peep-dired-next-file ()
  (interactive)
  (dired-next-line 1)
  (peep-dired-display-file-other-window))

(defun peep-dired-prev-file ()
  (interactive)
  (dired-previous-line 1)
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
  (let ((current-point (point)))
    (jump-to-register :peep_dired_before)
    (goto-char current-point)))

(defun peep-dired-enable ()
  (unless (string= major-mode "dired-mode")
    (error "Run it from dired buffer"))

  (window-configuration-to-register :peep_dired_before)
  (peep-dired-display-file-other-window)
  (run-hooks 'peep-dired-mode-hook))

;;;###autoload
(define-minor-mode peep-dired
  "A convienent way to look up file contents in other window while browsing directory in dired"
  :init-value nil
  :lighter " Peep"
  :keymap peep-dired-mode-map

  (if peep-dired
      (peep-dired-enable)
    (peep-dired-disable)))

(provide 'peep-dired)
