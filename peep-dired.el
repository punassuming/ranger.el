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

(defvar peep-dired-peeped-buffers ()
  "List with buffers of peeped files")

(defcustom peep-dired-cleanup-on-disable t
  "Cleanup opened buffers when disabling the minor mode"
  :group 'peep-dired
  :type 'boolean)

(defun peep-dired-next-file ()
  (interactive)
  (dired-next-line 1)
  (peep-dired-display-file-other-window))

(defun peep-dired-prev-file ()
  (interactive)
  (dired-previous-line 1)
  (peep-dired-display-file-other-window))

(defun peep-dired-display-file-other-window ()
  (add-to-list 'peep-dired-peeped-buffers
               (window-buffer
                (display-buffer
                 (find-file-noselect
                  (dired-file-name-at-point))))))

(defun peep-dired-scroll-page-down ()
  (interactive)
  (scroll-other-window))

(defun peep-dired-scroll-page-up ()
  (interactive)
  (scroll-other-window '-))

(defun peep-dired-disable ()
  (let ((current-point (point)))
    (jump-to-register :peep_dired_before)
    (when peep-dired-cleanup-on-disable
      (mapc 'kill-buffer-if-not-modified peep-dired-peeped-buffers))
    (goto-char current-point)))

(defun peep-dired-enable ()
  (unless (string= major-mode "dired-mode")
    (error "Run it from dired buffer"))

  (window-configuration-to-register :peep_dired_before)
  (make-local-variable 'peep-dired-peeped-buffers)
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
