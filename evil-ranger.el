;;; evil-ranger.el --- Make dired more like ranger

;; Copyright (C) 2015  Rich Alesi
;; Copyright (C) 2014  Adam Sokolnicki

;; Author : Rich Alesi <https://github.com/ralesi>
;; Original Author: Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; Keywords: files, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a minor mode forked from peep-dired
;; <https://github.com/asok/peep-dired> This is a minor mode that can be
;; enabled from a dired buffer. Once enabled it will show the file from
;; point in the other window. Moving to the other file within the dired
;; buffer with <down>/<up> or C-n/C-p will display different file.
;; Hitting <SPC> will scroll the previewed file down, whereas C-<SPC> and
;; <backspace> will scroll it up.

;;; Code:

(require 'cl-macs)
(require 'evil)

(defcustom evil-ranger-cleanup-on-disable t
  "Cleanup opened buffers when disabling the minor mode"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-cleanup-eagerly nil
  "Cleanup opened buffers upon `evil-ranger-next-file' & `evil-ranger-prev-file'"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-enable-on-directories t
  "When t it will enable the mode when visiting directories"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-ignored-extensions
  '("mkv" "iso" "mp4")
  "Extensions to not try to open"
  :group 'evil-ranger
  :type 'list)

(defcustom evil-ranger-show-literal t
  "When t it will show file literally"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-parent-depth 2
  "Number of directories up to traverse"
  :group 'evil-ranger
  :type 'integer)

(defcustom evil-ranger-preview-file nil
  "When t preview the selected file"
  :group 'evil-ranger
  :type 'boolean)

(make-local-variable 'evil-ranger-preview-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-ranger-mode nil)

(defvar evil-ranger-child-name nil)

(make-local-variable 'evil-ranger-child-name)

(defvar evil-ranger-preview-buffers ()
  "List with buffers of previewed files")

(defvar evil-ranger-preview-window nil)

(defvar evil-ranger-parent-buffers ()
  "List with buffers of parent buffers")

(defvar evil-ranger-parent-dir-hook '(dired-hide-details-mode
                                      evil-ranger-omit
                                      evil-ranger-point-to-child
                                      hl-line-mode
                                      evil-ranger-parent-click
                                      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-ranger-point-to-child ()
  (when evil-ranger-child-name
    (dired-goto-file evil-ranger-child-name)))

(defun evil-ranger-parent-click ()
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)
  (local-set-key (kbd  "<mouse-1>") 'dired-find-file))

(evil-define-key 'normal dired-mode-map (kbd "C-p") 'evil-ranger-mode)

;; define keymaps
(evil-define-key 'visual evil-ranger-mode-map "u" 'dired-unmark)
(evil-define-key 'normal evil-ranger-mode-map
  "j"            'evil-ranger-next-file
  "k"            'evil-ranger-prev-file
  (kbd "C-j")    'evil-ranger-scroll-page-down
  (kbd "C-k")    'evil-ranger-scroll-page-up
  "f"            'helm-find-files
  "i"            'evil-ranger-preview
  "h"            'evil-ranger-up-directory
  "l"            'evil-ranger-find-file
  "r"            '(lambda ()
                    (interactive)
                    (evil-ranger-setup)
                    (scroll-right)
                    (dired-do-redisplay)
                    (revert-buffer)
                    )
  (kbd "RET")    'evil-ranger-find-file
  "v"            'dired-toggle-marks
  "V"            'evil-visual-line
  "S"            'eshell
  "n"            'evil-search-next
  "N"            'evil-search-previous
  (kbd "C-SPC")  'dired-mark)

(add-hook 'evil-ranger-mode-hook 'evil-normalize-keymaps)

;; (add-hook 'evil-ranger-hook 'evil-normalize-keymaps)

(defun evil-ranger-preview ()
  "Toggle preview of selected file"
  (interactive)
  (if (eq evil-ranger-preview-file t)
      (progn
        (when (and evil-ranger-preview-window
                   (window-live-p evil-ranger-preview-window)
                   (window-at-side-p evil-ranger-preview-window 'right))
          (delete-window evil-ranger-preview-window)
          (dired-hide-details-mode -1)
          )
        (setq evil-ranger-preview-file nil))
    (setq evil-ranger-preview-file t)
    (dired-hide-details-mode t)
    (evil-ranger-setup-preview)
    )
  )

(defun evil-ranger-up-directory ()
  (interactive)
  (evil-ranger-disable)
  (dired-up-directory)
  (evil-ranger-enable)
  )

(defun evil-ranger-find-file ()
  (interactive)
  (let ((entry-name (dired-get-filename nil t)))
    ;; (delete-other-windows)
    (when entry-name
      (evil-ranger-disable)
      (find-file entry-name)
      (when (file-directory-p entry-name)
        (evil-ranger-enable)))
    ))

(defun evil-ranger-next-file ()
  (interactive)
  (dired-next-line 1)
  (when evil-ranger-preview-file
    (evil-ranger-setup-preview)
    (when evil-ranger-cleanup-eagerly
      (evil-ranger-cleanup))))

(defun evil-ranger-prev-file ()
  (interactive)
  (dired-previous-line 1)
  (when evil-ranger-preview-file
    (evil-ranger-setup-preview)
    (when evil-ranger-cleanup-eagerly
      (evil-ranger-cleanup))))

(defun evil-ranger-kill-buffers-without-window ()
  "Will kill all ranger buffers that are not displayed in any window"
  (interactive)
  (cl-loop for buffer in evil-ranger-parent-buffers do
           (unless (get-buffer-window buffer t)
             (kill-buffer-if-not-modified buffer)))
  (cl-loop for buffer in evil-ranger-preview-buffers do
           (unless (get-buffer-window buffer t)
             (kill-buffer-if-not-modified buffer))))

(defun evil-ranger-dir-buffer (entry-name)
  (with-current-buffer (or
                        (car (or (dired-buffers-for-dir entry-name) ()))
                        (dired-noselect entry-name))
    (when evil-ranger-enable-on-directories
      (run-hooks 'evil-ranger-parent-dir-hook))
    (current-buffer)))

(defun evil-ranger-parent-directory (entry)
  "find the parent directory of entry"
  (file-name-directory (directory-file-name entry)))

(defun evil-ranger-fix-width (window)
  (with-selected-window window
    (setq-local window-size-fixed 'width)
    ))

(defun evil-ranger-display-buffer-at-left (buffer alist)
  "Try displaying BUFFER in a window at the bottom of the selected frame.
This either splits the window at the bottom of the frame or the
frame's root window, or reuses an existing window at the bottom
of the selected frame."
  (let ((side (or (cdr (assq 'side alist)) 'bottom))
        (left-window window)
        (split-width-threshold 10)
        (window-min-width 1)
        (width (max 14 (ceiling  (* (frame-width) 0.12))))
        )
    (cond
     ((not (memq side '(top bottom left right)))
      (error "Invalid side %s specified" side)))
    ;; (setq left-window (window-left ))
    ;; (while (window-left left-window)
    ;;   (setq left-window (window-left left-window)))
    ;; (walk-window-tree
    ;;  (lambda (window) (unless left-window (setq left-window window))) nil nil 'nomini)
    ;; (message (format "%s" (ceiling  (* (frame-width) size))))
    (or (and (not (frame-parameter nil 'unsplittable))
             (setq window (ignore-errors (split-window left-window width side)))
             (window--display-buffer
              buffer window 'window alist display-buffer-mark-dedicated)
             )
        ;; (and (not (frame-parameter nil 'unsplittable))
        ;;      (setq window
        ;;            (condition-case nil
        ;;                (split-window (window--major-non-side-window))
        ;;              (error nil)))
        ;;      (window--display-buffer
        ;;       buffer window 'window alist display-buffer-mark-dedicated))
        ;; (and (setq window left-window)
        ;;      (not (window-dedicated-p window))
        ;;      (window--display-buffer
        ;;       buffer window 'reuse alist display-buffer-mark-dedicated))
        )))

(defun evil-ranger-setup-parents ()
  (let ((parent-name (evil-ranger-parent-directory default-directory))
        (current-name default-directory)
        ;; (split-width-threshold 10)
        (even-window-heights nil)
        (window-min-width 1)
        (i 0)
        )
    ;; clear out everything
    (delete-other-windows)
    (mapc 'kill-buffer evil-ranger-parent-buffers)
    (setq evil-ranger-parent-buffers ())
    (setq evil-ranger-parent-windows ())
    (setq evil-ranger-parent-dirs ())
    (while (and (file-directory-p parent-name)
                (< i evil-ranger-parent-depth))
      (setq i (+ i 1))
      (unless (string-equal current-name parent-name)
        (add-to-list 'evil-ranger-parent-dirs (cons current-name parent-name))
        (setq current-name (evil-ranger-parent-directory current-name))
        (setq parent-name (evil-ranger-parent-directory parent-name)))))
  ;; (message (format "%s" evil-ranger-parent-dirs))
  (mapc 'evil-ranger-make-parent evil-ranger-parent-dirs)
  ;; (mapc 'evil-ranger-fix-width evil-ranger-parent-windows)
  )

(defun evil-ranger-make-parent (parent)
  (let* ((parent-name (cdr parent))
         (current-name (car parent))
         (parent-window
          (display-buffer
           (evil-ranger-dir-buffer parent-name)
           `(evil-ranger-display-buffer-at-left . ((side . left)
                                                   (inhibit-same-window . t)
                                                   (window-width . 0.12)))))
         (parent-buffer (window-buffer parent-window)))
    (setq evil-ranger-child-name current-name)
    (add-to-list 'evil-ranger-parent-buffers parent-buffer)
    (add-to-list 'evil-ranger-parent-windows parent-window)
    ))

(defun evil-ranger-setup-preview ()
  (let ((entry-name (dired-get-filename nil t))
        (window-min-width 1)
        (even-window-heights nil))
    (when (and evil-ranger-preview-window
               (window-live-p evil-ranger-preview-window)
               (window-at-side-p evil-ranger-preview-window 'right)
               )
      (ignore-errors (delete-window evil-ranger-preview-window)))
    (when (and entry-name
               evil-ranger-preview-file)
      (unless (member (file-name-extension entry-name)
                      evil-ranger-ignored-extensions)
        (let* ((win (display-buffer
                     (if (file-directory-p entry-name)
                         (evil-ranger-dir-buffer entry-name)
                       (or
                        (find-buffer-visiting entry-name)
                        (find-file-noselect entry-name nil evil-ranger-show-literal)))
                     `(display-buffer-in-side-window . ((side . right)
                                                        ;; (inhibit-same-window . t)
                                                        ;; (window . ,atom-root)
                                                        ;; (slot . 0)
                                                        (window-width . 0.42)))))
               (wb
                (window-buffer win)))
          (add-to-list 'evil-ranger-preview-buffers wb)
          (setq evil-ranger-preview-window win)))
      ;; (message (format "%s" (window-tree)))
      )))

(defun evil-ranger-scroll-page-down ()
  (interactive)
  (scroll-other-window))

(defun evil-ranger-scroll-page-up ()
  (interactive)
  (scroll-other-window '-))

(defun evil-ranger-cleanup ()
  (mapc #'(lambda (window) (ignore-errors (delete-window window))
            evil-ranger-parent-windows))
  (setq evil-ranger-parent-windows ())
  (mapc 'kill-buffer-if-not-modified evil-ranger-parent-buffers)
  (setq evil-ranger-parent-buffers ())
  (mapc 'kill-buffer-if-not-modified evil-ranger-preview-buffers)
  (setq evil-ranger-preview-buffers ())
  )

;;;###autoload
(defun evil-ranger ()
  "Launch dired in evil-ranger-minor-mode"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (dired-jump))
  (evil-ranger-mode t)
  )

(defun evil-ranger-enable ()
  (interactive)
  (evil-ranger-mode t))

(defun evil-ranger-disable ()
  (interactive)
  (evil-ranger-mode -1))

(defun evil-ranger-setup ()
  (interactive)
  (delete-other-windows)
  (evil-ranger-setup-parents)
  (evil-ranger-setup-preview)
  )

(add-hook 'evil-ranger-mode-hook #'evil-ranger-omit)

(defun evil-ranger-omit ()
  (setq dired-omit-verbose nil)
  (dired-omit-mode t)
  )

;;;###autoload
(define-minor-mode evil-ranger-mode
  "A convienent way to look up file contents in other window while browsing directory in dired"
  :init-value nil
  :lighter " Ranger"
  :keymap (make-sparse-keymap)
  :group 'evil-ranger
  ;; :after-hook 'evil-ranger-mode-hook

  ;; only run from dired-mode
  (if evil-ranger-mode
      (progn
        (unless (string= major-mode "dired-mode")
          (error "Run it from dired buffer"))

        ;; (message (format "%s" (register-read-with-preview "Prompt")))
        (unless (get-register :ranger_dired_before)
          (window-configuration-to-register :ranger_dired_before))
        (setq evil-ranger-preview-window nil)
        (evil-ranger-setup)
        (dired-hide-details-mode -1)

        ;; (add-hook 'dired-after-readin-hook #'evil-ranger-enable)

        (defadvice find-file (before evil-ranger-find-file activate)
          (evil-ranger-disable))

        (defadvice dired-find-file (after evil-ranger-find-file activate)
          (evil-ranger-enable))

        (defadvice quit-window (before evil-ranger-quit activate)
          (when evil-ranger-mode (evil-ranger-disable)))

        ;; (add-hook 'dired-mode-hook #'evil-ranger-mode)
        (add-hook 'dired-mode-hook #'auto-revert-mode)
        ;; (add-hook 'window-size-change-functions #'(lambda (window) (when evil-ranger-mode evil-ranger-setup)))
        ;; (setq window-size-change-functions '())
        ;; (add-hook 'dired-mode-hook #'evil-ranger-enable)
        )
    (progn
      (let ((current-point (point)))
        ;; (remove-hook 'dired-after-readin-hook #'evil-ranger-enable)
        (remove-hook 'dired-mode-hook #'auto-revert-mode)
        ;; (remove-hook 'dired-mode-hook #'evil-ranger-enable)
        (when (get-register :ranger_dired_before)
          (jump-to-register :ranger_dired_before)
          (set-register :ranger_dired_before nil))
        (when evil-ranger-cleanup-on-disable
          (mapc 'kill-buffer-if-not-modified evil-ranger-preview-buffers))
        (setq evil-ranger-preview-buffers ()
              evil-ranger-parent-buffers ())
        (goto-char current-point))
      ;; remove find-file advice
      (ignore-errors
        (ad-remove-advice 'find-file 'before 'evil-ranger-find-file)
        (ad-remove-advice 'quit-window 'before 'evil-ranger-quit)
        (ad-remove-advice 'dired-find-file 'after 'evil-ranger-quit)
        )
      )))

(provide 'evil-ranger)

;;; evil-ranger.el ends here
