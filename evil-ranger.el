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
;; <https://github.com/asok/peep-dired>, which can only be enabled from a dired
;; buffer. Once enabled it will modify dired to work like ranger, to show the
;; parent direcory of the current folder, and optionally preview the seclected
;; file in the other window. Moving to the other file within the dired buffer
;; with j/k. Hitting <C-j>/<C-k> will scroll the previewed file down / up.

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

(defcustom evil-ranger-excluded-extensions
  '("mkv"
    "iso"
    "mp4"
    "bin"
    "exe"
    "msi")
  "File extensions to not preview"
  :group 'evil-ranger
  :type 'list)

(defcustom evil-ranger-max-preview-size 10
  "File size in MB to prevent preview of files"
  :group 'evil-ranger
  :type 'integer)

(defcustom evil-ranger-show-literal t
  "When t it will show file literally"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-show-dotfiles t
  "When t it will show dotfiles in directory"
  :group 'evil-ranger
  :type 'boolean)

(defcustom evil-ranger-history-length 30
  "When t it will show dotfiles in directory"
  :group 'evil-ranger
  :type 'integer)

(defcustom evil-ranger-parent-depth 2
  "Number of directories up to traverse"
  :group 'evil-ranger
  :type 'integer)

(defcustom evil-ranger-preview-file nil
  "When t preview the selected file"
  :group 'evil-ranger
  :type 'boolean)

;; (make-local-variable 'evil-ranger-preview-file)

(defcustom evil-ranger-width-parents 0.12
  "Fraction of frame width taken by parent windows"
  :group 'evil-ranger
  :type 'float
  )

(defcustom evil-ranger-width-preview 0.55
  "Fraction of frame width taken by preview window"
  :group 'evil-ranger
  :type 'float
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-ranger-mode nil)

(defvar evil-ranger-history-ring ())

(defvar evil-ranger-child-name nil)
(make-local-variable 'evil-ranger-child-name)

(defvar evil-ranger-preview-buffers ()
  "List with buffers of previewed files")

(defvar evil-ranger-preview-window nil)

(defvar evil-ranger-parent-buffers ()
  "List with buffers of parent buffers")

(defvar evil-ranger-parent-dir-hook '(dired-hide-details-mode
                                      evil-ranger-omit           ; ; hide extraneous stuf
                                      auto-revert-mode
                                      evil-ranger-sort
                                      evil-ranger-point-to-child ; ; point to child directory
                                      hl-line-mode               ; ; show line at current file
                                      evil-ranger-parent-click
                                      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-ranger-point-to-child ()
  (when evil-ranger-child-name
    (dired-goto-file evil-ranger-child-name)))

(defun evil-ranger-parent-click ()
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)
  (local-set-key (kbd  "<mouse-1>") 'evil-ranger-find-file))

(evil-define-key 'normal dired-mode-map (kbd "C-p") 'evil-ranger-mode)

;; define keymaps
(evil-define-key 'visual evil-ranger-mode-map "u" 'dired-unmark)
(evil-define-key 'normal evil-ranger-mode-map
  "j"            'evil-ranger-next-file
  "k"            'evil-ranger-prev-file
  (kbd "C-j")    'evil-ranger-scroll-page-down
  (kbd "C-k")    'evil-ranger-scroll-page-up
  "f"            'evil-ranger-search-files
  "i"            'evil-ranger-preview-toggle
  "zi"           'evil-ranger-toggle-literal
  "zh"           'evil-ranger-toggle-dotfiles
  "o"            'evil-ranger-sort-criteria
  "h"            'evil-ranger-up-directory
  "l"            'evil-ranger-find-file
  "q"            'evil-ranger-disable
  "r"            '(lambda ()
                    (interactive)
                    (evil-ranger-setup)
                    (scroll-right)
                    (dired-do-redisplay)
                    (revert-buffer)
                    )
  (kbd "RET")    'evil-ranger-find-file
  "z-"           'evil-ranger-less-parents
  "z+"           'evil-ranger-more-parents
  "v"            'dired-toggle-marks
  "V"            'evil-visual-line
  "S"            'eshell
  "n"            'evil-search-next
  "N"            'evil-search-previous
  (kbd "C-SPC")  'dired-mark)


;; (add-hook 'evil-ranger-hook 'evil-normalize-keymaps)

(defun evil-ranger-search-files ()
  (interactive)
  (if (featurep 'helm)
      (call-interactively 'helm-find-files)
    (call-interactively 'ido-find-file))
  (when (derived-mode-p 'dired-mode)
    (evil-ranger-enable)))

(defun evil-ranger-preview-toggle ()
  "Toggle preview of selected file"
  (interactive)
  (if evil-ranger-preview-file
      (progn
        (when (and evil-ranger-preview-window
                   (window-live-p evil-ranger-preview-window)
                   (window-at-side-p evil-ranger-preview-window 'right))
          (ignore-errors
            (delete-window evil-ranger-preview-window)))
        (dired-hide-details-mode -1)
        (funcall 'add-to-invisibility-spec 'dired-hide-details-information)
        (setq evil-ranger-preview-file nil))
    (progn
      (setq evil-ranger-preview-file t)
      (dired-hide-details-mode t))
    (evil-ranger-setup-preview)))

(defun evil-ranger-toggle-dotfiles ()
  "Show/hide dot-files"
  (interactive)
  (if evil-ranger-show-dotfiles ; if currently showing
      (progn
        (setq evil-ranger-show-dotfiles nil)
        (evil-ranger-hide-dotfiles))
    (progn (revert-buffer) ; otherwise just revert to re-show
           (setq evil-ranger-show-dotfiles t)))
  (message (format "Show Dotfiles: %s"  evil-ranger-show-dotfiles))
  )

(defun evil-ranger-hide-dotfiles ()
  (unless evil-ranger-show-dotfiles
    (dired-mark-files-regexp "^\\\.")
    (dired-do-kill-lines nil "")))

(defun evil-ranger-toggle-literal ()
  (interactive)
  (if evil-ranger-show-literal
      (setq evil-ranger-show-literal nil)
    (setq evil-ranger-show-literal t)
    )
  (ignore-errors
    (mapc 'kill-buffer-if-not-modified evil-ranger-preview-buffers)
    (delete-window evil-ranger-preview-window)
    )
  (when evil-ranger-preview-file
    (evil-ranger-setup-preview))
  (message (format "Literal Preview: %s"  evil-ranger-show-literal))
  )

(defun evil-ranger-sort-criteria (criteria)
  "sort-dired by different criteria"
  (interactive
   (list
    (read-char-choice "criteria [name]: size(S) extension(X) time(t) name(N) -- reverse sort(r) " '(?q ?r ?+ ?- ?G ?E ?D ?S ?X ?t ?N))
    ))
  (unless (eq criteria ?q)
    (dired-sort-other
     (concat dired-listing-switches
             (char-to-string criteria)))
    (run-hooks 'evil-ranger-mode-hook)))

(defun evil-ranger-up-directory ()
  (interactive)
  (let ((parent (evil-ranger-parent-directory default-directory)))
    (when parent
      (evil-ranger-find-file parent))))

(defun evil-ranger-less-parents ()
  (interactive)
  (setq evil-ranger-parent-depth (max 1 (- evil-ranger-parent-depth 1)))
  (evil-ranger-setup)
  )

(defun evil-ranger-more-parents ()
  (interactive)
  (setq evil-ranger-parent-depth (+ evil-ranger-parent-depth 1))
  (evil-ranger-setup)
  )

(defun evil-ranger-find-file (&optional entry)
  (interactive)
  (let ((find-name (or entry
                       (dired-get-filename nil t))))
    (when find-name
      ;; (evil-ranger-enable)
      (unless (file-directory-p find-name)
        (evil-ranger-revert)
        )
      (find-alternate-file find-name)
      (when (file-directory-p find-name)
        (evil-ranger-enable))
      )))

(defun evil-ranger-next-file ()
  (interactive)
  (dired-next-line 1)
  (if (eobp)
      (dired-next-line -1))
  (when evil-ranger-preview-file
    (evil-ranger-setup-preview)
    (when evil-ranger-cleanup-eagerly
      (evil-ranger-cleanup))))

(defun evil-ranger-prev-file ()
  (interactive)
  (dired-previous-line 1)
  (if (bobp)
      (dired-next-line 1))
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
  ;; (ignore-errors
  (with-current-buffer (or
                        (car (or (dired-buffers-for-dir entry-name) ()))
                        (dired-noselect entry-name))
    (when evil-ranger-enable-on-directories
      (run-hooks 'evil-ranger-parent-dir-hook)
      )
    (current-buffer)))
;; )

(defun evil-ranger-preview-buffer (entry-name)
  (with-current-buffer
      (or
       (find-buffer-visiting entry-name)
       (find-file-noselect entry-name nil evil-ranger-show-literal))
    ;; (message (format "%s" (image-type-from-buffer)))
    (current-buffer))
  )

;; (let ((temp-buffer (or (get-buffer "*preview*")
;;                        (generate-new-buffer "*preview*"))))
;;   (with-current-buffer temp-buffer
;;     (erase-buffer)
;;     (insert-file-contents entry-name)
;;     (current-buffer))))



(defun evil-ranger-parent-directory (entry)
  "find the parent directory of entry"
  (file-name-directory (directory-file-name entry)))

(defun evil-ranger-fix-width (window)
  (with-selected-window window
    (setq-local window-size-fixed 'width)
    ))

(defun evil-ranger-display-buffer-at-side (buffer alist)
  "Try displaying BUFFER at one side of the selected frame This splits the
window at the designated `side' of the frame. Accepts `window-width' as a
fraction of the total frame size"
  (let* ((side (or (cdr (assq 'side alist)) 'bottom))
         (slot (or (cdr (assq 'slot alist)) 0))
         ;; (split-side (cond
         ;;              ((eq side 'left) 'right)
         ;;              ((eq side 'right) 'left)
         ;;              ((eq side 'bottom) 'top)
         ;;              ((eq side 'top) 'bottom)))
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (window-size (ceiling  (* (frame-width) window-width)))
         (split-width-threshold 0)
         (current-window window)
         new-window
         reuse-window)

    (walk-window-tree
     (lambda (window)
       (when (eq (window-parameter window 'window-slot) slot)
         (setq reuse-window window)))
     nil nil 'nomini)

    (message (format "%s : %s" slot reuse-window))

    (if reuse-window
        (progn
          (window--display-buffer
           buffer reuse-window 'reuse alist display-buffer-mark-dedicated))
      (progn
        (setq new-window (split-window current-window window-size side))
        (set-window-parameter new-window 'window-slot slot)
        (window--display-buffer
         buffer new-window 'window alist display-buffer-mark-dedicated))
      )

    ))

(defun evil-ranger-setup-parents ()
  (let ((parent-name (evil-ranger-parent-directory default-directory))
        (current-name default-directory)
        (i 0)
        unused-window
        )
    ;; clear out everything
    (cl-loop for buffer in evil-ranger-parent-buffers do
             (unless (get-buffer-window (current-buffer))
               (kill-buffer-if-not-modified buffer)))
    ;; (mapc 'kill-buffer evil-ranger-parent-buffers)
    (setq evil-ranger-parent-buffers ())
    (setq evil-ranger-parent-windows ())
    (setq evil-ranger-parent-dirs ())
    (while (and (file-directory-p parent-name)
                (< i evil-ranger-parent-depth))
      (setq i (+ i 1))
      (if (string-equal current-name parent-name)
          (walk-window-tree
           (lambda (window)
             (when (eq (window-parameter window 'window-slot) (- 0 i))
               (setq unused-window window)
               ))
           nil nil 'nomini)
        (progn
          (add-to-list 'evil-ranger-parent-dirs (cons (cons current-name parent-name) i))
          (setq current-name (evil-ranger-parent-directory current-name))
          (setq parent-name (evil-ranger-parent-directory parent-name))))
      (when (and unused-window
                 (window-live-p unused-window))
        (delete-window unused-window))))
  ;; (message (format "%s" evil-ranger-parent-dirs))
  (mapc 'evil-ranger-make-parent evil-ranger-parent-dirs)
  ;; (mapc 'evil-ranger-fix-width evil-ranger-parent-windows)
  )

(defun evil-ranger-make-parent (parent)
  (let* ((parent-name (cdar parent))
         (current-name (caar parent))
         (slot (cdr parent))
         (parent-window
          (display-buffer
           (evil-ranger-dir-buffer parent-name)
           `(evil-ranger-display-buffer-at-side . ((side . left)
                                                   (slot . ,(- 0 slot))
                                                   ;; (inhibit-same-window . t)
                                                   (window-width . ,evil-ranger-width-parents)))))
         (parent-buffer (window-buffer parent-window)))
    (setq evil-ranger-child-name current-name)
    (add-to-list 'evil-ranger-parent-buffers parent-buffer)
    (add-to-list 'evil-ranger-parent-windows parent-window)))

(defun evil-ranger-setup-preview ()
  (let* ((entry-name (dired-get-filename nil t))
         (fsize
          (nth 7 (file-attributes entry-name))))
    (when (and evil-ranger-preview-window
               (window-live-p evil-ranger-preview-window)
               ;; (window-at-side-p evil-ranger-preview-window 'right)
               )
      (ignore-errors (delete-window evil-ranger-preview-window)))
    (when (and entry-name
               evil-ranger-preview-file)
      (unless (or
               (> fsize (* 1024 1024 evil-ranger-max-preview-size))
               (member (file-name-extension entry-name)
                       evil-ranger-excluded-extensions))
        (let* ((preview-window (display-buffer
                                (if (file-directory-p entry-name)
                                    (evil-ranger-dir-buffer entry-name)
                                  (evil-ranger-preview-buffer entry-name))
                                `(evil-ranger-display-buffer-at-side . ((side . right)
                                                                        (slot . 1)
                                                                        ;; (inhibit-same-window . t)
                                                                        (window-width . ,evil-ranger-width-preview)))))
               (preview-buffer
                (window-buffer preview-window)))
          (add-to-list 'evil-ranger-preview-buffers preview-buffer)
          (setq evil-ranger-preview-window preview-window)
          (dired-hide-details-mode t)
          )))
    ;; (message (format "%s" (window-tree)))
    ))

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
  (delete-other-windows)
  (unless (derived-mode-p 'dired-mode)
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
  (evil-ranger-setup-parents)
  (evil-ranger-setup-preview)
  )

(defun evil-ranger-omit ()
  (let ((dired-omit-verbose nil)) 
    (dired-omit-mode t)))

(defun evil-ranger-sort ()
  )

(defun evil-ranger-header-line ()
  ;; (message
  (concat
   (propertize
    (file-relative-name evil-ranger-child-name (evil-ranger-parent-directory evil-ranger-child-name))
    'face
    '(
      :background "#ffffff"
                  :foreground "#000000"
                  :weight bold
                  )
    )
   ;; (format " pw:%s pb:%s w:%s b:%s "
   ;;         ;; evil-ranger-parent-dirs
   ;;         evil-ranger-preview-window
   ;;         evil-ranger-preview-buffers
   ;;         evil-ranger-parent-windows
   ;;         evil-ranger-parent-buffers
   ;;         )
   )
  ;; )
  )

(add-hook 'evil-ranger-mode-hook 'evil-normalize-keymaps)
(add-hook 'evil-ranger-mode-hook 'evil-ranger-hide-dotfiles)
(add-hook 'evil-ranger-mode-hook 'evil-ranger-omit)
(add-hook 'evil-ranger-mode-hook 'evil-ranger-sort)
(add-hook 'evil-ranger-mode-hook 'auto-revert-mode)


(defun evil-ranger-revert ()
  (let ((current-point (point)))
    (setq header-line-format nil)
    (when (get-register :ranger_dired_before)
      (ignore-errors
        (jump-to-register :ranger_dired_before))
      (set-register :ranger_dired_before nil))
    (when evil-ranger-cleanup-on-disable
      (mapc 'kill-buffer-if-not-modified evil-ranger-preview-buffers))
    (when evil-ranger-cleanup-on-disable
      (mapc 'kill-buffer-if-not-modified evil-ranger-parent-buffers))
    (setq evil-ranger-preview-buffers ()
          evil-ranger-parent-buffers ())
    (goto-char current-point))
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
        (unless (derived-mode-p 'dired-mode)
          (error "Run it from dired buffer"))

        ;; (message (format "%s" (register-read-with-preview "Prompt")))
        (unless (get-register :ranger_dired_before)
          (window-configuration-to-register :ranger_dired_before))
        (setq evil-ranger-preview-window nil)

        (dired-hide-details-mode -1)
        ;; hide details line at top
        (funcall 'add-to-invisibility-spec 'dired-hide-details-information)
        ;; (delete-other-windows)
        (evil-ranger-setup)

        ;; (add-hook 'dired-after-readin-hook #'evil-ranger-enable)

        ;; (defadvice find-file (before evil-ranger-find-file activate)
        ;;   (evil-ranger-disable))

        ;; (defadvice dired-find-file (after evil-ranger-find-file activate)
        ;;   (evil-ranger-enable))

        ;; (defadvice quit-window (before evil-ranger-quit activate)
        ;;   (when evil-ranger-mode (evil-ranger-disable)))

        ;; (add-hook 'dired-mode-hook #'evil-ranger-mode)
        (make-local-variable 'header-line-format)
        (setq header-line-format '(:eval (evil-ranger-header-line)))

        ;; (add-hook 'window-size-change-functions #'(lambda (window) (when evil-ranger-mode evil-ranger-setup)))
        ;; (setq window-size-change-functions '())
        ;; (add-hook 'dired-mode-hook #'evil-ranger-enable)
        )
    (progn
      (evil-ranger-revert) 
      (ignore-errors
        ;; (ad-remove-advice 'find-file 'before 'evil-ranger-find-file)
        ;; (ad-remove-advice 'quit-window 'before 'evil-ranger-quit)
        ;; (ad-remove-advice 'dired-find-file 'after 'evil-ranger-quit)
        )
      )))

(provide 'evil-ranger)

;;; evil-ranger.el ends here
