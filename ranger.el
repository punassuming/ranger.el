;;; ranger.el --- Make dired more like ranger

;; Copyright (C) 2015  Rich Alesi
;; Copyright (C) 2014  Adam Sokolnicki (peep-dired)

;; Author : Rich Alesi <https://github.com/ralesi>
;; Version: 0.9.8
;; Keywords: files, convenience
;; Homepage: https://github.com/ralesi/ranger
;; Package-Requires: ((emacs "24.4")(cl-lib "0.5"))

;; Based on work from
;; peep-dired - Author: Adam Sokolnicki <adam.sokolnicki@gmail.com>

;;; License:

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

;; This is a minor mode that runs within dired emulating many of the features of
;; ranger. This minor mode shows a stack of the parent directories and updates
;; the parent buffers while nvaigating the file system. The preview window takes
;; some of the ideas from Peep-Dired <https://github.com/asok/peep-dired> to
;; display previews for selected files in the primary dired buffer. This package
;; tries its best to make a seamless user experience from ranger created for
;; python.

;;; FEATURES

;; Replaces dired buffer with features from Ranger
;; - show window stack of parent directories
;; - show preview window of selected directory or file
;; - fast navigation using vi-like keybindings
;; - move through navigation history
;; - copy and paste functionality utilizing a copy ring

;;; KNOWN ISSUES

;; - window specific settings neededs
;;  - current tab
;;  - history
;;  - current-file

;;; HISTORY

;; version 0.9.1, 2015-07-19 changed package to ranger
;; version 0.9.2, 2015-07-26 improve exit from ranger, bookmark support
;; version 0.9.4, 2015-07-31 deer mode, history navigation
;; version 0.9.5, 2015-08-20 fixed most bugs when reverting from ranger
;; version 0.9.6, 2015-09-11 delete all accessed buffers, add details to echo
;; version 0.9.7, 2015-09-13 copy and paste functionality added
;; version 0.9.8, 2015-10-04 multiple ranger window support, override dired

;;; Code:

(declare-function dired-omit-mode "dired-x")
(declare-function dired-kill-tree "dired-aux")
(declare-function image-dired-display-window-height "image-dired")
(declare-function image-dired-display-window-width "image-dired")
(declare-function image-dired-create-display-image-buffer "image-dired")
(declare-function image-dired-insert-image "image-dired")
(declare-function image-dired-update-property "image-dired")
(declare-function format-spec "format-spec")

(require 'cl-lib)
(require 'dired)
(require 'hl-line)
(require 'autorevert)
(require 'bookmark)
(require 'ring)

(require 'subr-x nil t)
(require 'diminish nil t)

;; include string-join
(unless (featurep 'subr-x)
  (defun string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator)))

(defgroup ranger ()
  "Modify dired to act like ranger."
  :group 'ranger
  :prefix "ranger-")

;; directory options
(defcustom ranger-cleanup-on-disable t
  "Cleanup opened buffers when disabling the minor mode."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-cleanup-eagerly nil
  "Cleanup opened buffers upon `ranger-next-file' & `ranger-prev-file'."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-show-dotfiles t
  "When t it will show dotfiles in directory."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-history-length 30
  "Length of history ranger will track."
  :group 'ranger
  :type 'integer)

(defcustom ranger-copy-ring-length 10
  "How many sets of files stored for copy and paste."
  :group 'ranger
  :type 'integer)

(defcustom ranger-parent-depth 1
  "Number of directories up to traverse."
  :group 'ranger
  :type 'integer)

;; preview options
(defcustom ranger-excluded-extensions
  '("mkv"
    "iso"
    "mp4"
    "bin"
    "exe"
    "msi")
  "File extensions to not preview."
  :group 'ranger
  :type 'list)

(defcustom ranger-modify-header t
  "Modify the header to style like ranger."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-max-preview-size 10
  "File size in MB to prevent preview of files."
  :group 'ranger
  :type 'integer)

(defcustom ranger-show-literal t
  "When non-nil it will show file literally."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-persistent-sort nil
  "When non-nil, sort all directories with the current flags."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-preview-file t
  "When t preview the selected file."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'ranger
  :type 'float)

;; parent options
(defcustom ranger-width-parents 0.12
  "Fraction of frame width taken by parent windows"
  :group 'ranger
  :type 'float)

(defcustom ranger-max-parent-width 0.42
  "The max width allocated to showing parent windows."
  :group 'ranger
  :type 'float)

(defcustom ranger-key "C-p"
  "`dired-mode' key used to toggle `ranger-mode'"
  :group 'ranger
  :type 'string)

(defcustom ranger-max-tabs 9
  "Maximum number of tabs to allow ranger to maintain."
  :group 'ranger
  :type 'integer)

;; header functions
(defcustom ranger-header-func 'ranger-header-line
  "Function used to output header of primary ranger window.
Outputs a string that will show up on the header-line."
  :group 'ranger
  :type 'function)

(defcustom ranger-parent-header-func 'ranger-parent-header-line
  "Function used to output header of primary ranger window.
Outputs a string that will show up on the header-line."
  :group 'ranger
  :type 'function)

(defcustom ranger-preview-header-func 'ranger-preview-header-line
  "Function used to output header of primary ranger window.
Outputs a string that will show up on the header-line."
  :group 'ranger
  :type 'function)

(defcustom ranger-tabs-style 'normal
  "Specify style to display tabs in ranger."
  :group 'ranger
  :type '(radio (const :tag "Normal" :value normal)
                (const :tag "Roman numerals" :value roman)
                (const :tag "Numbers only" :value numbers)))



(defvar ranger-mode)
(defvar dired-omit-verbose)
(defvar dired-omit-mode)
(defvar image-dired-temp-image-file)
(defvar image-dired-cmd-create-temp-image-program)
(defvar image-dired-cmd-create-temp-image-options)
(defvar image-dired-display-image-buffer)

;; global variables

(defvar ranger-sorting-switches nil)

(defvar ranger-window nil)
(defvar ranger-buffer nil)
(defvar ranger-frame nil)

(defvar ranger-windows-alist ()
  "List of windows using ranger")

(defvar ranger-frames-alist ()
  "List of frames using ranger")

(defvar ranger-tabs-alist ()
  "List of tabs to keep track of in ranger.")

(defvar ranger-history-index 0)

(defvar ranger-history-ring (make-ring ranger-history-length))

(defvar ranger-copy-ring (make-ring ranger-copy-ring-length))

(defvar ranger-image-scale-ratio 1.3)
(defvar ranger-image-fit-window nil)

(defvar ranger-pre-alist)
(defvar ranger-pre-saved nil)
(defvar ranger-pre-hl-mode nil)
(defvar ranger-pre-arev-mode nil)
(defvar ranger-pre-omit-mode nil)
(defvar ranger-pre-dired-listing nil)

(defvar ranger-preview-window nil)
(defvar ranger-preview-buffers ()
  "List with buffers of previewed files.")

(defvar ranger-parent-windows nil)
(defvar ranger-parent-buffers ()
  "List with buffers of parent buffers.")
(defvar ranger-parent-dirs nil)

(defvar ranger-visited-buffers ()
  "List of buffers visited in ranger")

;; frame specific variables
(defvar ranger-minimal nil)

(defvar ranger-subdir-p nil)

;; buffer local variables

(defvar ranger-current-tab 1)

(defvar ranger-current-file nil)
;; (make-variable-buffer-local 'ranger-current-file)

(defvar ranger-child-name nil)
(make-variable-buffer-local 'ranger-child-name)

;; hooks
(defvar ranger-mode-load-hook nil)
(defvar ranger-parent-dir-hook '(revert-buffer
                                 dired-hide-details-mode
                                 ranger-sort
                                 ranger-hide-dotfiles
                                 ranger-omit
                                 auto-revert-mode
                                 ranger-sub-window-setup))

;; maps
(defvar ranger-mode-map (make-sparse-keymap))


;; mapping macro
(defmacro ranger-map (key func)
  "Define macro to bind evil and emacs state for ranger"
  `(progn
     (eval-after-load 'evil
       #'(evil-define-key 'normal ranger-mode-map ,key ,func))
     (define-key ranger-mode-map ,key ,func)))

;; mappings
(when ranger-key
  (with-eval-after-load "evil"
    (evil-define-key 'normal dired-mode-map (kbd ranger-key) 'ranger-mode))
  (define-key ranger-mode-map (kbd ranger-key) 'ranger-mode))

(defun ranger-define-maps ()
  "Define mappings for ranger-mode."

  ;; make sure ranger normalizes current mappings
  (when (featurep 'evil)
    ;; turn off evilified buffers for evilify usage
    (progn
      (when (and (fboundp 'evil-evilified-state-p)
                 (evil-evilified-state-p))
        (evil-evilified-state -1))
      (evil-normal-state)
      (evil-normalize-keymaps)
      (add-hook 'ranger-mode-hook 'evil-normalize-keymaps)))

  (with-eval-after-load "evil"
    ;; some evil specific bindings
    (evil-define-key 'visual ranger-mode-map "u" 'dired-unmark)
    (evil-define-key 'normal ranger-mode-map
      "V"            'evil-visual-line
      "n"            'evil-search-next
      "N"            'evil-search-previous))

  (ranger-map "?"           'ranger-help)
  (ranger-map "'"           'ranger-show-size)
  (ranger-map "!"           'shell-command)
  (ranger-map "B"           'ranger-show-bookmarks)
  (ranger-map "D"           'dired-do-delete)
  (ranger-map "G"           'ranger-goto-bottom)
  (ranger-map "H"           'ranger-prev-history)
  (ranger-map "I"           'ranger-insert-subdir)
  (ranger-map "J"           'ranger-next-subdir)
  (ranger-map "K"           'ranger-prev-subdir)
  (ranger-map "L"           'ranger-next-history)
  (ranger-map "R"           'dired-do-rename)
  (ranger-map "S"           'eshell)
  (ranger-map "["           'ranger-prev-parent)
  (ranger-map "]"           'ranger-next-parent)
  (ranger-map "f"           'ranger-search-files)
  (ranger-map "gg"          'ranger-goto-top)
  (ranger-map "gh"          'ranger-go-home)
  (ranger-map "h"           'ranger-up-directory)
  (ranger-map "-"           'ranger-up-directory)
  (ranger-map "i"           'ranger-preview-toggle)
  (ranger-map "j"           'ranger-next-file)
  (ranger-map "k"           'ranger-prev-file)
  (ranger-map "l"           'ranger-find-file)
  (ranger-map "m"           'ranger-create-mark)
  (ranger-map "o"           'ranger-sort-criteria)
  (ranger-map "ws"          'ranger-open-file-vertically)
  (ranger-map "wv"          'ranger-open-file-horizontally)
  (ranger-map "wf"          'ranger-open-file-frame)
  (ranger-map "we"          'ranger-open-in-external-app)
  (ranger-map "q"           'ranger-disable)
  (ranger-map "u"           'dired-unmark)
  (ranger-map "v"           'dired-toggle-marks)
  (ranger-map "zz"          'ranger-show-history)

  ;; copy and paste
  (ranger-map "yy"          'ranger-copy)
  (ranger-map "dd"          'ranger-cut)
  (ranger-map "pp"          'ranger-paste)
  (ranger-map "po"          'ranger-paste-over)
  (ranger-map "p?"          'ranger-show-copy-contents)

  ;; settings
  (ranger-map "z+"          'ranger-more-parents)
  (ranger-map "z-"          'ranger-less-parents)
  (ranger-map "zh"          'ranger-toggle-dotfiles)
  (ranger-map "zi"          'ranger-toggle-literal)
  (ranger-map "zp"          'ranger-minimal-toggle)
  (ranger-map "zf"          'ranger-toggle-scale-images)

  ;; tabs
  (ranger-map "gn"          'ranger-new-tab)
  (ranger-map "gT"          'ranger-prev-tab)
  (ranger-map "gt"          'ranger-next-tab)
  (ranger-map "gc"          'ranger-close-tab)

  (ranger-map (kbd "C-r")   'ranger-refresh)
  (ranger-map (kbd "C-SPC") 'ranger-mark)
  (ranger-map (kbd "TAB")   'ranger-mark)
  (ranger-map (kbd "C-j")   'ranger-scroll-page-down)
  (ranger-map (kbd "C-k")   'ranger-scroll-page-up)
  (ranger-map (kbd "RET")   'ranger-find-file)
  (ranger-map (kbd "`")     'ranger-goto-mark)


  ;; and simulating search in standard emacs
  (define-key ranger-mode-map "/" 'isearch-forward)
  (define-key ranger-mode-map "n" 'isearch-repeat-forward)
  (define-key ranger-mode-map "N" 'isearch-repeat-backward)

  ;; make sure isearch is cleared before we delete the buffer on exit
  (add-hook 'ranger-mode-hook '(lambda () (setq isearch--current-buffer nil)))

  )


;; copy / paste
(defun ranger-show-copy-ring (copy-index)
  "Show copy ring in `ranger-copy-ring', selection inserts at top for use."
  (interactive (list
                (completing-read "Select from copy ring: "
                                 (ranger--ring-elements
                                  ranger-copy-ring)))))

(defun ranger-update-copy-ring (move append)
  "Add marked files to `ranger-copy-ring'. When `MOVE' is non-nil, targets will
be moved. `APPEND' will add files to current ring."
  ;; ideas borrowed from Fuco1's `dired-ranger' package.
  (let ((marked-files (dired-get-marked-files)))
    (if (or (ring-empty-p ranger-copy-ring)
            (not append))
        (ring-insert ranger-copy-ring (cons move marked-files))
      (let* ((current (ring-remove ranger-copy-ring 0))
             (cur-files (cdr current)))
        (ring-insert ranger-copy-ring
                     (cons move
                           (cl-remove-duplicates
                            (append cur-files marked-files)
                            :test (lambda (x y) (or (null y) (equal x y)))
                            )))))
    (ranger-show-flags)
    (message (format "%s %d item(s) to %s ring [total:%d]"
                     (if append "Added" "Copied")
                     (length marked-files)
                     (if move "cut" "copy")
                     (length (cdr (ring-ref ranger-copy-ring 0)))))))

(defun ranger-mark (arg &optional interactive)
  "Mark the file at point in the Dired buffer.
If the region is active, mark all files in the region.
Otherwise, with a prefix arg, mark files on the next ARG lines."
  (interactive (list current-prefix-arg t))
  (cond
   ;; Mark files in the active region.
   ((and interactive (use-region-p))
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (dired-mark-files-in-region
         (progn (goto-char beg) (line-beginning-position))
         (progn (goto-char end) (line-beginning-position))))))
   ;; Mark the current (or next ARG) files.
   (t
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       (function (lambda () (delete-char 1) (insert dired-marker-char))))))))

(defun ranger-copy (&optional append)
  "Place selected files in the copy ring and mark to be copied.
`universal-argument' can be used to append to current copy ring."
  (interactive "P")
  (ranger-update-copy-ring nil append))

(defun ranger-cut (&optional append)
  "Place selected files in the copy ring and mark to be moved.
`universal-argument' can be used to append to current copy ring."
  (interactive "P")
  (ranger-update-copy-ring t append))

(defun ranger-paste (&optional overwrite link)
  "Paste copied files from topmost copy ring."
  (interactive)
  (let* ((current (ring-ref ranger-copy-ring 0))
         (move (car current))
         (fileset (cdr current))
         (target (dired-current-directory))
         (filenum 0))
    (cl-loop for file in fileset do
             (when (file-exists-p file)
               (if move
                   (rename-file file target overwrite)
                 (if (file-directory-p file)
                     (copy-directory file target)
                   (copy-file file target overwrite)))
               (setq filenum (+ filenum 1))))
    ;; show immediate changes in buffer
    (revert-buffer)
    (message (format "%s %d/%d item(s) from the copy ring."
                     (if move "Moved" "Copied")
                     filenum
                     (length fileset)
                     ))))

(defun ranger-paste-over ()
  "Paste and overwrite copied files when same file names exist."
  (interactive)
  (ranger-paste t))

(defun ranger-show-copy-contents ()
  "Show the current contents to be copied / moved"
  (interactive)
  (let* ((current (ring-ref ranger-copy-ring 0))
         (move (if (car current) "Move" "Copy"))
         (fileset (cdr current)))
    (message (format "%s - total size: %s\n%s"
                     (propertize move 'face 'font-lock-builtin-face)
                     (ranger--get-file-sizes fileset)
                     (propertize (string-join fileset "\n") 'face 'font-lock-comment-face)
                     ))))


;;; frame parameter helpers

(defmacro r--fget (var &optional frame)
  "Return the value of `VAR', looks for buffer local version first."
  (let ((parameter (intern (format "%s" var))))
    `(or
      (when (local-variable-if-set-p (quote ,parameter))
        (buffer-local-value (quote ,parameter) (current-buffer)))
      (frame-parameter ,frame (quote ,parameter))
      ,var)))

(defmacro r--fset (var val &optional frame buffer-local)
  "Set the value of `VAR' in local buffer and on frame. When `BUFFER-LOCAL' is
non-nil, set buffer local variable as well."
  (let ((parameter (intern (format "%s" var))))
    `(progn
       (when ,buffer-local
         (set (make-local-variable (quote ,parameter)) ,val))
       (modify-frame-parameters ,frame (list (cons (quote  ,parameter) ,val)))
       ;; (message "%s" (frame-parameter nil ,parameter))
       )))

(defmacro r--fclear (parameter)
  `(r--fset ,parameter nil))

;; (message "%s:%s:%s:%s:%s"
;;          (r--fget ranger-minimal)
;;          (r--fget ranger-current-file)
;;          (frame-parameter nil 'ranger-minimal)
;;          (buffer-local-value ranger-minimal (current-buffer))
;;          ranger-minimal)

;;; alist helpers

(defun r--aget (alist key)
  "Return the value of KEY in ALIST. Uses `assoc'.
If PARAM is not found, return nil."
  (cdr-safe (assoc key alist)))

(defun r--akeys (alist)
  "Return the value of KEY in ALIST. Uses `assoc'.
If PARAM is not found, return nil."
  (mapcar 'car alist))

(defmacro r--aput (alist key value &optional no-overwrite)
  "Remove key from alist and set key with value. Set `NO-OVERWRITE' to non-nil
to not replace existing value."
  `(let ((sublist (assoc ,key ,alist)))
     (if sublist
         (unless ,no-overwrite
           (setcdr sublist ,value))
       (push (cons ,key ,value) ,alist))))

(defmacro r--aremove (alist key)
  "Remove KEY's key-value-pair from ALIST."
  `(setq ,alist (delq (assoc ,key ,alist) ,alist)))


;;tabs

(defun ranger--available-tabs ()
  "Returns list of unused tabs."
  (let* ((tabs
          (r--akeys ranger-tabs-alist))
         (total-tabs
          (number-sequence 1 ranger-max-tabs))
         (available-tabs
          (cl-remove-if '(lambda (tab) (member tab tabs)) total-tabs)))
    available-tabs))

(defun ranger-new-tab (&optional index no-refresh)
  "Create new tab using optional `INDEX' with the current directory."
  (interactive)
  (let* ((available-tabs (ranger--available-tabs))
         (index (or index
                    (and available-tabs
                         (apply 'min available-tabs))
                    ranger-current-tab)))
    ;; (message (format "%s" index))
    (when (and index (<= index ranger-max-tabs))
      (setq ranger-current-tab index)
      (ranger-update-tab index)
      (unless no-refresh
        (ranger-setup-preview)))))

(defun ranger-close-tab (&optional index)
  (interactive)
  (let ((index (or index
                   ranger-current-tab)))
    (when index
      (r--aremove ranger-tabs-alist index)
      (ranger-prev-tab))))

(defun ranger-other-tab (dir &optional index)
  (interactive)
  (let* ((tabs
          (r--akeys ranger-tabs-alist))
         (tab ranger-current-tab)
         (target index))
    (while (and
            (> tab 0)
            (<= tab ranger-max-tabs)
            (not target))
      (setq tab (+ tab dir))
      (when (member tab tabs)
        (setq target tab)))
    (unless target
      (if (eq dir 1)
          (setq target (apply 'min tabs))
        (setq target (apply 'max tabs))))
    (ranger-goto-tab target)))

(defun ranger-next-tab (&optional index)
  (interactive "P")
  (ranger-other-tab 1 index))

(defun ranger-prev-tab (&optional index)
  (interactive "P")
  (ranger-other-tab -1 index))

(defun ranger-update-tab (index)
  (let ((entry dired-directory)
        (relative (substring (ranger--dir-relative) 0 -1)))
    (when entry
      (r--aput ranger-tabs-alist
               index
               (cons relative entry)))))

(defun ranger-goto-tab (index)
  (interactive)
  (let ((tab (r--aget ranger-tabs-alist index)))
    (when tab
      (setq ranger-current-tab index)
      (ranger-find-file (cdr tab)))))


;; marks
(defun ranger-show-bookmarks ()
  "Show bookmark prompt for all bookmarked directories."
  (interactive)
  (let ((bookmark
         (completing-read "Select from bookmarks: "
                          (delq nil (mapcar
                                     #'(lambda (bm)
                                         (when (file-directory-p (cdr (cadr bm)))
                                           (cdr  (cadr bm))))
                                     bookmark-alist)))))
    (when bookmark (ranger-find-file bookmark))))

(defun ranger-create-mark (mark)
  "Create new bookmark using internal bookmarks, designating bookmark name as
ranger-`CHAR'."
  (interactive "cm-")
  (let ((mark-letter (char-to-string mark)))
    (bookmark-set (concat "ranger-" mark-letter))
    (message "Bookmarked directory %s as `ranger-%s'"
             default-directory mark-letter)))

(defun ranger-goto-mark ()
  "Go to bookmarks specified from `ranger-create-mark'."
  (interactive)
  (let* ((mark
          (read-key
           (mapconcat
            #'(lambda (bm)
                (when (and
                       (string-match "ranger-" (car  bm))
                       (file-directory-p (cdr (cadr bm))))
                  (replace-regexp-in-string "ranger-" "" (car bm))))
            bookmark-alist " ")))
         (mark-letter (char-to-string mark))
         (bookmark-name (concat "ranger-" mark-letter))
         (bookmark-path (bookmark-location bookmark-name)))
    (when (file-directory-p bookmark-path)
      (ranger-find-file bookmark-path))))


;; ring utilities
(defun ranger--ring-elements (ring)
  "Return deduplicated elements of `ring'"
  (delq nil
        (cl-remove-duplicates
         (ring-elements ring)
         :test (lambda (x y) (or (null y) (equal x y)))
         )))

(defun ranger--ring-index-elements (ring)
  "Return elements of `ring', along with its index in a (cons)."
  (let (listing)
    (dotimes (idx (ring-length ring) listing)
      (setq listing (append (cons idx (ring-ref ring idx)) listing)))))


;; history
(defun ranger-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list
    (completing-read "Select from history: "
                     (ranger--ring-elements ranger-history-ring))))
  (when history (ranger-find-file history)))

(defun ranger-jump-history (jump)
  "Move through history ring by increment `jump'"
  (let* ((ring ranger-history-ring)
         (curr-index ranger-history-index)
         (goto-idx (min
                    (max 0 (+ curr-index jump))
                    (- (ring-length ring) 1)))
         (jump-history (ring-ref ring goto-idx)))
    (message (format "ranger-history : %i/%i" (+ 1 goto-idx) (ring-length ranger-history-ring)))
    (when jump-history
      (setq ranger-history-index goto-idx)
      (ranger-find-file jump-history t))))

(defun ranger-next-history ()
  "Move forward in history"
  (interactive)
  (ranger-jump-history -1))

(defun ranger-prev-history ()
  "Move backward in history"
  (interactive)
  (ranger-jump-history 1))


;; primary window functions
(defun ranger-refresh ()
  "Refresh evil ranger buffer."
  (interactive)
  ;; make sure cursor is visible on screen
  (scroll-right)
  ;; reset dired trees
  (dired-kill-tree dired-directory)
  ;; refresh for any changes
  (revert-buffer)
  ;; setup buffer
  (ranger-setup)
  (ranger-show-details))

(defun ranger-help ()
  "Show help message for ranger basics."
  (interactive)
  (message "[h/l]-back/forward [j/k]-up/down [f]ind-file [i]nspect-file [^R]eload [S]hell [H/L]-history back/forward"))

(defun ranger-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (if ranger-show-dotfiles ; if currently showing
      (progn
        (setq ranger-show-dotfiles nil)
        (ranger-hide-dotfiles))
    (progn
      (setq ranger-show-dotfiles t)
      (revert-buffer) ; otherwise just revert to re-show
      ))
  (ranger-setup)
  (message (format "Show Dotfiles: %s"  ranger-show-dotfiles)))

(defun ranger-hide-dotfiles ()
  "Hide dotfiles in directory. TODO add variable for files to hide."
  (unless ranger-show-dotfiles
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
          (not (eolp))			; empty line
          (let ((fn (dired-get-filename t t)))
            (and fn (string-match-p  "^\\\." fn))))
     nil)
    (dired-do-kill-lines nil "")))

(defun ranger-sort-criteria (criteria)
  "Call sort-dired by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "criteria: (n/N)ame (e/E)xt (s/S)ize m(t/T)ime (c/C)time " '(?q ?n ?N ?e ?E ?s ?S ?t ?T ?c ?C))))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (uppercasep (and (stringp c) (string-equal c (upcase c)) ))
           (cc (downcase c))
           (ranger-sort-flag
            (cond
             ((string-equal cc "n") "")
             ((string-equal cc "c") "c")
             ((string-equal cc "e") "X")
             ((string-equal cc "t") "t")
             ((string-equal cc "s") "S")))
           )
      (setq ranger-sorting-switches
            (concat ranger-sort-flag
                    (when uppercasep "r")))
      (ranger-sort t)
      (ranger-refresh))))

(defun ranger-omit ()
  "Quietly omit files in dired."
  (setq-local dired-omit-verbose nil)
  ;; (setq-local dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
  (dired-omit-mode t))

(defun ranger-sort (&optional force)
  "Perform current sort on directory. Specify `FORCE' to sort even when
`ranger-persistent-sort' is nil."
  (when (or force
            ranger-persistent-sort)
    (dired-sort-other
     (concat dired-listing-switches
             ranger-sorting-switches))))


;; preview windows functions
(defun ranger-preview-toggle ()
  "Toggle preview of selected file."
  (interactive)
  (if ranger-preview-file
      (progn
        (when (and ranger-preview-window
                   (window-live-p ranger-preview-window)
                   (window-at-side-p ranger-preview-window 'right))
          (ignore-errors
            (delete-window ranger-preview-window)))
        (dired-hide-details-mode -1)
        (funcall 'add-to-invisibility-spec 'dired-hide-details-information)
        (setq ranger-preview-file nil))
    (progn
      (setq ranger-preview-file t)
      (dired-hide-details-mode t)
      (setq dired-hide-details-hide-symlink-targets nil))
    (ranger-setup-preview))
  (ranger-show-details))

(defun ranger-toggle-scale-images ()
  "Show/hide dot-files."
  (interactive)
  (if ranger-image-fit-window ; if currently showing
      (setq ranger-image-fit-window nil)
    (setq ranger-image-fit-window t))
  (when ranger-preview-file
    (ranger-setup-preview))
  (message (format "Fit Images to Window: %s"  ranger-image-fit-window)))

(defun ranger-toggle-literal ()
  "Toggle showing literal / actual preview of file."
  (interactive)
  (if ranger-show-literal
      (setq ranger-show-literal nil)
    (setq ranger-show-literal t))
  (when ranger-preview-file
    (ranger-setup-preview))
  (message (format "Literal Preview: %s"  ranger-show-literal)))

(defun ranger-scroll-page-down ()
  "Scroll preview window up."
  (interactive)
  (scroll-other-window))

(defun ranger-scroll-page-up ()
  "Scroll preview window down."
  (interactive)
  (scroll-other-window '-))


;; dired navigation
(defun ranger-search-files ()
  "Search for files / directories in folder."
  (interactive)
  (if (featurep 'helm)
      (call-interactively 'helm-find-files)
    (call-interactively 'ido-find-file)))

(defun ranger-up-directory ()
  "Move to parent directory."
  (interactive)
  (let ((current default-directory)
        (parent (ranger-parent-directory default-directory)))
    (when parent
      (ranger-find-file parent)
      (dired-goto-file current))))

(defun ranger-find-file (&optional entry ignore-history)
  "Find file in ranger buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in ranger. `IGNORE-HISTORY' will not update history-ring on change"
  (interactive)
  (let ((find-name (or entry
                       (dired-get-filename nil t)))
        (frame (window-frame))
        (minimal (r--fget ranger-minimal)))
    (when find-name
      (if (file-directory-p find-name)
          (progn
            (unless minimal
              (r--aput ranger-frames-alist
                       frame
                       (current-window-configuration)
                       t))
            (r--aput ranger-windows-alist
                     (selected-window)
                     (current-buffer)
                     t)
            (unless ignore-history
              (ranger-update-history find-name))
            (find-file find-name)
            (if minimal
                (r--fset ranger-minimal t)
              (r--fset ranger-minimal nil))
            (ranger-enable))
        (progn
          (ranger-disable)
          (find-file find-name))))))

(defun ranger-open-file (&optional horizontal)
  "Find file in ranger buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in ranger. `IGNORE-HISTORY' will not update history-ring on change"
  (let ((marked-files (dired-get-marked-files)))
    (cl-loop for find-name in marked-files do
             (when (and find-name
                        (not (file-directory-p find-name)))
               (cl-case horizontal
                 ('frame
                  (let ((goto-frame (make-frame)))
                    (select-frame-set-input-focus goto-frame)))
                 (t
                  (ranger-disable)
                  (split-window-right)
                  (windmove-right))
                 (nil
                  (ranger-disable)
                  (split-window-below)
                  (windmove-down)))
               (find-file find-name)))))

;; idea taken from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun ranger-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (cond ((string-equal system-type "windows-nt")
           (mapc
            (lambda (f) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" f t t)) )
            marked-files) )
          ((string-equal system-type "darwin")
           (mapc
            (lambda (f) (shell-command (format "open \"%s\"" f)))
            marked-files))
          ((string-equal system-type "gnu/linux")
           (mapc
            (lambda (f) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" f)))
            marked-files))
          (t (message "System type not supported.")))))

(defun ranger-open-file-horizontally ()
  "Open current file as a split with previously opened window"
  (interactive)
  (ranger-open-file t))

(defun ranger-open-file-vertically ()
  "Open current file as a split with previously opened window"
  (interactive)
  (ranger-open-file nil))

(defun ranger-open-file-frame ()
  "Open current file as a split with previously opened window"
  (interactive)
  (ranger-open-file 'frame))

(defun ranger-insert-subdir ()
  "Insert subdir from selected folder."
  (interactive)
  (let ((find-name (dired-get-filename nil t)))
    (if (file-directory-p find-name)
        (progn
          (setq header-line-format nil)
          (setq ranger-subdir-p t)
          (dired-insert-subdir find-name)
          (revert-buffer)
          (ranger-setup))
      (message "Can only insert on a directory."))))

(defun ranger-prev-subdir ()
  "Go to previous subdir in ranger buffer."
  (interactive)
  (dired-prev-subdir 1 t)
  (beginning-of-line))

(defun ranger-next-subdir ()
  "Go to next subdir in ranger buffer."
  (interactive)
  (dired-next-subdir 1 t)
  (beginning-of-line))

(defun ranger-update-history (name)
  "Update history ring and current index"
  (when (or (ring-empty-p ranger-history-ring)
            (not (eq name (ring-ref ranger-history-ring 0))))
    (progn
      (ring-insert ranger-history-ring (directory-file-name name))
      (setq ranger-history-index 0))))

(defun ranger-goto-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min))
  (ranger-prev-file))

(defun ranger-goto-bottom ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-max))
  (ranger-next-file))

(defun ranger-go-home ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-max))
  (ranger-find-file "~/"))

(defun ranger-next-file ()
  "Move to next file in ranger."
  (interactive)
  (dired-next-line 1)
  (when (eobp)
    (dired-next-line -1))
  (ranger-show-details)
  (when ranger-preview-file
    (ranger-setup-preview)))

(defun ranger-prev-file ()
  "Move to previous file in ranger."
  (interactive)
  (dired-previous-line 1)
  (unless ranger-modify-header
    (when (bobp)
      (dired-next-line 1)))
  (ranger-show-details)
  (when ranger-preview-file
    (ranger-setup-preview)))

(defun ranger--footer-spec ())

(defun ranger-show-size ()
  "Show directory size."
  (interactive)
  (ranger-show-details t))

(defun ranger-show-details (&optional sizes)
  "Echo file details"
  (when (dired-get-filename nil t)
    (let* ((entry (dired-get-filename nil t))
           (fattr (file-attributes entry))
           (fwidth (frame-width))
           (file-size (file-size-human-readable (nth 7 fattr)))
           (dir-size (if sizes (ranger--get-file-sizes
                                (list dired-directory))
                       ""))
           (filedir-size (if sizes (ranger--get-file-sizes
                                    (ranger--get-file-listing dired-directory))
                           ""))
           (file-date (format-time-string "%Y-%m-%d %H:%m"
                                          (nth 5 fattr)))
           (file-perm (nth 8 fattr))
           (cur-pos (line-number-at-pos (point)))
           (final-pos (- (line-number-at-pos (point-max)) 1))
           (position (format "%3d/%-3d"
                             cur-pos
                             final-pos))
           (footer-spec (ranger--footer-spec))
           (space (- fwidth
                     7
                     (max 08 (length file-size))
                     (max 08 (length filedir-size))
                     (max 08 (length dir-size))
                     (length position)
                     (length file-date)
                     (length file-perm)))
           (message-log-max nil))

      (r--fset ranger-current-file entry nil t)
      (message "%s" (format
                     (format  "%%s     f:%%08s d:%%08s t:%%08s%%%ds %%s" space)
                     (propertize file-date 'face 'font-lock-warning-face)
                     file-size
                     filedir-size
                     dir-size
                     position
                     file-perm
                     )))))



;; parent window functions
(defun ranger-sub-window-setup ()
  "Parent window options."
  ;; allow mouse click to jump to that directory
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)
  (local-set-key (kbd  "<mouse-1>") 'ranger-find-file)
  ;; set header-line
  (when ranger-modify-header
    (setq header-line-format `(:eval (,ranger-parent-header-func)))))

(defun ranger-parent-child-select ()
  (when ranger-child-name
    (dired-goto-file ranger-child-name)
    (hl-line-mode t)))

(defun ranger-less-parents ()
  "Reduce number of ranger parents."
  (interactive)
  (setq ranger-parent-depth (max 0 (- ranger-parent-depth 1)))
  (ranger-setup))

(defun ranger-more-parents ()
  "Increase number of ranger parents."
  (interactive)
  (setq ranger-parent-depth (+ ranger-parent-depth 1))
  (ranger-setup))

(defun ranger-setup-parents ()
  "Setup all parent directories."
  (let ((parent-name (ranger-parent-directory default-directory))
        (current-name default-directory)
        (i 0)
        (unused-windows ()))

    (setq ranger-buffer (current-buffer))

    (setq ranger-window (get-buffer-window (current-buffer)))

    (setq ranger-visited-buffers (append ranger-parent-buffers ranger-visited-buffers))

    (setq ranger-parent-buffers ())
    (setq ranger-parent-windows ())
    (setq ranger-parent-dirs ())

    (while (and parent-name
                (not (r--fget ranger-minimal))
                (file-directory-p parent-name)
                (< i ranger-parent-depth))
      (setq i (+ i 1))
      (unless (string-equal current-name parent-name)
        ;; (walk-window-tree
        ;;  (lambda (window)
        ;;    (when (eq (window-parameter window 'window-slot) (- 0 i))
        ;;      (setq unused-window window)
        ;;      ))
        ;;  nil nil 'nomini)
        (progn
          (add-to-list 'ranger-parent-dirs (cons (cons current-name parent-name) i))
          (setq current-name (ranger-parent-directory current-name))
          (setq parent-name (ranger-parent-directory parent-name)))))
    (mapc 'ranger-make-parent ranger-parent-dirs)

    ;; select child folder in each parent
    (save-excursion
      (walk-window-tree
       (lambda (window)
         (progn
           (when (member window ranger-parent-windows)
             ;; select-window needed for hl-line
             (select-window window)
             (ranger-parent-child-select))))
       nil nil 'nomini))

    (select-window ranger-window)
    ))

(defun ranger-make-parent (parent)
  "Make parent window.  `PARENT' is a construct with ((current . parent) .
slot)."
  (let* ((parent-name (cdar parent))
         (current-name (caar parent))
         (slot (cdr parent))
         (parent-buffer (ranger-dir-buffer parent-name))
         (parent-window
          (display-buffer
           parent-buffer
           `(ranger-display-buffer-at-side . ((side . left)
                                              (slot . ,(- 0 slot))
                                              (inhibit-same-window . t)
                                              (window-width . ,(min
                                                                (/ ranger-max-parent-width
                                                                   (length ranger-parent-dirs))
                                                                ranger-width-parents)))))))
    (with-current-buffer parent-buffer
      (setq ranger-child-name (directory-file-name current-name)))

    (add-to-list 'ranger-parent-buffers parent-buffer)
    (add-to-list 'ranger-parent-windows parent-window)))

(defun ranger-next-parent ()
  "Move up in parent directory"
  (interactive)
  (with-current-buffer (car ranger-parent-buffers)
    (dired-next-line 1)
    (let ((curfile (dired-get-filename nil t)))
      (if (file-directory-p curfile)
          (ranger-find-file curfile)
        (dired-next-line -1)))))

(defun ranger-prev-parent ()
  "Move up in parent directory"
  (interactive)
  (with-current-buffer (car ranger-parent-buffers)
    (dired-next-line -1)
    (let ((curfile (dired-get-filename nil t)))
      (if (file-directory-p curfile)
          (ranger-find-file curfile)
        (dired-next-line 1)))))


;; window creation subroutines
(defun ranger-dir-buffer (entry)
  "Open `ENTRY' in dired buffer."
  ;; (ignore-errors
  (with-current-buffer (or
                        (car (or (dired-buffers-for-dir entry) ()))
                        (dired-noselect entry))
    (run-hooks 'ranger-parent-dir-hook)
    (current-buffer)))

(defun ranger-dir-contents (entry)
  "Open `ENTRY' in dired buffer."
  (let ((temp-buffer (or (get-buffer "*ranger-prev*")
                         (generate-new-buffer "*ranger-prev*"))))
    (with-demoted-errors
        (with-current-buffer temp-buffer
          (make-local-variable 'font-lock-defaults)
          (setq font-lock-defaults '((dired-font-lock-keywords) nil t))
          (erase-buffer)
          (turn-on-font-lock)
          (insert-directory entry (concat dired-listing-switches ranger-sorting-switches) nil t)
          (goto-char (point-min))
          ;; truncate lines in directory buffer
          (setq truncate-lines t)
          ;; remove . and .. from directory listing
          (save-excursion
            (while (re-search-forward "total used in directory\\|\\.$" nil t)
              (kill-whole-line)))
          (current-buffer)))))

(defun ranger-preview-buffer (entry-name)
  "Create the preview buffer of `ENTRY-NAME'.  If `ranger-show-literal'
is set, show literally instead of actual buffer."
  (if ranger-show-literal
      ;; show literal version of file
      (let ((temp-buffer (or (get-buffer "*ranger-prev*")
                             (generate-new-buffer "*ranger-prev*"))))
        (with-current-buffer temp-buffer
          (erase-buffer)
          (font-lock-mode -1)
          (insert-file-contents entry-name)
          (current-buffer)))
    ;; show file
    ;; (if (image-type-from-file-header entry-name)
    (if (and (image-type-from-file-header entry-name)
             (not (eq (image-type-from-file-header entry-name) 'gif))
             ranger-image-fit-window)
        (ranger-setup-image-preview entry-name)
      (with-current-buffer
          (or
           (find-buffer-visiting entry-name)
           (find-file-noselect entry-name nil ranger-show-literal))
        (current-buffer)))))

(defun ranger-setup-image-preview (entry-name)
  "Setup and maybe resize image"
  (let* ((new-file (expand-file-name image-dired-temp-image-file))
         (file (expand-file-name entry-name))
         ret success
         (width (* ranger-width-preview ranger-image-scale-ratio (image-dired-display-window-width)))
         (height (image-dired-display-window-height))
         (image-type 'jpeg) command)
    (if (and (not (eq (image-type-from-file-header entry-name) 'gif))
             ranger-image-fit-window)
        (progn
          (setq command
                (format-spec
                 image-dired-cmd-create-temp-image-options
                 (list
                  (cons ?p image-dired-cmd-create-temp-image-program)
                  (cons ?w width)
                  (cons ?h height)
                  (cons ?f file)
                  (cons ?t new-file))))
          (setq ret (call-process shell-file-name nil nil nil
                                  shell-command-switch command))
          (when (= 0 ret)
            (setq success 1))))
    (with-current-buffer (image-dired-create-display-image-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (clear-image-cache)
        (image-dired-insert-image (if success
                                      image-dired-temp-image-file
                                    file)
                                  image-type 0 0)
        (goto-char (point-min))
        (image-dired-update-property 'original-file-name file))
      image-dired-display-image-buffer)))

(defun ranger-setup-preview ()
  "Setup ranger preview window."
  (let* ((entry-name (dired-get-filename nil t))
         (inhibit-modification-hooks t)
         (fsize
          (nth 7 (file-attributes entry-name))))
    (when ranger-cleanup-eagerly
      (ranger-preview-cleanup))
    ;; delete existing preview window
    (when (and ranger-preview-window
               (window-live-p ranger-preview-window))
      (ignore-errors (delete-window ranger-preview-window)))
    (when (and (not (r--fget ranger-minimal))
               entry-name
               ranger-preview-file)
      (unless (or
               (> fsize (* 1024 1024 ranger-max-preview-size))
               (member (file-name-extension entry-name)
                       ranger-excluded-extensions))
        (with-demoted-errors "%S"
          (let* ((preview-window (display-buffer
                                  (if (file-directory-p entry-name)
                                      (ranger-dir-contents entry-name)
                                    (ranger-preview-buffer entry-name))
                                  `(ranger-display-buffer-at-side . ((side . right)
                                                                     (slot . 1)
                                                                     ;; (inhibit-same-window . t)
                                                                     (window-width . ,(- ranger-width-preview
                                                                                         (min
                                                                                          (- ranger-max-parent-width
                                                                                             ranger-width-parents)
                                                                                          (* (- ranger-parent-depth 1)
                                                                                             ranger-width-parents))))))))
                 (preview-buffer
                  (window-buffer preview-window)))

            (with-current-buffer preview-buffer
              (when ranger-modify-header
                (setq header-line-format `(:eval (,ranger-preview-header-func)))))

            (add-to-list 'ranger-preview-buffers preview-buffer)
            (setq ranger-preview-window preview-window)
            (dired-hide-details-mode t)))))))


;; utilities
(defun ranger-parent-directory (entry)
  "Find the parent directory of `ENTRY'."
  (file-name-directory (directory-file-name entry)))

;; (defun ranger-fix-width (window)
;;   "Fix the width of `WINDOW'."
;;   (with-selected-window window
;;     (setq-local window-size-fixed 'width)))

(defun ranger--get-file-sizes (fileset)
  "Determine file size of provided list of files in `FILESET'."
  (if (and
       fileset
       (executable-find "du"))
      (with-temp-buffer
        (apply 'call-process "du" nil t nil "-sch" fileset)
        (format "%s"
                (progn
                  (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                  (match-string 1)))) ""))

(defun ranger--get-file-listing (dir)
  "Return listing of files in dired directory."
  (save-excursion
    (let ((fileset ())
          file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
        (save-excursion
          (and (not (looking-at-p dired-re-dir))
               (not (eolp))
               (setq file (dired-get-filename nil t)) ; nil on non-file
               (progn (end-of-line)
                      (push file fileset))))
        (forward-line 1))
      fileset)))

(defun ranger-display-buffer-at-side (buffer alist)
  "Try displaying `BUFFER' at one side of the selected frame. This splits the
window at the designated `side' of the frame.  Accepts `window-width' as a
fraction of the total frame size"
  (let* ((side (or (cdr (assq 'side alist)) 'bottom))
         (slot (or (cdr (assq 'slot alist)) 0))
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (window-size (ceiling  (* (frame-width) window-width)))
         (split-width-threshold 0)
         (current-window ranger-window)
         new-window
         reuse-window)

    ;; (walk-window-tree
    ;;  (lambda (window)
    ;;    (progn
    ;;      (when (not (eq current-window window))
    ;;        (when (eq (window-parameter window 'window-slot) slot)
    ;;          (setq reuse-window window))
    ;;        (when (eq (window-parameter window 'window-slot) (+ slot 1))
    ;;          (setq current-window window))
    ;;        )))
    ;;  nil nil 'nomini)

    ;; (if reuse-window
    ;;     (progn
    ;;       (shrink-window (-  window-size (window-width reuse-window)) t)
    ;;       ;; (set-window-parameter reuse-window 'window-slot slot)
    ;;       (window--display-buffer
    ;;        buffer reuse-window 'reuse alist display-buffer-mark-dedicated)
    ;;       )
    (progn
      (setq new-window (split-window current-window window-size side))
      (set-window-parameter new-window 'window-slot slot)
      (window--display-buffer
       buffer new-window 'window alist display-buffer-mark-dedicated))
    ;; )
    ))

(defun ranger-show-flags ()
  "Show copy / paste flags in ranger buffer."
  (when (not (ring-empty-p ranger-copy-ring))
    (ranger-clear-flags ?P)
    (ranger-clear-flags ?M)
    ;; (dired-unmark-all-files ?\r)
    (let* ((current (ring-ref ranger-copy-ring 0))
           (fileset (cdr current))
           (dired-marker-char (if (car current) ?M ?P)))
      (save-excursion
        (cl-loop for file in fileset do
                 (when
                     (dired-goto-file file)
                   (ranger-mark 1)))))))

(defun ranger-clear-flags (mark)
  "Remove a copy / paste flags from every file."
  (save-excursion
    (let* ((count 0)
           (inhibit-read-only t) case-fold-search
           (string (format "\n%c" mark)))
      (goto-char (point-min))
      (while
          (search-forward string nil t)
        (when (dired-get-filename t t)
          (subst-char-in-region (1- (point)) (point)
                                (preceding-char) ?\s))))))


;; cleanup and reversion
(defun ranger-preview-cleanup ()
  "Cleanup all old buffers and windows used by ranger."
  (mapc 'ranger-kill-buffer ranger-preview-buffers)
  (setq ranger-preview-buffers ()))

(defun ranger-kill-buffer (buffer)
  "Delete unmodified buffers and any dired buffer"
  (when
      (and (buffer-live-p buffer)
           (or (eq 'dired-mode (buffer-local-value 'major-mode buffer))
               (not (buffer-modified-p buffer))))
    (kill-buffer buffer)))



(defun ranger-revert (&optional buffer)
  "Revert ranger settings."
  ;; restore window configuration

  (let* ((minimal (r--fget ranger-minimal))
         (prev-buffer
          (r--aget ranger-windows-alist
                   (selected-window)))
         (win-config
          (r--aget ranger-frames-alist
                   (window-frame))))

    (when (or win-config
              prev-buffer)

      (r--aremove ranger-windows-alist (selected-window))

      (when (and minimal
                 prev-buffer
                 (buffer-live-p prev-buffer))
        (switch-to-buffer prev-buffer))

      (when (and (not minimal)
                 win-config
                 (window-configuration-p win-config))
        (set-window-configuration win-config)
        (r--aremove ranger-frames-alist (window-frame)))

      ;; (r--aremove ranger-tabs-alist ranger-current-tab)

      (when (not (or (ranger-windows-exists-p)
                     (ranger-frame-exists-p)))

        (message "Reverting all buffers")
        ;; remove all hooks and advices
        (advice-remove 'dired-readin #'ranger-setup-dired-buffer)
        (remove-hook 'window-configuration-change-hook 'ranger-window-check)

        (r--fset ranger-minimal nil)

        ;; revert appearance
        (ranger-revert-appearance (or buffer (current-buffer)))
        (ranger-revert-appearance ranger-buffer)

        ;; delete and cleanup buffers
        (let ((all-ranger-buffers
               (cl-remove-duplicates
                (append
                 ranger-preview-buffers
                 ranger-parent-buffers
                 ranger-visited-buffers
                 (list ranger-buffer))
                :test (lambda (x y) (or (null y) (eq x y)))
                )))
          ;; (message (format "all buffers : %s" all-ranger-buffers))

          (if ranger-cleanup-on-disable
              (mapc 'ranger-kill-buffer all-ranger-buffers)
            (mapc 'ranger-revert-appearance all-ranger-buffers)))

        ;; kill preview buffer
        (when (get-buffer "*ranger-prev*")
          (kill-buffer (get-buffer "*ranger-prev*")))

        (setq ranger-frames-alist ())
        (setq ranger-windows-alist ())

        ;; clear variables
        (setq ranger-preview-buffers ()
              ranger-visited-buffers ()
              ranger-parent-buffers ()))

      ;; clear ranger-show-details information
      (message "%s" ""))))

(defun ranger-revert-appearance (buffer)
  "Revert the `BUFFER' to pre-ranger defaults"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; revert buffer local modes used in ranger
      ;; (message (format "reverting : %s" buffer))

      (unless ranger-pre-hl-mode
        (hl-line-mode -1))
      (unless ranger-pre-arev-mode
        (auto-revert-mode -1))
      (setq header-line-format nil)
      (when (derived-mode-p 'dired-mode)
        (unless ranger-pre-omit-mode
          (dired-omit-mode -1))
        (setq dired-listing-switches ranger-pre-dired-listing)
        (dired-hide-details-mode -1)
        ;; revert ranger-mode
        (setq ranger-mode nil)
        ;; hide details line at top
        (funcall 'remove-from-invisibility-spec 'dired-hide-details-information)
        (revert-buffer)))))

(defun ranger-still-dired ()
  "Enable or disable ranger based on current mode"
  (if (derived-mode-p 'dired-mode)
      (ranger-enable)
    (progn
      ;; Try to manage new windows / frames created without killing ranger
      (let ((current (current-buffer))
            (buffer-fn (buffer-file-name (current-buffer))))
        (if buffer-fn
            (progn
              (message "File opened, exiting ranger")
              (ranger-disable)
              (find-file buffer-fn))
          (progn
            (message "Redirecting window to new frame")
            (set-window-buffer nil ranger-buffer)
            (when current
              (display-buffer-other-frame current))))))))

(defun ranger-window-check ()
  "Detect when ranger-window is no longer part of ranger-mode"
  (let* ((windows (window-list))
         (ranger-windows (r--akeys ranger-windows-alist))
         (ranger-frames (r--akeys ranger-frames-alist)))
    ;; if all frames and windows are killed, revert buffer settings
    (if (not  (or (ranger-windows-exists-p)
                  (ranger-frame-exists-p)))
        (progn
          (message "All ranger frames have been killed, reverting ranger settings and cleaning buffers.")
          (ranger-revert))
      ;; when still in ranger's window, make sure ranger's primary window and buffer are still here.
      (when (memq (selected-window) ranger-windows)
        (unless (and (memq ranger-window windows)
                     (eq (window-buffer ranger-window) ranger-buffer))
          (remove-hook 'window-configuration-change-hook 'ranger-window-check)
          (ranger-still-dired))))))

(defun ranger-windows-exists-p ()
  "Test if any ranger-windows are live."
  (if (delq nil
            (mapcar 'window-live-p
                    (r--akeys ranger-windows-alist)))
      t
    nil))

(defun ranger-frame-exists-p ()
  "Test if any ranger-frames are live."
  (if (delq nil
            (mapcar 'frame-live-p
                    (r--akeys ranger-frames-alist)))
      t
    nil))

(defun ranger-kill-buffers-without-window ()
  "Will kill all ranger buffers that are not displayed in any window."
  (interactive)
  (cl-loop for buffer in ranger-parent-buffers do
           (unless (get-buffer-window buffer t)
             (kill-buffer-if-not-modified buffer)))
  (cl-loop for buffer in ranger-preview-buffers do
           (unless (get-buffer-window buffer t)
             (kill-buffer-if-not-modified buffer))))


;; header / mode line
(defun ranger--dir-relative ()
  "Return the topmost directory name in path"
  (let* ((current-name default-directory)
         (parent-name (ranger-parent-directory default-directory))
         (relative
          (if (string-equal current-name parent-name)
              current-name
            (file-relative-name current-name parent-name))))
    relative))

(defun ranger--header-tabs ()
  (let* ((curr ranger-current-tab)
         (tabs (sort (r--akeys ranger-tabs-alist) '<)))
    (mapconcat
     (lambda (key)
       (let* ((item (r--aget ranger-tabs-alist key))
              (roman (ranger--ar2ro key))
              (value (car-safe item))
              ret)
         (setq ret (cl-case ranger-tabs-style
                     ('normal (format "%d:%s" key value))
                     ('roman (format "%s" roman))
                     ('number (format "%s" key))))
         (if (equal key curr)
             (propertize ret 'face 'default)
           ret)))
     tabs " ")))

(defun ranger--ar2ro (AN)
  "translate from arabic number AN to roman number,
   ranger--ar2ro returns string of roman numerals."
  (cond
   ((>= AN 10) (concat "X" (ranger--ar2ro (- AN 10))))
   ((>= AN 9) (concat "I" (concat "X" (ranger--ar2ro (- AN 9)))))
   ((>= AN 5) (concat "V" (ranger--ar2ro (- AN 5))))
   ((>= AN 4) (concat "I" (concat "V" (ranger--ar2ro (- AN 4)))))
   ((>= AN 1) (concat "I" (ranger--ar2ro (- AN 1))))
   ((= AN 0) nil)))

(defun ranger--header-rhs ()
  (concat
   (propertize
    (format "%s / %s "
            (if ranger-show-dotfiles ".." "")
            ;; (if ranger-show-literal "raw" "act")
            ranger-parent-depth)
    'face 'font-lock-comment-face)
   (when (> (length ranger-tabs-alist) 1)
     (format "| %s"
             (ranger--header-tabs)))))

(defun ranger--header-lhs ()
  "Setup header-line for ranger buffer."
  (let* ((user (user-login-name))
         (current-file (or (r--fget ranger-current-file) ""))
         (file-path (file-name-directory current-file))
         (file-name (file-name-nondirectory current-file)))
    (format "%s%s"
            file-path
            (propertize file-name 'face 'font-lock-constant-face))))

(defun ranger--header-string ()
  "Compose header string"
  (let* ((lhs (ranger--header-lhs))
         (rhs (ranger--header-rhs))
         (minimal (r--fget ranger-minimal))
         (used-length (+ (length rhs) (length lhs)))
         (total-window-width (+ (window-width ranger-window)
                                (if (and
                                     (not minimal)
                                     ranger-preview-file)
                                    (+ (window-width ranger-preview-window) 2)
                                  0)))
         (filler (make-string (max 0 (- total-window-width used-length)) (string-to-char " "))))
    (concat
     lhs
     filler
     rhs)))

(defun ranger-parent-header-line ()
  "Setup header-line for ranger parent buffer."
  (let* ((relative (ranger--dir-relative))
         (header (format " %s" relative)))
    (propertize header 'face 'dired-header)))

(defun ranger-preview-header-line ()
  "Setup header-line for ranger parent buffer."
  (substring (ranger--header-string) (+ (window-width ranger-window) 2) ))

(defun ranger-header-line ()
  "Setup header-line for ranger parent buffer."
  (substring (ranger--header-string) 0
             (+ (window-width ranger-window)
                (if ranger-preview-file 2 0))))

(defun ranger-set-modeline ()
  "This is a redefinition of the fn from `dired.el'. This one
properly provides the modeline in dired mode. "
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                   "Ranger:mtime")
                  ((string-match "^-[^c]*c[^c]*$" dired-actual-switches)
                   "Ranger:ctime")
                  ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                   "Ranger:ext")
                  ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                   "Ranger:size")
                  ((string-match "^-[^SXUt]*$" dired-actual-switches)
                   "Ranger:name")
                  (t
                   (concat "Ranger " dired-actual-switches)))))
    (with-eval-after-load "diminish"
      (diminish 'ranger-mode)
      (with-eval-after-load "dired-omit-mode"
        (diminish 'dired-omit-mode " O"))
      (with-eval-after-load 'auto-revert-mode
        (diminish 'auto-revert-mode " R")))
    (force-mode-line-update)))

(defun ranger-setup-dired-buffer ()
  "Setup the dired buffer by removing the header and sorting folders directory first."
  (when (eq (window-frame) ranger-frame)
    (save-excursion
      (let ((switches (concat
                       dired-listing-switches
                       ranger-sorting-switches))
            (buffer-read-only))
        (if (and (not ranger-subdir-p)
                 ranger-modify-header)
            (kill-whole-line)
          (forward-line 1))
        ;; check sorting mode and sort with directories first
        (when (not (string-match "[XStU]+" switches))
          (if (string-match "r" switches)
              (sort-regexp-fields nil "^.*$" "[ ]*." (point) (point-max))
            (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
        (set-buffer-modified-p nil)))))

;;;###autoload
(defun deer ()
  "Launch dired in a minimal ranger window."
  (interactive)
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (when dir
      (r--fset ranger-minimal t)
      (ranger-find-file dir))))

(defun ranger-minimal-toggle ()
  (interactive)
  (let ((minimal (r--fget ranger-minimal)))
    (ranger-revert)
    ;; (message "%s" minimal)
    (if minimal
        (ranger)
      (deer))))

;;;###autoload
(defun ranger ()
  "Launch dired in ranger-mode."
  (interactive)
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (when dir
      (r--fset ranger-minimal nil)
      (ranger-find-file dir))))

(defun ranger-enable ()
  "Interactively enable ranger-mode."
  (interactive)
  (ranger-mode t))

(defun ranger-disable ()
  "Interactively disable ranger-mode."
  (interactive)
  (ranger-mode -1))

(defun ranger-setup ()
  "Setup all associated ranger windows."
  (interactive)

  (unless (derived-mode-p 'dired-mode)
    (error "Run it from dired buffer"))

  ;; load bookmarks
  (unless bookmark-alist
    (bookmark-maybe-load-default-file))

  (require 'dired-x)

  (run-hooks 'ranger-mode-load-hook)

  ;; store previous settings
  (unless ranger-pre-saved
    (setq ranger-pre-hl-mode hl-line-mode)
    (setq ranger-pre-arev-mode auto-revert-mode)
    (setq ranger-pre-omit-mode dired-omit-mode)
    (setq ranger-pre-dired-listing dired-listing-switches)
    (setq ranger-pre-saved t))

  ;; save window-config for frame unless already
  ;; specified from running `ranger'
  (unless (r--fget ranger-minimal)
    (r--aput ranger-frames-alist
             (window-frame)
             (current-window-configuration)
             t))
  (r--aput ranger-windows-alist
           (selected-window)
           (current-buffer)
           t)
  ;; reset subdir optiona
  (setq ranger-subdir-p nil)

  ;; ranger specific objects
  (setq ranger-buffer (current-buffer))
  (setq ranger-window (get-buffer-window (current-buffer)))
  (setq ranger-frame (window-frame ranger-window))

  (setq ranger-preview-window nil)

  ;; hide groups, show human readable file sizes
  ;; TODO add as defcustom
  (setq dired-listing-switches "-alGh")

  (unless (r--fget ranger-minimal)
    (dired-hide-details-mode -1)
    (delete-other-windows))

  ;; CHANGE - removed
  ;; (dired-sort-other dired-listing-switches)

  ;; consider removing
  (auto-revert-mode)

  ;; set hl-line-mode for ranger usage
  (hl-line-mode t)
  ;; truncate lines for primary window
  (setq truncate-lines t)

  ;; clear out everything if not in deer mode

  (add-to-list 'ranger-visited-buffers ranger-buffer)

  ;; hide details line at top - show symlink targets
  (funcall 'add-to-invisibility-spec 'dired-hide-details-information)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  (ranger-sort t)
  (ranger-show-flags)
  (ranger-omit)
  (ranger-hide-dotfiles)

  ;; open new tab if ranger is in multiple frames.
  (if (> (length ranger-frames-alist) 1)
      (ranger-new-tab nil t)
    (ranger-update-tab ranger-current-tab))

  (ranger-setup-parents)
  (ranger-setup-preview)

  ;; scroll back to left in case new windows affected primary buffer
  (set-window-hscroll ranger-window 0)

  (when ranger-modify-header
    (setq header-line-format `(:eval (,ranger-header-func))))

  (ranger-show-details)
  (ranger-set-modeline))

;;;###autoload
(define-minor-mode ranger-mode
  "A convienent way to look up file contents in other window while browsing directory in dired"
  :init-value nil
  :lighter " Ranger"
  :keymap 'ranger-mode-map
  :group 'ranger
  ;; :after-hook 'ranger-mode-hook

  ;; define keymaps
  (ranger-define-maps)

  (if ranger-mode
      (progn
        (advice-add 'dired-readin :after #'ranger-setup-dired-buffer)
        (ranger-setup)
        (add-hook 'window-configuration-change-hook 'ranger-window-check))
    (ranger-revert)))

(provide 'ranger)

;;; ranger.el ends here
