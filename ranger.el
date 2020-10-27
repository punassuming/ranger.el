;;; ranger.el --- Make dired more like ranger -*- lexical-binding: t -*-
;; Copyright (C) 2015-2017  Rich Alesi

;; Author : Rich Alesi <https://github.com/ralesi>
;; Version: 0.9.8.6
;; Keywords: files, convenience, dired
;; Homepage: https://github.com/ralesi/ranger
;; Package-Requires: ((emacs "24.4"))

;; Inspired by work from
;; peep-dired - Author: Adam Sokolnicki

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

;; This is a derived major mode that runs within dired emulating many of the features of
;; ranger <https://github.com/hut/ranger>. This minor mode shows a stack of the
;; parent directories and updates the parent buffers while nvaigating the file
;; system. The preview window takes some of the ideas from Peep-Dired
;; <https://github.com/asok/peep-dired> to display previews for selected files
;; in the primary dired buffer. This package tries its best to make a seamless
;; user experience from ranger created for python.

;;; FEATURES

;; Replaces dired buffer with features from Ranger
;; - show window stack of parent directories
;; - show preview window of selected directory or file
;; - fast navigation using vi-like keybindings
;; - move through navigation history
;; - copy and paste functionality utilizing a copy ring

;;; KNOWN ISSUES

;; - window specific settings needed
;;  - current tab
;;  - history
;;  - current-file
;;  - cloasing last ranger does not clear variables
;;  - multiple ranger windows

;;; HISTORY

;; version 0.9.1,   2015-07-19 changed package to ranger
;; version 0.9.2,   2015-07-26 improve exit from ranger, bookmark support
;; version 0.9.3,   2015-07-31 deer mode, history navigation
;; version 0.9.4,   2015-08-20 fixed most bugs when reverting from ranger
;; version 0.9.5,   2015-09-11 delete all accessed buffers, add details to echo
;; version 0.9.6,   2015-09-13 copy and paste functionality added
;; version 0.9.7,   2015-10-04 multiple ranger window support, override dired
;; version 0.9.8,   2015-10-04 ranger is now a major mode
;; version 0.9.8.1, 2016-08-23 ranger-override-dired-mode
;; version 0.9.8.4, 2016-10-02 more mappings to match ranger

;;; Code:

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
(require 'ring)

(require 'subr-x)
(require 'diminish nil t)

(defgroup ranger ()
  "Modify dired to act like ranger."
  :group 'ranger
  :prefix "ranger-")

;; directory options
(defcustom ranger-cleanup-on-disable t
  "Cleanup opened buffers when disabling the minor mode."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-return-to-ranger nil
  "Return to ranger after killing edit buffer."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-cleanup-eagerly nil
  "Cleanup opened buffers upon `ranger-next-file' & `ranger-prev-file'."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-show-hidden 'hidden
  "When t it will show hidden files in directory."
  :group 'ranger
  :type '(radio (const :tag "Show All Files" :value 'format)
                (const :tag "Hide Common Files" :value 'prefer)
                (const :tag "Hide All Dotfiles" :value 'hidden)))

(defcustom ranger-format-regexp
  '("^\\.?#\\|^\\.$\\|^\\.\\.$")
  "Regexp of filenames to hide."
  :group 'ranger
  :type 'list)

(defcustom ranger-hidden-regexp
  '("^\\.")
  "Regexp of filenames to hide."
  :group 'ranger
  :type 'list)

(defcustom ranger-prefer-regexp
  '(
    ;; vcs folders
    "^\\.\\(git\\|hg\\|svn\\)$"
    ;; compiled files
    "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
    ;; generated files, caches or local pkgs
    "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
    ;; org-mode folders
    "^\\.\\(sync\\|export\\|attach\\)$"
    ;; backup files
    "~$"
    "^#.*#$"
    )
  "Regexp of custom filetypes to omit in ranger."
  :group 'ranger
  :type 'list)

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

(defcustom ranger-max-preview-size 2
  "File size in MB to prevent preview of files."
  :group 'ranger
  :type 'integer)

(defcustom ranger-show-literal t
  "When non-nil it will show file literally."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-listing-switches "-alhF"
  "Default listing switchs for dired buffer."
  :group 'ranger
  :type 'string)

(defcustom ranger-listing-dir-first t
  "Non-nil modifies dired buffer to sort directories first."
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

(defcustom ranger-deer-show-details t
  "When t show details in minimal ranger mode."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'ranger
  :type 'float)

;; parent options
(defcustom ranger-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'ranger
  :type 'float)

(defcustom ranger-max-parent-width 0.42
  "The max width allocated to showing parent windows."
  :group 'ranger
  :type 'float)

;;;###autoload
(defcustom ranger-key [?\C-p]
  "Key in `dired-mode' used to toggle `ranger-mode'."
  :group 'ranger
  :type 'sexp)

(defcustom ranger-max-tabs 9
  "Maximum number of tabs to allow ranger to maintain."
  :group 'ranger
  :type 'integer)

(defcustom ranger-footer-delay 0.01
  "Time in seconds to delay running footer functions."
  :group 'ranger
  :type 'float)

(defcustom ranger-footer-format " %d %m%w%s %c %f %p"
  "Format for footer display. "
  :group 'ranger
  :type 'string)

(defcustom ranger-preview-delay 0.05
  "Time in seconds to delay running preview file functions."
  :group 'ranger
  :type 'float)

;; header functions
(defcustom ranger-header-func 'ranger-header-line
  "Function used to output header of primary ranger window.
Outputs a string that will show up on the header-line."
  :group 'ranger
  :type 'function)

(defcustom ranger-map-style 'ranger
  "Specify style to display tabs in ranger."
  :group 'ranger
  :type '(radio (const :tag "Ranger" :value ranger)
                (const :tag "Dired" :value dired)
                (const :tag "Emacs" :value emacs)))

(defcustom ranger-tabs-style 'normal
  "Specify style to display tabs in ranger."
  :group 'ranger
  :type '(radio (const :tag "Normal" :value normal)
                (const :tag "Roman numerals" :value roman)
                (const :tag "Numbers only" :value numbers)))

;;;###autoload
(defcustom ranger-override-dired nil
  "When non-nil, load `deer' whenever dired is loaded."
  :group 'ranger
  :type '(radio (const :tag "Deer" :value t)
                (const :tag "Ranger" :value ranger)
                (const :tag "Don't Override" :value nil)))

(defcustom ranger-dont-show-binary t
  "When non-nil, detect binary files and don't show them in the
preview window."
  :group 'ranger
  :type 'boolean)

(defcustom ranger-hide-cursor t
  "When non-nil, hide cursor in dired buffers."
  :group 'ranger
  :type 'boolean)


;; declare used variables
(defvar ranger-was-ranger)
(defvar ranger-mode)
(defvar image-dired-temp-image-file)
(defvar image-dired-cmd-create-temp-image-program)
(defvar image-dired-cmd-create-temp-image-options)
(defvar image-dired-display-image-buffer)

;; global variables

(defvar ranger-wdired nil)

(defvar ranger-sorting-switches nil)

(defvar ranger-window nil)
(defvar ranger-buffer nil)
(defvar ranger-frame nil)

(defvar ranger-w-alist ()
  "List of windows using ranger.")

(defvar ranger-f-alist ()
  "List of frames using ranger.")

(defvar ranger-t-alist ()
  "List of tabs to keep track of in ranger.")

(defvar ranger-undo-tab ()
  "Last tab thaat was closed.")

(defvar ranger-history-index 0)

(defvar ranger-history-ring (make-ring ranger-history-length))

(defvar ranger-copy-ring (make-ring ranger-copy-ring-length))

(defvar ranger-image-scale-ratio 1.3)
(defvar ranger-image-fit-window nil)

(defvar ranger-pre-alist)
(defvar ranger-pre-saved nil)
(defvar ranger-pre-hl-mode nil)
(defvar ranger-pre-arev-mode nil)
(defvar ranger-pre-dired-listing nil)

(defvar ranger-preview-window nil)
(defvar ranger-preview-buffers ()
  "List with buffers of previewed files.")

(defvar ranger-parent-windows nil)
(defvar ranger-parent-buffers ()
  "List with buffers of parent buffers.")
(defvar ranger-parent-dirs nil)

(defvar ranger-visited-buffers ()
  "List of buffers visited in ranger.")

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
(defvar ranger-preview-dir-hook '(;; ranger-to-dired
                                  ;; revert-buffer
                                  ranger-sort
                                  ranger-filter-files
                                  ranger-truncate
                                  ranger-show-details
                                  ))

;; Masking and details

(defun ranger-show-details ()
  "Show file details in ranger."
  (dired-hide-details-mode -1))

(defun ranger-hide-details ()
  "Hide file details in ranger."
  (dired-hide-details-mode t))

(defcustom ranger-dired-display-mask '(t t t t t t t t)
  "Contols hiding/transforming columns.  If set, its value must be a list of
symbols, one for each attributes column.  If the symbol is nil, then the
corresponding column will be hidden, and if it's not nil then the column will be
left untouched.  The symbol may also be the name of a function that takes one
string argument and evaluates to a different string -- in this case this
function will be used to transform the contents of the corresponding column and
its result will be displayed instead."
  :group 'ranger
  :type '(repeat symbol))

(defcustom ranger-dired-hide-mask '(nil nil nil nil nil nil nil t)
  "Contols hiding/transforming columns.  If set, its value must be a list of
symbols, one for each attributes column.  If the symbol is nil, then the
corresponding column will be hidden, and if it's not nil then the column will be
left untouched.  The symbol may also be the name of a function that takes one
string argument and evaluates to a different string -- in this case this
function will be used to transform the contents of the corresponding column and
its result will be displayed instead."
  :group 'ranger
  :type '(repeat symbol))

(defun ranger-mask-attributes (beg end mask)
  "Manage the hiding of attributes in region from BEG to END.
Selective hiding of specific attributes can be controlled by MASK."
  (let ((cursor beg) props)
    (cl-labels ((ranger-make-display-props
            (display-function-or-flag)
            (cond ((functionp display-function-or-flag)
                   `(display
                     ,(apply display-function-or-flag
                             (list (buffer-substring cursor (1- (point)))))))
                  ((null display-function-or-flag) '(invisible t))
                  (t nil))))
      (if t
          (block block
            (mapc (lambda (do-display)
                    (search-forward-regexp "\\w")
                    (search-forward-regexp "\\s-")
                    (forward-char -1)
                    (setq props (ranger-make-display-props do-display))
                    (when props
                      (add-text-properties cursor (point) props))
                    (setq cursor (point))
                    (if (>= (point) end) (return-from block)))
                  mask))
        (unless (>= cursor end)
          (add-text-properties cursor (1- end) '(invisible t)))))))

(defun ranger-mask-details (mask)
  "Manage the display of file attributes with defined `MASK'."
  (let ((inhibit-read-only t)
        (beg (point-min))
        (end (point-max))
        (next))
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (null next) (< (point) end))
        (forward-line 1)
        (setq next (dired-move-to-filename)))
      (while (and next (< next end))
        (beginning-of-line)
        (forward-char 1)
        (remove-text-properties (point) next '(invisible t))
        (remove-text-properties (point) next '(display))
        (ranger-mask-attributes (point) next mask)
        (forward-line 1)
        (setq next (dired-move-to-filename))))))

(defun ranger-mask-show-details ()
  (interactive)
  (ranger-mask-details ranger-dired-display-mask))

(defun ranger-mask-hide-details ()
  (interactive)
  (ranger-mask-details ranger-dired-hide-mask))

(defun ranger-truncate ()
  "Truncate lines."
  (setq truncate-lines t))

(defvar ranger-parent-dir-hook '(;; ranger-to-dired
                                 ;; revert-buffer
                                 ranger-hide-details
                                 ranger-sort
                                 ranger-filter-files
                                 ranger-sub-window-setup
                                 ))

(defvar ranger-normal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; basics
    (define-key map "?"                'ranger-help)
    (define-key map "du"               'ranger-show-size)
    (define-key map "q"                'ranger-close)
    ;; (define-key map (kbd "<ESC>")   'ranger-close)
    (define-key map "ZZ"               'ranger-close)
    (define-key map "Q"                'ranger-disable)
    (define-key map "ZQ"               'ranger-disable)
    (define-key map (kbd "C-r")        'ranger-refresh)

    ;; bookmarks
    (define-key map (kbd "`")          'ranger-goto-mark)
    (define-key map (kbd "'")          'ranger-goto-mark)
    (define-key map "B"                'ranger-show-bookmarks)
    (define-key map "m"                'ranger-create-mark)
    (define-key map "um"               'ranger-remove-mark)

    ;; marking
    (define-key map "v"                'dired-toggle-marks)
    (define-key map (kbd "C-SPC")      'ranger-mark)
    (define-key map (kbd "TAB")        'ranger-mark)
    (define-key map (kbd "\"")         'dired-mark-files-regexp)
    (define-key map (kbd "uv")         'dired-unmark-all-marks)
    (define-key map "t"                'ranger-toggle-mark)

    ;; dired commands
    (define-key map "!"                'dired-do-shell-command)
    (define-key map "D"                'dired-do-delete)
    (define-key map "R"                'dired-do-rename)
    (define-key map "+"                'mkdir)
    (define-key map "="                'dired-diff)

    ;; navigation
    (define-key map "-"                'ranger-up-directory)
    (define-key map "G"                'ranger-goto-bottom)
    (define-key map "h"                'ranger-up-directory)
    (define-key map "j"                'ranger-next-file)
    (define-key map "k"                'ranger-prev-file)
    (define-key map "l"                'ranger-find-file)
    (define-key map (kbd "C-f")        'ranger-page-down)
    (define-key map (kbd "C-b")        'ranger-page-up)
    (define-key map "J"                'ranger-half-page-down)
    (define-key map "K"                'ranger-half-page-up)
    (define-key map (kbd "C-d")        'ranger-half-page-down)
    (define-key map (kbd "C-u")        'ranger-half-page-up)
    (define-key map [left]             'ranger-up-directory)
    (define-key map [down]             'ranger-next-file)
    (define-key map [up]               'ranger-prev-file)
    (define-key map [right]            'ranger-find-file)
    (define-key map (kbd "RET")        'ranger-find-file)

    ;; jumping around
    (define-key map "["                'ranger-prev-parent)
    (define-key map "]"                'ranger-next-parent)
    (define-key map "}"                'ranger-find-file)
    (define-key map "f"                'ranger-travel)

    ;; going
    (define-key map "g"                'ranger-go)

    ;; history
    (define-key map "zz"               'ranger-show-history)
    (define-key map "H"                'ranger-prev-history)
    (define-key map "L"                'ranger-next-history)

    ;; subtrees
    (define-key map "I"                'ranger-insert-subdir)

    (define-key map ">"                'dired-next-dirline)
    (define-key map "<"                'dired-prev-dirline)

    ;; preview windows
    (define-key map "i"                'ranger-preview-toggle)
    (define-key map (kbd "C-j")        'ranger-scroll-page-down)
    (define-key map (kbd "C-k")        'ranger-scroll-page-up)
    (define-key map "zp"               'ranger-toggle-details)
    ;; TODO map zc    toggle_option collapse_preview
    (define-key map "zi"               'ranger-toggle-literal)
    (define-key map "zf"               'ranger-toggle-scale-images)

    ;; copy and paste
    (define-key map "yy"               'ranger-copy)
    ;; TODO undo copy - uy
    (define-key map "ya"               'ranger-copy-append)
    ;; TODO remove from copy - yr
    (define-key map "dd"               'ranger-cut)
    ;; TODO undo cut - ud
    (define-key map "da"               'ranger-cut-append)
    ;; TODO remove from cut - dr
    (define-key map "pp"               'ranger-paste)
    (define-key map "po"               'ranger-paste-over)
    (define-key map "pl"               'ranger-paste-as-symlink)
    (define-key map "pL"               'ranger-paste-as-relative-symlink)
    (define-key map "p?"               'ranger-show-copy-contents)

    ;; copy names and paths
    (define-key map "yp"               'ranger-copy-absolute-file-paths)
    (define-key map "yd"               'ranger-copy-current-dir-path)
    (define-key map "yn"               'ranger-copy-filename)

    ;; settings
    (define-key map "o"                'ranger-sort-criteria)
    (define-key map "z+"               'ranger-more-parents)
    (define-key map "z-"               'ranger-less-parents)
    (define-key map "zh"               'ranger-toggle-dotfiles)
    (define-key map (kbd "C-h")        'ranger-toggle-dotfiles)
    (define-key map "zP"               'ranger-minimal-toggle)
    (define-key map "zd"               'ranger-toggle-dir-first)
    ;; TODO map zf   regexp filter

    ;; tabs
    (define-key map (kbd "C-n")        'ranger-new-tab)
    (define-key map (kbd "C-w")        'ranger-close-tab)
    (define-key map (kbd "C-TAB")      'ranger-next-tab)
    (define-key map (kbd "C-S-TAB")    'ranger-prev-tab)
    (define-key map (kbd "M-<Right>")  'ranger-next-tab)
    (define-key map (kbd "M-<Left>")   'ranger-prev-tab)
    (define-key map "uq"               'ranger-restore-tab)

    ;; define M + number bindings to access tabs.
    (define-key map "\M-1" '(lambda () (interactive) (ranger-goto-tab 1)))
    (define-key map "\M-2" '(lambda () (interactive) (ranger-goto-tab 2)))
    (define-key map "\M-3" '(lambda () (interactive) (ranger-goto-tab 3)))
    (define-key map "\M-4" '(lambda () (interactive) (ranger-goto-tab 4)))
    (define-key map "\M-5" '(lambda () (interactive) (ranger-goto-tab 5)))
    (define-key map "\M-6" '(lambda () (interactive) (ranger-goto-tab 6)))
    (define-key map "\M-7" '(lambda () (interactive) (ranger-goto-tab 7)))
    (define-key map "\M-8" '(lambda () (interactive) (ranger-goto-tab 8)))
    (define-key map "\M-9" '(lambda () (interactive) (ranger-goto-tab 9)))
    (define-key map "\M-0" '(lambda () (interactive) (ranger-goto-tab 0)))
    ;; (cl-loop for num in '(1 2 3 4 5 6 7 8 9)
    ;;          do (eval `(define-key map (concat "\\M-" ,(number-to-string num)) '(lambda() (interactive)(ranger-goto-tab ,num)))))

    ;; search
    (define-key map "/"                'ranger-search)
    (define-key map "n"                'ranger-search-next)
    (define-key map "N"                'ranger-search-previous)

    ;; utilities
    (define-key map (kbd "C-c C-e")    'wdired-change-to-wdired-mode)
    (define-key map "S"                'ranger-pop-eshell)

    ;; file opening
    (define-key map "ws"               'ranger-open-file-vertically)
    (define-key map "wv"               'ranger-open-file-horizontally)
    (define-key map "wf"               'ranger-open-file-new-frame)
    (define-key map "wj"               'ranger-open-file-other-window)
    (define-key map "we"               'ranger-open-in-external-app)

    ;; mouse
    (define-key map (kbd  "<mouse-1>") 'ranger-find-file)
    (define-key map (kbd  "<mouse-3>") 'ranger-up-directory)

    map)
  "Ranger mode map style using ranger style bindings.")

(defvar ranger-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    ;; basics
    (define-key map (kbd "C-h")                'ranger-help)
    (define-key map (kbd "C-x C-c")                'ranger-close)
    (define-key map (kbd "C-x k")                'ranger-disable)
    (define-key map (kbd "C-l")        'ranger-refresh)

    ;; bookmarks
    (define-key map (kbd "C-x rb")          'ranger-goto-mark)
    (define-key map (kbd "C-x rB")                'ranger-show-bookmarks)
    (define-key map (kbd "C-x rm")                'ranger-create-mark)
    (define-key map (kbd "C-x ru")               'ranger-remove-mark)

    ;; marking
    (define-key map (kbd "C-SPC")      'ranger-toggle-mark)
    (define-key map (kbd "TAB")        'ranger-mark)
    (define-key map (kbd "\"")         'dired-mark-files-regexp)
    ;; (define-key map (kbd "uv")         'dired-unmark-all-marks)

    ;; dired commands
    (define-key map (kbd "C-x du")                'ranger-show-size)
    (define-key map (kbd "M-!")                'dired-do-shell-command)
    (define-key map "D"                'dired-do-delete)
    (define-key map "R"                'dired-do-rename)

    ;; navigation
    (define-key map [home]                'ranger-goto-top)
    (define-key map [end]                'ranger-goto-bottom)
    (define-key map (kbd "M-v")        'ranger-page-down)
    (define-key map (kbd "C-v")        'ranger-page-up)
    (define-key map [pagedown]                'ranger-half-page-down)
    (define-key map [pageup]                'ranger-half-page-up)
    (define-key map (kbd "C-b")             'ranger-up-directory)
    (define-key map (kbd "C-n")             'ranger-next-file)
    (define-key map (kbd "C-p")               'ranger-prev-file)
    (define-key map (kbd "C-f")            'ranger-find-file)
    (define-key map [left]             'ranger-up-directory)
    (define-key map [down]             'ranger-next-file)
    (define-key map [up]               'ranger-prev-file)
    (define-key map [right]            'ranger-find-file)
    (define-key map (kbd "RET")        'ranger-find-file)

    ;; jumping around
    (define-key map (kbd "M-[")                'ranger-prev-parent)
    (define-key map (kbd "M-]")                'ranger-next-parent)
    ;; (define-key map "f"                'ranger-travel)

    ;; going
    (define-key map (kbd "C-x g")                'ranger-go)

    ;; history
    ;; (define-key map "zz"               'ranger-show-history)
    (define-key map (kbd "C-u C-SPC")                'ranger-prev-history)

    ;; subtrees
    ;; (define-key map "I"                'ranger-insert-subdir)

    ;; (define-key map ">"                'dired-next-dirline)
    ;; (define-key map "<"                'dired-prev-dirline)

    ;; preview windows
    ;; (define-key map "i"                'ranger-preview-toggle)
    ;; (define-key map (kbd "C-j")        'ranger-scroll-page-down)
    ;; (define-key map (kbd "C-k")        'ranger-scroll-page-up)
    ;; (define-key map "zp"               'ranger-toggle-details)
    ;; TODO map zc    toggle_option collapse_preview
    ;; (define-key map "zi"               'ranger-toggle-literal)
    ;; (define-key map "zf"               'ranger-toggle-scale-images)

    ;; copy and paste
    ;; (define-key map "yy"               'ranger-copy)
    ;; TODO undo copy - uy
    ;; (define-key map "ya"               'ranger-copy-append)
    ;; ;; TODO remove from copy - yr
    ;; (define-key map "dd"               'ranger-cut)
    ;; ;; TODO undo cut - ud
    ;; (define-key map "da"               'ranger-cut-append)
    ;; ;; TODO remove from cut - dr
    ;; (define-key map "pp"               'ranger-paste)
    ;; (define-key map "po"               'ranger-paste-over)
    ;; ;; TODO paste link - pl
    ;; (define-key map "p?"               'ranger-show-copy-contents)

    ;; ;; copy names and paths
    ;; (define-key map "yp"               'ranger-copy-absolute-file-paths)
    ;; (define-key map "yd"               'ranger-copy-current-dir-path)
    ;; (define-key map "yn"               'ranger-copy-filename)

    ;; ;; settings
    ;; (define-key map (kbd "C-x o")                'ranger-sort-criteria)
    ;; (define-key map "z+"               'ranger-more-parents)
    ;; (define-key map "z-"               'ranger-less-parents)
    ;; (define-key map "zh"               'ranger-toggle-dotfiles)
    ;; (define-key map (kbd "C-h")        'ranger-toggle-dotfiles)
    ;; (define-key map "zP"               'ranger-minimal-toggle)
    ;; (define-key map "zd"               'ranger-toggle-dir-first)
    ;; TODO map zf   regexp filter

    ;; tabs
    (define-key map (kbd "C-n")        'ranger-new-tab)
    (define-key map (kbd "C-w")        'ranger-close-tab)
    (define-key map (kbd "C-TAB")      'ranger-next-tab)
    (define-key map (kbd "C-S-TAB")    'ranger-prev-tab)
    (define-key map (kbd "M-<Right>")  'ranger-next-tab)
    (define-key map (kbd "M-<Left>")   'ranger-prev-tab)
    (define-key map "uq"               'ranger-restore-tab)

    ;; define M + number bindings to access tabs.
    (define-key map "\M-1"             '(lambda () (interactive) (ranger-goto-tab 1)))
    (define-key map "\M-2"             '(lambda () (interactive) (ranger-goto-tab 2)))
    (define-key map "\M-3"             '(lambda () (interactive) (ranger-goto-tab 3)))
    (define-key map "\M-4"             '(lambda () (interactive) (ranger-goto-tab 4)))
    (define-key map "\M-5"             '(lambda () (interactive) (ranger-goto-tab 5)))
    (define-key map "\M-6"             '(lambda () (interactive) (ranger-goto-tab 6)))
    (define-key map "\M-7"             '(lambda () (interactive) (ranger-goto-tab 7)))
    (define-key map "\M-8"             '(lambda () (interactive) (ranger-goto-tab 8)))
    (define-key map "\M-9"             '(lambda () (interactive) (ranger-goto-tab 9)))
    (define-key map "\M-0"             '(lambda () (interactive) (ranger-goto-tab 0)))

    ;; search
    ;; (define-key map "/"                'ranger-search)
    ;; (define-key map "n"                'ranger-search-next)
    ;; (define-key map "N"                'ranger-search-previous)

    ;; utilities
    (define-key map (kbd "C-c C-e")    'wdired-change-to-wdired-mode)
    ;; (define-key map "S"                'ranger-pop-eshell)

    ;; file opening
    (define-key map (kbd "C-x ws")               'ranger-open-file-vertically)
    (define-key map (kbd "C-x wv")               'ranger-open-file-horizontally)
    (define-key map (kbd "C-x wf")               'ranger-open-file-new-frame)
    (define-key map (kbd "C-x wj")               'ranger-open-file-other-window)
    (define-key map (kbd "C-x we")               'ranger-open-in-external-app)

    ;; mouse
    (define-key map (kbd  "<mouse-1>") 'ranger-find-file)
    (define-key map (kbd  "<mouse-3>") 'ranger-up-directory)

    map)
  "Ranger mode map style using emacs style bindings.")

(defvar ranger-mode-map
  (let ((map (make-sparse-keymap)))
    ;; define bindings based on
    (cl-case ranger-map-style
      ('ranger
       (set-keymap-parent map ranger-normal-mode-map))
      ('dired
       (set-keymap-parent map dired-mode-map))
      ('emacs
       (progn
         ;; allow all emacs modes to overwrite dired mapping
         (set-keymap-parent ranger-emacs-mode-map dired-mode-map)
         (set-keymap-parent map ranger-emacs-mode-map)
         )))
    ;; define a prefix for all dired commands
    (define-prefix-command 'ranger-dired-map nil "Dired-prefix")
    (setq ranger-dired-map (copy-tree dired-mode-map))
    (define-key map ";" ranger-dired-map)
    map)
  "Define child mode for ranger-mode." )

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
       )))

(defmacro r--fclear (parameter)
  `(r--fset ,parameter nil))


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


;; data structures

(cl-defstruct (ranger
               (:conc-name ranger-)
               (:constructor make-ranger))
  (id nil)
  (window nil)
  (frame nil)
  (buffer nil)
  (track-file nil)
  (entry-buffer nil)
  (window-conf nil)
  (tab-index nil)
  (history nil)
  (minimal nil))

(ranger-id
 (make-ranger))

(cl-defstruct (ranger-window (:constructor ranger-track-window))
  prev-buffer curr-buffer curr-tab history)

(defun ranger-track-window (window &optional prev curr tab)
  (let ((new-win
         (ranger--track-window
          :prev-buffer prev
          :curr-buffer curr
          :curr-tab tab
          :history (make-ring ranger-history-length))))
    (r--aput ranger-w-alist
             window
             new-win)))

(cl-defstruct (ranger-tab (:constructor ranger--new-tab)) name path)

(defun ranger-make-tab (index name path)
  (let ((new-tab (ranger--new-tab :name name :path path)))
    (r--aput ranger-t-alist
             index
             new-tab)))

;; (ranger-track-window (selected-window) (current-buffer) (current-buffer) 1)
;; (ranger-make-tab 2 "hsello" "d:/tabs")


;; mappings

;;;###autoload
(when ranger-key
  (add-hook 'dired-mode-hook
            (defun ranger-set-dired-key ()
              (define-key dired-mode-map ranger-key 'deer-from-dired))))

(defun ranger-define-additional-maps (&optional mode)
  "Define additional mappings for ranger-mode that can't simply be in the defvar (depend on packages)."

  ;; custom ranger-key
  (when ranger-key
    (define-key ranger-mode-map ranger-key 'ranger-to-dired))

  (with-eval-after-load "evil"
    ;; turn off evilified buffers for evilify usage
    (evil-set-initial-state 'ranger-mode 'motion)

    ;; allow cursor to be cleared
    (when ranger-hide-cursor
      (defadvice evil-refresh-cursor (around evil activate)
        (unless (or (eq major-mode 'ranger-mode)
                    (memq (current-buffer) ranger-parent-buffers))
          ad-do-it))))

  ;; make sure isearch is cleared before we delete the buffer on exit
  (add-hook 'ranger-mode-hook '(lambda () (setq isearch--current-buffer nil))))

;; TODO visual mode bindings don't seem to work
;; normalize keymaps to work with evil mode

(add-hook 'ranger-mode-hook #'ranger--normalize-keymaps)

(defun ranger--normalize-keymaps ()
  (when (boundp 'evil-mode)
    ;; turn off evilified buffers for evilify usage
    (evil-make-overriding-map ranger-mode-map 'motion)
    (evil-normalize-keymaps)))

;; wdired integration
(eval-after-load 'wdired
  '(progn
     (defadvice wdired-change-to-wdired-mode (before evil activate)
       (setq ranger-was-ranger (eq major-mode 'ranger-mode))
       (when ranger-was-ranger
         (ranger-to-dired)))
     (defadvice wdired-exit (after evil activate)
       (when ranger-was-ranger
         (ranger-mode)))
     (defadvice wdired-abort-changes (after evil activate)
       (when ranger-was-ranger
         (ranger-mode)))
     (defadvice wdired-finish-edit (after evil activate)
       (when ranger-was-ranger
         (ranger-mode)))))




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
    (message "%s %d item(s) to %s ring [total:%d]"
             (if append "Added" "Copied")
             (length marked-files)
             (if move "cut" "copy")
             (length (cdr (ring-ref ranger-copy-ring 0))))))

(defun ranger-toggle-mark ()
  "Toggle mark on current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (apply 'subst-char-in-region
             (point) (1+ (point))
             (if (eq ?\040 (following-char)) ; SPC
                 (list ?\040 dired-marker-char)
               (list dired-marker-char ?\040))))))

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

(defun ranger-copy-append ()
  "Place selected files in the copy ring and mark to be copied.
`universal-argument' can be used to append to current copy ring."
  (interactive)
  (ranger-update-copy-ring nil t))

(defun ranger-copy (&optional append)
  "Place selected files in the copy ring and mark to be copied.
`universal-argument' can be used to append to current copy ring."
  (interactive "P")
  (ranger-update-copy-ring nil append))

(defun ranger-cut-append ()
  (interactive)
  (ranger-update-copy-ring t t))

(defun ranger-cut (&optional append)
  "Place selected files in the copy ring and mark to be moved.
`universal-argument' can be used to append to current copy ring."
  (interactive "P")
  (ranger-update-copy-ring t append))

(defun ranger--invent-new-name (target-file)
  "Helper function for ranger-paste.
Returns target file name with extra characters appended if necessary to avoid
name clashes."
  (ranger--message "New file would be: %s" target-file)
  (let ((new-target target-file)
        (suffix 1))
    (while (file-exists-p new-target)
      (setq new-target
            (concat target-file "~"
                    (number-to-string suffix)))
      (setq suffix (1+ suffix))
      (ranger--message "Renamed file would be: %s" new-target))
    new-target))

(defun ranger-paste (&optional overwrite)
  "Paste copied files from topmost copy ring."
  (interactive)
  (let* ((current (ring-ref ranger-copy-ring 0))
         (move (car current))
         (fileset (cdr current))
         (target (dired-current-directory))
         (filenum 0))
    (cl-loop for file in fileset do
             (when (file-exists-p file)
               (let* ((from-name (file-name-nondirectory file))
                      (base-target
                       (concat (file-name-directory target)
                               from-name)))
                 (if overwrite
                     (if move
                         (dired-rename-file file target overwrite)
                       (dired-copy-file file target overwrite))
                   (let ((new-target (ranger--invent-new-name base-target)))
                     (if move
                         (unless (string= base-target file)
                           (dired-rename-file file new-target overwrite))
                       (dired-copy-file file new-target overwrite)))))
               (setq filenum (+ filenum 1))))
    ;; show immediate changes in buffer
    (ranger-refresh)
    (message "%s %d/%d item(s) from the copy ring."
             (if move "Moved" "Copied")
             filenum
             (length fileset))))

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
    (message "%s - total size: %s\n%s"
             (propertize move 'face 'font-lock-builtin-face)
             (ranger--get-file-sizes fileset)
             (propertize (string-join fileset "\n") 'face 'font-lock-comment-face)
             )))

(defun ranger-paste-as-symlink (&optional relative)
  "Paste files from topmost copy ring as symbolic links."
  (interactive)
  (let* ((current (ring-ref ranger-copy-ring 0))
         (fileset (cdr current))
         (target (dired-current-directory))
         (filenum 0))
    (cl-loop for file in fileset do
             (when (file-exists-p file)
               (let* ((from-name (file-name-nondirectory file))
                      (target (ranger--invent-new-name
                               (concat (file-name-directory target)
                                       from-name))))
                 (if relative
                     (dired-make-relative-symlink file target)
                   (make-symbolic-link file target)))
               (setq filenum (1+ filenum))))
    ;; show immediate changes in buffer
    (ranger-refresh)
    (message "Pasted %d/%d item(s) as %s symlinks"
             filenum
             (length fileset)
             (if relative "relative" "absolute"))))

(defun ranger-paste-as-relative-symlink ()
  "Paste files from topmost copy ring as relative symbolic links."
  (interactive)
  (ranger-paste-as-symlink t))


;;; copy names and paths

(defun ranger-copy-absolute-file-paths (&optional arg)
  "Copy absolute file path(s) into the kill ring.

Copies either the paths of the marked files (separated by spaces)
or the path of the currently selected file. See
`dired-copy-filename-as-kill'."
  (interactive "P")
  (dired-copy-filename-as-kill (or arg 0)))

(defun ranger-copy-current-dir-path ()
  "Copy the current directory's (`default-directory''s) absolute
path."
  (interactive)
  (message (kill-new (expand-file-name default-directory))))

(defalias 'ranger-copy-filename 'dired-copy-filename-as-kill
  "Copy file name(s) into the kill ring.

Copies either the names of the marked files (separated by spaces)
or the name of the currently selected file.")



(defun ranger-pop-eshell (&optional arg)
  "Create an eshell window below selected window, working directory."
  (interactive)
  (let ((tmp (generate-new-buffer "*temp*")))
    (if (r--fget ranger-minimal)
        (display-buffer-below-selected tmp '((window-height . 7)))
      (display-buffer-at-bottom tmp '((window-height . 7))))
    (select-window
     (get-buffer-window tmp))
    (eshell)
    (kill-buffer tmp)
    (add-hook 'eshell-exit-hook
              '(lambda () (unless (one-window-p) (delete-window))
                 (select-window 'ranger-window)) nil t)))


;;; delayed function creation

(defmacro ranger-define-delayed (func-sym delay)
  "Define a delayed version of FUNC-SYM with delay time DELAY.
When called, a delayed function only runs after the idle time
specified by DELAY. Multiple calls to the same function before
the idle timer fires are ignored."
  (let* ((func-str (symbol-name func-sym))
         (new-func (intern (format "%s-delayed" func-str)))
         (time-var (intern (format "%s-delay-time" func-str)))
         (timer (intern (format "%s-delay-timer" func-str))))
    `(progn (defvar ,time-var ,delay)
            (defvar ,timer nil)
            (defun ,new-func ()
              ,(format "Delayed version of %s" func-str)
              (unless (timerp ,timer)
                (setq ,timer
                      (run-with-idle-timer
                       ,delay nil
                       (lambda ()
                         (,func-sym)
                         (setq ,timer nil)))))))))

;; define delayed functions
(ranger-define-delayed ranger-details-message ranger-footer-delay)
(ranger-define-delayed ranger-setup-preview ranger-preview-delay)


;;tabs

(defun ranger--available-tabs ()
  "Returns list of unused tabs."
  (let* ((tabs
          (r--akeys ranger-t-alist))
         (total-tabs
          (number-sequence 1 ranger-max-tabs))
         (available-tabs
          (cl-remove-if (lambda (tab) (member tab tabs)) total-tabs)))
    available-tabs))

(defun ranger-new-tab (&optional index no-refresh)
  "Create new tab using optional `INDEX' with the current directory."
  (interactive)
  (let* ((available-tabs (ranger--available-tabs))
         (index (or index
                    (and available-tabs
                         (apply 'min available-tabs))
                    ranger-current-tab)))
    (ranger--message "tab index : %s" index)
    (when (and index (<= index ranger-max-tabs))
      (setq ranger-current-tab index)
      (ranger-update-tab index)
      (unless no-refresh
        (ranger-setup-preview)))))

(defun ranger-close-tab (&optional index)
  (interactive)
  (let ((index (or index
                   ranger-current-tab)))
    (when (and index (> (length ranger-t-alist) 1))
      (setq ranger-undo-tab (r--aget ranger-t-alist index))
      (r--aremove ranger-t-alist index)
      (ranger-prev-tab))))

(defun ranger-restore-tab ()
  (interactive)
  (let* ((available-tabs (ranger--available-tabs))
         (index (or (and available-tabs
                         (apply 'min available-tabs))
                    ranger-current-tab)))
    (ranger--message "tab restored index : %s" index)
    (when (and index
               (<= index ranger-max-tabs)
               ranger-undo-tab)
      (r--aput ranger-t-alist
               index
               ranger-undo-tab)
      (setq ranger-undo-tab nil)
      (ranger-setup-preview))))

(defun ranger-other-tab (dir &optional index)
  (interactive)
  (let* ((tabs (r--akeys ranger-t-alist))
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
      (r--aput ranger-t-alist
               index
               (cons relative entry)))))

(defun ranger-goto-tab (index)
  (interactive)
  (let ((tab (r--aget ranger-t-alist index)))
    (when tab
      (setq ranger-current-tab index)
      (ranger-find-file (cdr tab)))))


;; searching

(defun ranger-search ()
  (interactive)
  (if (and (fboundp 'evil-motion-state-p)
           (evil-motion-state-p))
      (evil-search-forward)
    (isearch-forward)))

(defun ranger-search-next ()
  (interactive)
  (if (and (fboundp 'evil-motion-state-p)
           (evil-motion-state-p))
      (evil-search-next)
    (isearch-repeat-forward)))

(defun ranger-search-previous ()
  (interactive)
  (if (and (fboundp 'evil-motion-state-p)
           (evil-motion-state-p))
      (evil-search-previous)
    (isearch-repeat-backward)))


;; marks

(defun ranger-show-bookmarks ()
  "Show bookmark prompt for all bookmarked directories."
  (interactive)
  (let ((bookmark
         (completing-read "Select from bookmarks: "
                          (ranger--directory-bookmarks) )))
    (when bookmark (ranger-find-file bookmark))))

(defun ranger--directory-bookmarks ()
  "Return a list of all bookmarks linking to a directory."
  (cl-remove-duplicates
   (cl-loop for bm in bookmark-alist
            for fname = (cdr-safe (assq 'filename bm))
            when (and fname (file-directory-p fname)) collect fname into new
            finally return new)
   :test (lambda (x y) (or (null y) (equal x y)))))

(defun ranger-create-mark (mark)
  "Create new bookmark using internal bookmarks, designating bookmark name as
ranger-`CHAR'."
  (interactive "cm-")
  (let ((mark-letter (char-to-string mark)))
    (unless (string= mark-letter "")
      (bookmark-set (concat "ranger-" mark-letter))
      (message "Bookmarked directory %s as `ranger-%s'"
               default-directory mark-letter))))

(defun ranger-remove-mark ()
  (interactive)
  (let* ((mark
          (read-key (ranger--marks)))
         (mark-letter (char-to-string mark))
         (bookmark-name (concat "ranger-" mark-letter))
         (bookmark-path (bookmark-location bookmark-name)))
    (when (and bookmark-path (file-directory-p bookmark-path))
      (bookmark-delete bookmark-name))))

(defun ranger-goto-mark ()
  "Go to bookmarks specified from `ranger-create-mark'."
  (interactive)
  (let* ((mark
          (read-key (ranger--marks)))
         (mark-letter (char-to-string mark))
         (bookmark-name (concat "ranger-" mark-letter))
         (bookmark-path (bookmark-location bookmark-name)))
    (when (and bookmark-path (file-directory-p bookmark-path))
      (ranger-find-file bookmark-path))))

(defun ranger--marks ()
  "Concatenated list of bookmarks."
  (mapconcat
   #'(lambda (bm)
       (when (and
              (string-match "ranger-" (car  bm))
              (file-directory-p (cdr (cadr bm))))
         (replace-regexp-in-string "ranger-" "" (car bm))))
   bookmark-alist " "))

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
    (message "ranger-history : %i/%i" (+ 1 goto-idx) (ring-length ranger-history-ring))
    (when (and (not (= goto-idx curr-index)) jump-history)
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
  (ranger-show-file-details))

(defun ranger-help ()
  "Show help message for ranger basics."
  (interactive)
  (message "[h/l]-back/forward [j/k]-up/down [f]ind-file [i]nspect-file [^R]eload [S]hell [H/L]-history back/forward"))

(defun ranger-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq ranger-show-hidden (cl-case ranger-show-hidden
                             ('format 'prefer)
                             ('prefer 'hidden)
                             ('hidden 'format)))
  (ranger-setup)
  (message (format "Show Dotfiles: %s"  ranger-show-hidden)))

(defun ranger-toggle-dir-first ()
  "Sort / unsort directories first."
  (interactive)
  (setq ranger-listing-dir-first (not ranger-listing-dir-first))
  (ranger-setup)
  (message (format "Sort directories first: %s"  ranger-listing-dir-first)))

(defun ranger-toggle-details ()
  "Show/hide dot-files."
  (interactive)
  (setq ranger-deer-show-details (not ranger-deer-show-details))
  (ranger-setup)
  ;; (ranger-mask-details)
  (message (format "Show file details: %s"  ranger-deer-show-details)))

(defun ranger-filter-files ()
  "Omit and filter files in ranger."
  (let ((omit-re (append ranger-format-regexp
                         (cl-case ranger-show-hidden
                           ('prefer ranger-prefer-regexp)
                           ('hidden ranger-prefer-regexp ranger-hidden-regexp)))))
                 (ranger-omit-files omit-re)))

(defun ranger-omit-files (regexp)
  (interactive)
  (let ((omit-re (mapconcat 'concat regexp "\\|"))
        (old-modified-p (buffer-modified-p))
        count)
    (or (string= omit-re "")
        (let ((dired-marker-char 16))
          (if (dired-mark-unmarked-files omit-re nil nil 'no-dir)
              (progn
                (ranger--message "deleting hidden files")
                (setq count (dired-do-kill-lines nil ""))
                (force-mode-line-update)))))
    ;; Try to preserve modified state of buffer.  So `%*' doesn't appear
    ;; in mode-line of omitted buffers.
    ;; (set-buffer-modified-p (and old-modified-p
    ;;                             (save-excursion
    ;;                               (goto-char (point-min))
    ;;                               (re-search-forward dired-re-mark nil t))))
    count))

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

(defun ranger-sort (&optional force)
  "Perform current sort on directory. Specify `FORCE' to sort even when
`ranger-persistent-sort' is nil."
  ;; TODO dired-sort-other only does this:
  ;;   (setq dired-actual-switches switches)
  (setq-local ls-lisp-dirs-first ranger-listing-dir-first)
  (dired-sort-other
   (concat dired-listing-switches
           (when (or force
                     ranger-persistent-sort)
             ranger-sorting-switches))))


;; preview windows functions

(defun ranger-preview-toggle ()
  "Toggle preview of selected file."
  (interactive)
  (if (r--fget ranger-minimal)
      (message "Currently in deer mode. Previews are disabled.")
    (setq ranger-preview-file (not ranger-preview-file))
    (if ranger-preview-file
        (progn
          (ranger-hide-details)
          (ranger-setup-preview))
      (progn
        (when (and ranger-preview-window
                   (eq (selected-frame) (window-frame ranger-preview-window))
                   (window-live-p ranger-preview-window)
                   (window-at-side-p ranger-preview-window 'right))
          (ignore-errors
            (delete-window ranger-preview-window)))
        (ranger-show-details)))))

(defun ranger-toggle-scale-images ()
  "Show/hide dot-files."
  (interactive)
  (if ranger-image-fit-window ; if currently showing
      (setq ranger-image-fit-window nil)
    (setq ranger-image-fit-window t))
  (when ranger-preview-file
    (ranger-setup-preview))
  (message "Fit Images to Window: %s"  ranger-image-fit-window))

(defun ranger-toggle-literal ()
  "Toggle showing literal / actual preview of file."
  (interactive)
  (if ranger-show-literal
      (setq ranger-show-literal nil)
    (setq ranger-show-literal t))
  (when ranger-preview-file
    (ranger-setup-preview))
  (message "Literal Preview: %s"  ranger-show-literal))

(defun ranger-scroll-page-down ()
  "Scroll preview window up."
  (interactive)
  (scroll-other-window))

(defun ranger-scroll-page-up ()
  "Scroll preview window down."
  (interactive)
  (scroll-other-window '-))


;; dired navigation

;; TODO reenter deer if navigating to  directory.
(defun ranger-travel ()
  "Open a file or go to a directory in current buffer."
  (interactive)
  (let ((prompt "Travel: "))
    (cond
     ((featurep 'helm)
      (ranger-find-file (helm-read-file-name prompt)))
     ((featurep 'ivy)
      (ivy-read prompt 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :action
                (lambda (x)
                  (with-ivy-window
                    (ranger-find-file (expand-file-name x default-directory))))))
     (t
      (ranger-find-file (ido-read-file-name prompt))))))

(defun ranger-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (ranger-parent-directory current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (ranger-show-drives))
      (progn
        (ranger-find-file parent)
        (dired-goto-file current)))))

(defun ranger-save-window-settings (&optional overwrite)
  (let ((frame (window-frame))
        (window (selected-window))
        (minimal (r--fget ranger-minimal)))
    (unless minimal
      (r--aput ranger-f-alist
               frame
               (current-window-configuration)
               (null overwrite))
      (delete-other-windows))
    (r--aput ranger-w-alist
             window
             (cons (cons (current-buffer) (point)) nil)
             (null overwrite))))

(defun ranger-find-file (&optional entry ignore-history)
  "Find file in ranger buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in ranger. `IGNORE-HISTORY' will not update history-ring on change"
  (interactive)
  (let ((find-name (or entry
                       (dired-get-filename nil t)))
        (minimal (r--fget ranger-minimal))
        (bname (buffer-file-name (current-buffer))))
    (when find-name
      (if (file-directory-p find-name)
          (progn
            (ranger--message "opening directory: %s" find-name)
            (ranger-save-window-settings)
            (ranger--message "settings saved: %s" find-name)
            (unless ignore-history
              (ranger-update-history find-name))
            (advice-add 'dired-readin :after #'ranger-setup-dired-buffer)
            (switch-to-buffer
             ;; TODO make separate buffer of directory if more than one already exists.
             (or (car (or (dired-buffers-for-dir find-name) ()))
                 (dired-noselect find-name)))
            ;; select origination file
            (when (and bname (file-exists-p bname))
              (dired-goto-file bname))
            ;; reset minimal setting
            (if minimal
                (r--fset ranger-minimal t)
              (r--fset ranger-minimal nil))
            (ranger-parent-child-select)
            (ranger--message "setting up ranger windows: %s" find-name)
            (ranger-mode)
            (ranger--message "DONE")
            ;; (dired-unadvertise find-name)
            )
        (progn
          (ranger--message "opening file: %s" find-name)
          (find-file find-name)
          (ranger-still-dired)
          )))))

;; TODO closing one deer window disables both
(defun ranger-open-file (&optional mode)
      "Find file in ranger buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in ranger. `IGNORE-HISTORY' will not update history-ring on change"
      (let ((marked-files (dired-get-marked-files)))
        (cl-loop for find-name in marked-files do
                 (let ((dir-p (file-directory-p find-name))
                       (min (r--fget ranger-minimal)))
                   (when (and find-name)
                     (cl-case mode
                       ('frame
                        (let ((goto-frame (make-frame)))
                          (select-frame-set-input-focus goto-frame)))
                       ('horizontal
                        (when (or min (not  dir-p))
                          (unless min
                            (ranger-disable))
                          (split-window-right)
                          (windmove-right)))
                       ('vertical
                        (when (or min (not dir-p))
                          (unless min
                            (ranger-disable))
                          (split-window-below)
                          (windmove-down)))
                       ('other
                        (when (or min (not dir-p))
                          (unless min
                            (ranger-disable))
                          (other-window 1))))
                     (ranger-find-file find-name))))))

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
  (ranger-open-file 'horizontal))

(defun ranger-open-file-vertically ()
  "Open current file as a split with previously opened window"
  (interactive)
  (ranger-open-file 'vertical))

(defun ranger-open-file-new-frame ()
  "Open current file in a new frame."
  (interactive)
  (ranger-open-file 'frame))

(defun ranger-open-file-other-window ()
  "Open current file in `other-window'."
  (interactive)
  (ranger-open-file 'other))

(defun ranger-insert-subdir ()
  "Insert subdir from selected folder."
  (interactive)
  (let ((find-name (dired-get-filename nil t)))
    (if (file-directory-p find-name)
        (progn
          ;; (setq header-line-format nil)
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
  (ranger-prev-file 1))

(defun ranger-page-down ()
  "Move to top of file list"
  (interactive)
  (ranger-next-file (window-height)))

(defun ranger-half-page-down ()
  "Move to top of file list"
  (interactive)
  (ranger-next-file (/ (window-height) 2)))

(defun ranger-goto-bottom ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-max))
  (ranger-next-file 1))

(defun ranger-page-up ()
  "Move to top of file list"
  (interactive)
  (ranger-prev-file (window-height)))

(defun ranger-half-page-up ()
  "Move to top of file list"
  (interactive)
  (ranger-prev-file (/ (window-height) 2)))

(defun ranger-go (path)
  "Go subroutine"
  (interactive
   (list
    (read-char-choice
     "e   : /etc
u   : /usr
d   : /dev
l   : follow directory link
L   : follow selected file
o   : /opt
v   : /var
h   : ~/
m   : /media
M   : /mnt
s   : /srv
r,/ : /
R   : ranger . el location
> "
     '(?q ?e ?u ?d ?l ?L ?o ?v ?m ?M ?s ?r ?R ?/ ?h ?g ?D ?j ?k ?T ?t ?n ?c))))
  (message nil)
  (let* ((c (char-to-string path))
         (new-path
          (cl-case (intern c)
            ('e "/etc")
            ('u "/usr")
            ('d "/dev")
            ('l (file-truename default-directory))
            ('L (file-truename (dired-get-filename)))
            ('o "/opt")
            ('v "/var")
            ('m "/media")
            ('M "/mnt")
            ('s "/srv")
            ('r "/")
            ('R (file-truename (file-name-directory (find-library-name "ranger.el"))))
            ('h  "~/")
            ('/ "/")))
         (alt-option
          (cl-case (intern c)
            ;; Subdir Handlng
            ('j 'ranger-next-subdir)
            ('k 'ranger-prev-subdir)
            ;; Tab Handling
            ('n 'ranger-new-tab)
            ('T 'ranger-prev-tab)
            ('t 'ranger-next-tab)
            ('c 'ranger-close-tab)
            ('g 'ranger-goto-top))))
    (when (string-equal c "q")
      (keyboard-quit))
    (when (and new-path (file-directory-p new-path))
      (ranger-find-file new-path))
    (when (eq system-type 'windows-nt)
      (when (string-equal c "D")
        (ranger-show-drives)))
    (when alt-option
      (call-interactively alt-option))))

(defun ranger-next-file (arg)
  "Move lines in ranger and initiate updates to preview window."
  (interactive "^p")
  (let ((cur (point)))
    (dired-next-line arg)
    (cond
     ((eobp) (progn (forward-line -1)
                    (dired-next-line 0)))
     ((bobp) (dired-next-line 0)))
   (ranger-show-file-details)
    ;; don't change preview window if no change
    (when (not (eq (point) cur))
      (when (and (not (r--fget ranger-minimal))
                 ranger-preview-file)
        (when (get-buffer "*ranger-prev*")
          (with-current-buffer "*ranger-prev*"
            (erase-buffer)))
        (ranger-setup-preview-delayed)))))

(defun ranger-prev-file (arg)
  (interactive "^p")
  (ranger-next-file (- 0 arg)))

(defun ranger-show-size ()
  "Show directory size."
  (interactive)
  (ranger-details-message t))

(defun ranger-show-file-details ()
  "Show current file information in the footer"
  (ranger-update-current-file)
  (ranger-details-message-delayed))

(defun ranger--footer-spec (&optional sizes)
  "Echo file details."
    (let* ((entry (dired-get-filename nil t))
           ;; enable to troubleshoot speeds
           ;; (sizes t)
           (filename (file-name-nondirectory entry))
           (fattr (file-attributes entry))
           (file-size (if sizes (concat "File "
                                        (file-size-human-readable (nth 7 fattr))) "Press \'du\' for size info."))
           (dir-size (if sizes (concat "Dir " (ranger--get-file-sizes
                                               (ranger--get-file-listing
                                                dired-directory)))
                       ""))
           (user (nth 2 fattr))
           (file-mount
            (if sizes
                (or (let ((index 0)
                          size
                          return)
                      (dolist (mount (ranger--get-mount-partitions 'mount)
                                     return)
                        (when (string-match (concat "^" mount "/.*") entry)
                          (setq size
                                (nth index
                                     (ranger--get-mount-partitions 'avail)))
                          (setq return (format "%s free (%s)" size mount)))
                        (setq index (+ index 1))
                        ))
                    "") ""))
           (filedir-size (if sizes (ranger--get-file-sizes
                                    (ranger--get-file-listing dired-directory))
                           ""))
           (file-date
            (propertize
             (format-time-string "%Y-%m-%d %H:%m"
                                 (nth 5 fattr))
             'face 'font-lock-warning-face))
           (file-perm (nth 8 fattr))
           (cur-pos (- (line-number-at-pos (point)) 1))
           (final-pos (- (line-number-at-pos (point-max)) 2))
           (position (format "%3d/%-3d"
                             cur-pos
                             final-pos))
           (space "&&&")
           (footer-spec
             `(
              (?U . ,user)
              (?c . ,dir-size)
              (?d . ,file-date)
              (?f . ,file-mount)
              (?m . ,file-perm)
              (?p . ,position)
              (?s . ,file-size)
              (?u . ,filedir-size)
              (?w . ,space)
              )))
      footer-spec
      ))

(defun ranger-details-message (&optional sizes)
  "Echo file details."
  (when (and (dired-get-filename nil t) ranger-footer-format)
    (let* ((fwidth (frame-width))
           (spec (ranger--footer-spec sizes))
           (footer (format-spec
                    ranger-footer-format
                    spec))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth
                     fringe-gap
                     (length lhs)))
           (message-log-max nil)
           (msg
            (format
             ;; "%s"
             (format  "%%s%%%ds" space)
             lhs
             rhs
             )))
      (message "%s" msg))))

(defun ranger-update-current-file ()
  (r--fset ranger-current-file
           (or
            (dired-get-filename nil t)
            dired-directory) nil t))



;; parent window functions
(defun ranger-sub-window-setup ()
  "Parent window options."
  ;; allow mouse click to jump to that directory
  (setq-local mouse-1-click-follows-link nil)
  (local-set-key (kbd  "<mouse-1>") 'ranger-find-file)
  ;; set header-line
  (when ranger-modify-header
    (setq header-line-format `(:eval (,ranger-header-func)))))

(defun ranger-parent-child-select ()
  "Select the current child file in the parent directory."
  (when ranger-child-name
    (dired-goto-file ranger-child-name)))

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
        (current-name (expand-file-name default-directory))
        (i 0)
        (unused-windows ()))

    (setq ranger-buffer (current-buffer))

    (setq ranger-window (get-buffer-window (current-buffer)))

    (setq ranger-visited-buffers (append ranger-parent-buffers ranger-visited-buffers))

    (setq ranger-parent-buffers ())
    (setq ranger-parent-windows ())
    (setq ranger-parent-dirs ())

    (unless (r--fget ranger-minimal)
      (while (and parent-name
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
               (with-selected-window window
                 (ranger-parent-child-select)
                 (ranger-hide-the-cursor)
                 ))))
         nil nil 'nomini))

      ;; (select-window ranger-window)
      )))

(defun ranger-make-parent (parent)
  "Make parent window.  `PARENT' is a construct with ((current . parent) .
slot)."
  ;; (set (make-local-variable 'window-configuration-change-hook) nil)
  (let* ((parent-name (cdar parent))
         (window-configuration-change-hook nil)
         (current-name (caar parent))
         (slot (cdr parent))
         (parent-buffer (ranger-dir-buffer parent-name nil))
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
  "Move down in parent directory"
  (interactive)
  (let ((parent (car ranger-parent-buffers)))
    (if parent
        (with-current-buffer parent
          (dired-next-line 1)
          (let ((curfile (dired-get-filename nil t)))
            (if (file-directory-p curfile)
                (ranger-find-file curfile)
              (dired-next-line -1))))
      (message "No parent directory."))))

(defun ranger-prev-parent ()
  "Move up in parent directory"
  (interactive)
  (let ((parent (car ranger-parent-buffers)))
    (if parent
        (with-current-buffer (car ranger-parent-buffers)
          (dired-next-line -1)
          (let ((curfile (dired-get-filename nil t)))
            (if (file-directory-p curfile)
                (ranger-find-file curfile)
              (dired-next-line 1))))
      (message "No parent directory."))))


;; window creation subroutines
(defun ranger-dir-buffer (entry preview)
  "Open `ENTRY' in dired buffer. Run `PREVIEW' or parent hooks."
  ;; (ignore-errors
  (let ((window-configuration-change-hook nil))
    (with-current-buffer
        (or (car (or (dired-buffers-for-dir entry) ()))
            (dired-noselect entry))
      (if preview
          (run-hooks 'ranger-preview-dir-hook)
        (run-hooks 'ranger-parent-dir-hook))
      (current-buffer))))

(defun ranger-dir-contents (entry)
  "Open `ENTRY' in dired buffer."
  (let ((temp-buffer (or (get-buffer "*ranger-prev*")
                         (generate-new-buffer "*ranger-prev*"))))
    (with-demoted-errors
        (with-current-buffer temp-buffer
          (make-local-variable 'font-lock-defaults)
          (setq font-lock-defaults '((dired-font-lock-keywords) nil t))
          (buffer-disable-undo)
          (setq-local cursor-type nil)
          (erase-buffer)
          (turn-on-font-lock)
          (insert-directory entry (concat dired-listing-switches ranger-sorting-switches) nil t)
          (goto-char (point-min))
          ;; truncate lines in directory buffer
          (ranger-truncate)
          ;; (visual-line-mode nil)
          ;; remove . and .. from directory listing
          (save-excursion
            (while (re-search-forward "total used in directory\\|\\.$" nil t)
              ;; (beginning-of-line)
              (delete-region (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point)))))
          (current-buffer)))))

(defun ranger-preview-buffer (entry-name)
  "Create the preview buffer of `ENTRY-NAME'.  If `ranger-show-literal'
is set, show literally instead of actual buffer."
  (if ranger-show-literal
      ;; show literal version of file
      (let ((temp-buffer (or (get-buffer "*ranger-prev*")
                             (generate-new-buffer "*ranger-prev*"))))
        (with-current-buffer temp-buffer
          (buffer-disable-undo)
          (setq-local cursor-type nil)
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
      (let ((inhibit-modification-hooks t)
            (auto-save-default nil)
            (inhibit-message t))
        (with-current-buffer
            (or
             (find-buffer-visiting entry-name)
             (let ((delay-mode-hooks t)
                   (enable-local-variables nil)
                   ;; don't update recentf list
                   (recentf-list nil))
               (find-file-noselect entry-name t ranger-show-literal)))
          (current-buffer))))))

(defun ranger-setup-image-preview (entry-name)
  "Setup and maybe resize image"
  (when ranger-image-fit-window
    (require 'image-dired))
  (let* ((new-file (expand-file-name image-dired-temp-image-file))
         (file (expand-file-name entry-name))
         (width (* ranger-width-preview ranger-image-scale-ratio (image-dired-display-window-width)))
         (height (image-dired-display-window-height))
         (image-type 'jpeg)
         command ret success)
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
         (window-configuration-change-hook nil)
         (original-buffer-list (buffer-list))
         (inhibit-modification-hooks t)
         (fsize
          (nth 7 (file-attributes entry-name))))
    (when ranger-cleanup-eagerly
      (mapc 'ranger-kill-buffer
            (remove (current-buffer) ranger-preview-buffers))
      (setq ranger-preview-buffers (delq nil ranger-preview-buffers)))
    (when (and (not (r--fget ranger-minimal))
               entry-name
               ranger-preview-file)
      (unless (or
               (> fsize (* 1024 1024 ranger-max-preview-size))
               (member (file-name-extension entry-name)
                       ranger-excluded-extensions))
        (with-demoted-errors "%S"
          (let* ((dir (file-directory-p entry-name))
                 (dired-listing-switches ranger-listing-switches)
                 (preview-buffer (if dir
                                     (ranger-dir-buffer entry-name t)
                                   ;; (ranger-dir-contents entry-name)
                                   (ranger-preview-buffer entry-name)))
                 ;; check for existance of *ranger-prev* buffer
                 (preview-window (and (window-live-p ranger-preview-window)
                                      (eq (selected-frame) (window-frame ranger-preview-window))
                                      ranger-preview-window))
                 )
            (if preview-window
                (with-selected-window preview-window
                  (switch-to-buffer preview-buffer))
              (unless (and (not dir) ranger-dont-show-binary (ranger--prev-binary-p))
                (setq preview-window
                      (display-buffer
                       preview-buffer
                       `(ranger-display-buffer-at-side . ((side . right)
                                                          (slot . 1)
                                                          (inhibit-same-window . t)
                                                          (window-width . ,(- ranger-width-preview
                                                                              (min
                                                                               (- ranger-max-parent-width
                                                                                  ranger-width-parents)
                                                                               (* (- ranger-parent-depth 1)
                                                                                  ranger-width-parents)))))))))
              )
            (with-current-buffer preview-buffer
              (setq-local cursor-type nil)
              (setq-local mouse-1-click-follows-link nil)
              (local-set-key (kbd  "<mouse-1>") #'(lambda ()
                                                    (interactive)
                                                    (select-window ranger-window)
                                                    (call-interactively
                                                     'ranger-find-file)))
              (when ranger-modify-header
                (setq header-line-format `(:eval (,ranger-header-func))))
              (ranger-hide-the-cursor))
            (when (not (memq preview-buffer original-buffer-list))
              (add-to-list 'ranger-preview-buffers preview-buffer))
            (setq ranger-preview-window preview-window)
            ;; (ranger-hide-details)
            (ranger-hide-details)
            ))))))


;; utilities
(defun ranger-parent-directory (entry)
  "Find the parent directory of `ENTRY'."
  (file-name-directory (directory-file-name (expand-file-name entry))))

;; (defun ranger-fix-width (window)
;;   "Fix the width of `WINDOW'."
;;   (with-selected-window window
;;     (setq-local window-size-fixed 'width)))

(defun ranger-show-drives ()
  "Show drive prompt."
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((drive
           (completing-read "Select drive: "
                            (ranger--get-windows-drives))))
      (when drive (ranger-find-file drive)))))

(defun ranger--get-windows-drives ()
  "Return list of drives in MS Windows."
  (if (executable-find "wmic")
      (cdr (split-string (shell-command-to-string "wmic logicaldisk get name")))))

(defun ranger--get-file-sizes (fileset)
  "determine file size of provided list of files in `fileset'."
  (if (and
       fileset
       (executable-find "du"))
      (with-temp-buffer
        (apply 'call-process "du" nil t nil "-sch" fileset)
        (format "%s"
                (progn
                  (re-search-backward "\\(^[0-9.,]+[a-za-z]+\\).*total$")
                  (match-string 1)))) ""))

(defun ranger--get-mount-partitions (mode)
  "Determine device information."
  (if (executable-find "df")
      (with-temp-buffer
        (apply 'call-process "df" nil t t
               (list  "-h"
                      (concat "--output="
                              (cl-case mode
                                ('mount
                                 (if (eq system-type 'windows-nt)
                                     "source"
                                   "target"))
                                ('avail "avail")
                                ('size "size")))))
        (nreverse
         (cdr (split-string
               (replace-regexp-in-string "\n$" "" (buffer-string)) "[\n\r]+"))))
    ()))

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
         (window-configuration-change-hook nil)
         new-window
         reuse-window)

    ;; new window
    (setq new-window (split-window current-window window-size side))
    ;; set parameters
    (set-window-parameter new-window 'window-slot slot)
    (if (version< emacs-version "27")
        (window--display-buffer buffer new-window 'window alist display-buffer-mark-dedicated)
      ;; optional argument `display-buffer-mark-dedicated' was removed in emacs 27 development branch
      ;; (as of 2019-01-14). This is a temporary fix to be re-evaluated once emacs
      ;; 27 development reaches the pretest phase. (docstring to be re-evaluated
      ;; as well)
      (window--display-buffer buffer new-window 'window alist))
    ;; (add-hook 'window-configuration-change-hook 'ranger-window-check)
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

;; Idea from http://emacs.stackexchange.com/questions/10277/make-emacs-automatically-open-binary-files-in-hexl-mode
(defun ranger--prev-binary-p ()
  (when (get-buffer "*ranger-prev*")
    (with-current-buffer "*ranger-prev*"
      (save-excursion
        (goto-char (point-min))
        (search-forward (string ?\x00) nil t 1)))))


;; cleanup and reversion

(defun ranger-kill-buffer (buffer)
  "Delete unmodified buffers and any dired buffer"
  (when
      (and
       (buffer-live-p buffer)
       (or (not (buffer-modified-p buffer))
           (eq 'dired-mode (buffer-local-value 'major-mode buffer))
           (eq 'ranger-mode (buffer-local-value 'major-mode buffer))))
    (kill-buffer buffer)))



(defun ranger-revert (&optional buffer)
  "Revert ranger settings."
  ;; restore window configuration

  (let* ((minimal (r--fget ranger-minimal))
         (ranger-window-props
          (r--aget ranger-w-alist
                   (selected-window)))
         (prev-buffer (caar ranger-window-props))
         (prev-point (cdar ranger-window-props))
         (ranger-buffer (cdr ranger-window-props))
         (config
          (r--aget ranger-f-alist
                   (window-frame))))
    (if (or config prev-buffer)
        (progn
          (r--aremove ranger-w-alist (selected-window))

          (if minimal
              (when (and prev-buffer
                         (buffer-live-p prev-buffer))
                (switch-to-buffer prev-buffer)
                (goto-char prev-point))
            (when (and config
                       (window-configuration-p config))
              (set-window-configuration config)
              (r--aremove ranger-f-alist (window-frame))))

          ;; TODO make separate tabs for each ranger window
          ;; (r--aremove ranger-t-alist ranger-current-tab)

          ;; revert appearance
          (advice-remove 'dired-readin #'ranger-setup-dired-buffer)
          (ranger-revert-appearance (or buffer ranger-buffer))
          (advice-add 'dired-readin :after #'ranger-setup-dired-buffer)
          (setq ranger-pre-saved nil)

          ;; if no more ranger frames or windows
          (when (not (or (ranger-windows-exists-p)
                         (ranger-frame-exists-p)))
            (message "Reverting all buffers")
            ;; remove all hooks and advices
            ;; TODO use established dired-after-readin-hook
            (advice-remove 'dired-readin #'ranger-setup-dired-buffer)
            (remove-hook 'window-configuration-change-hook 'ranger-window-check)

            ;; revert setting for minimal
            (r--fset ranger-minimal nil)

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
              (ranger--message "Cleaning all buffers : %s" all-ranger-buffers)

              (if ranger-cleanup-on-disable
                  (mapc 'ranger-kill-buffer all-ranger-buffers)
                (mapc 'ranger-revert-appearance all-ranger-buffers)))

            ;; kill preview buffer
            (when (get-buffer "*ranger-prev*")
              (kill-buffer (get-buffer "*ranger-prev*")))

            (setq ranger-f-alist ())
            (setq ranger-w-alist ())

            ;; clear variables
            (setq ranger-preview-buffers ()
                  ranger-visited-buffers ()
                  ranger-parent-buffers ()))

          ;; clear ranger-show-file-details information
          (message "%s" ""))
      ;; config is gone, kill buffer
      )))

(defun ranger-revert-appearance (buffer)
  "Revert the `BUFFER' to pre-ranger defaults without closing ranger session."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; revert buffer local modes used in ranger
      (ranger--message "Reverting appearance to buffer : %s" buffer)

      ;; BUG : dired auto revert adds header / . / .. when window not active
      (unless ranger-pre-hl-mode
        (hl-line-mode -1))
      (unless ranger-pre-arev-mode
        (auto-revert-mode -1))
      (setq header-line-format ranger-pre-header-format)
      (when (derived-mode-p 'dired-mode)
        (setq dired-listing-switches ranger-pre-dired-listing)
        (ranger-show-details)
        ;; (ranger-show-details)
        ;; revert ranger-mode
        ;; (setq ranger-mode nil)
        ;; hide details line at top
        (funcall 'remove-from-invisibility-spec 'dired-hide-details-information)
        ;; sort dired with previous listing options
        (dired-sort-other dired-listing-switches)
        ))))

(defun ranger-still-dired ()
  "Enable or disable ranger based on current mode"
  (ranger--message "ranger-still-dired : mode %s : window : %s"
                   major-mode
                   (selected-window))
  ;; TODO Try to manage new windows / frames created without killing ranger
  (let* ((ranger-window-props
          (r--aget ranger-w-alist
                   (selected-window)))
         (prev-buffer (caar ranger-window-props))
         (minimal (r--fget ranger-minimal))
         (ranger-buffer (cdr ranger-window-props))
         (current (current-buffer))
         (buffer-fn (buffer-file-name (current-buffer))))
    (cond
     ((and buffer-fn (not (eq  current ranger-buffer)))
      (message "File opened, exiting ranger")
      (ranger-disable)
      (find-file buffer-fn)
      (setq-local cursor-type t)
      (setq header-line-format ranger-pre-header-format)
      (when ranger-return-to-ranger
        (add-hook 'kill-buffer-hook 'ranger nil t)))
     ((eq major-mode 'dired-mode)
      (if minimal
          (deer)
        (ranger)))
     ((not buffer-fn)
      (message "Ranger window was overwritten. Redirecting window to new frame")
      (unless minimal
        (set-window-buffer nil ranger-buffer)
        (when current
          (display-buffer-pop-up-frame current '(inhibit-switch-frame . nil)))))
     (t
      ;; nothing else to do
      ))))

(defun ranger-window-check ()
  "Detect when ranger-window is no longer part of ranger-mode"
  (let* ((windows (window-list))
         (ranger-window-props
          (r--aget ranger-w-alist
                   (selected-window)))
         (prev-buffer (caar ranger-window-props))
         (ranger-windows (r--akeys ranger-w-alist))
         (ranger-buffer (cdr ranger-window-props))
         (ranger-frames (r--akeys ranger-f-alist)))
    ;; if all frames and windows are killed, revert buffer settings
    (ranger--message "Window Check (%s) : %s
        buffer: %s
        w:%s f:%s "
                     major-mode
                     last-command
                     (current-buffer)
                     (and (memq (selected-window) ranger-windows) t)
                     (and (memq (selected-frame) ranger-frames) t))
    ;; TODO deal with new-frame command
    (if (not  (or (ranger-windows-exists-p)
                  (ranger-frame-exists-p)))
        (progn
          (message "All ranger frames have been killed, reverting ranger settings and cleaning buffers.")
          (ranger-revert))
      ;; when still in ranger's window, make sure ranger's primary window and buffer are still here.
      (when ranger-window-props
        ;; Unless selected window does not have ranger buffer
        (when (and (memq (selected-window) ranger-windows)
                   (not (eq major-mode 'ranger-mode)))
          (ranger--message
           "Window Check : Ranger window is not the selected window
** buffer: %s: %s
** window: %s: %s"
           (current-buffer)
           major-mode
           (selected-window)
           (memq (selected-window) ranger-windows) )
          (ranger-still-dired))))))

(defun ranger-windows-exists-p ()
  "Test if any ranger-windows are live."
  (member t
          (mapcar (lambda (l)
                    (and (window-live-p (car l))
                         (buffer-live-p (cddr l))))
                  ranger-w-alist)))

(defun ranger-frame-exists-p ()
  "Test if any ranger-frames are live."
  (mapcar 'frame-live-p
          (r--akeys ranger-f-alist)))

(defun ranger-kill-buffers-without-window ()
  "Will kill all ranger buffers that are not displayed in any window."
  (interactive)
  (cl-loop for buffer in
            (cl-remove-duplicates
             (append
              ranger-preview-buffers
              ranger-parent-buffers
              ranger-visited-buffers)
             :test (lambda (x y) (or (null y) (eq x y)))
             ) do
           (unless (get-buffer-window buffer t)
             (ranger-kill-buffer buffer))))


;; header / mode line
(defun ranger--dir-relative ()
  "Return the topmost directory name in path"
  (let* ((current-name (expand-file-name default-directory))
         (parent-name (ranger-parent-directory default-directory))
         (relative
          (if (string-equal current-name parent-name)
              current-name
            (file-relative-name current-name parent-name))))
    relative))

(defun ranger--header-new-tab ()
  (propertize " + "
              'face '(:inherit font-lock-builtin-face
                      :background "#787878"
                      :foreground "black")
              'pointer 'hand
              'local-map (let ((keymap (make-sparse-keymap)))
                           (define-key keymap [header-line down-mouse-1] 'ignore)
                           (define-key keymap [header-line mouse-1] #'ranger-new-tab)
                           keymap)))

(defun ranger--header-tabs ()
  (let* ((curr ranger-current-tab)
         (tabs (sort (r--akeys ranger-t-alist) '<)))
    (mapconcat
     (lambda (key)
       (let* ((item (r--aget ranger-t-alist key))
              (index (car (assoc key ranger-t-alist)))
              (roman (ranger--ar2ro key))
              (value (car-safe item))
              ret)
         (setq ret (cl-case ranger-tabs-style
                     ('normal (format " %s " value))
                     ('roman (format " %s " roman))
                     ('number (format " %s " key))))
         (if (equal key curr)
             (propertize ret
                         'face '(:inherit font-lock-builtin-face)
                                     :background "#323232")
           (propertize ret
                       'face 'font-lock-comment-face
                       'pointer 'hand
                       'local-map (eval `(ranger-make-header-keymap ,index))
                       ))))
     tabs "")))

(defun ranger-make-header-keymap (index)
  "Return a keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-1] `(lambda () (interactive) (ranger-goto-tab ,index)))
    (define-key keymap [header-line mouse-3] 'ranger-close-tab)
    keymap))

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
    (format "[%s]%s"

            ;; (if ranger-show-literal "raw" "act")
            ranger-parent-depth
            (cl-case ranger-show-hidden
              ('format "..")
              ('prefer "._")
              ('hidden "__")))
    'face 'font-lock-comment-face)
   (when (> (length ranger-t-alist) 1)
     (format " %s" (ranger--header-tabs)))
   (format " %s " (ranger--header-new-tab))))

(defun ranger--header-lhs ()
  "Setup header-line for ranger buffer."
  (let* ((current-file (or (r--fget ranger-current-file) ""))
         (file-path (file-name-directory current-file))
         (file-name (file-name-nondirectory current-file)))
    (format " %s : %s%s"
            (propertize
             (format "%s@%s" (user-login-name) (system-name))
             'face 'font-lock-keyword-face)
            file-path
            (propertize file-name 'face 'font-lock-constant-face))))

(defun ranger--header-string ()
  "Compose header string"
  (let* ((lhs (ranger--header-lhs))
         (rhs (ranger--header-rhs))
         (minimal (r--fget ranger-minimal))
         (used-length (+ (length rhs) (length lhs)))
         (fringe-gap (if (eq fringe-mode 0) 2 0))
         (total-window-width (+ 3 (if minimal
                                      (window-width)
                                    (- (frame-width) fringe-gap))))
         (filler (make-string (max 0 (- total-window-width used-length)) (string-to-char " "))))
    (concat lhs filler rhs)))

(defun ranger-parse-coords ()
  (interactive)
  (let* ((window (selected-window))
         (frame (window-frame))
         (coords (ranger-get-window-coords))
         (window-list (mapcar 'car coords))
         (width-list (mapcar 'cdr coords))
         (winidx (cl-position window window-list))
         (current-width (nth winidx width-list))
         (framew (frame-width frame))
         (left-margin (apply '+ (cl-subseq width-list 0 winidx))))
    ;; (ranger--message "Coord calculation - %s : %s : %s + %s / %s" window winidx left-margin current-width framew)
    (cons left-margin winidx)))

(defun ranger-get-window-coords ()
  "Get the coordinates for each ranger window to setup headerline."
  (interactive)
  (let (info)
    (if (r--fget ranger-minimal)
        (push (cons (selected-window) (window-width)) info)
      (walk-window-tree
       (lambda (window)
         (push
          (cons window (- (window-width window) 1))
          info)) nil nil))
    (nreverse info)))

(defun ranger-header-line ()
  "Setup header-line for ranger windows."
  (let* ((coords (ranger-parse-coords))
         (lm (car coords))
         (num (cdr coords)))
    (substring (ranger--header-string)
               ;; the left margin
               (+ lm 1
                  (* num (+
                          ;; account for graphical margin
                          (if window-divider-mode 2 3)
                          ;; account for scroll bar and fringe
                          (if (eq fringe-mode 0) -2 0)
                          (if scroll-bar-mode 3 0)))))))

(defun ranger-set-modeline ()
  "This is a redefinition of the fn from `dired.el'. This one
properly provides the modeline in dired mode. "
  (when (eq major-mode 'ranger-mode)
    (setq mode-name
          (concat
           ;; (if buffer-read-only "<N>" "<I>")
           (if (r--fget ranger-minimal)
               "Deer:"
             "Ranger:")
           (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                  "mtime")
                 ((string-match "^-[^c]*c[^c]*$" dired-actual-switches)
                  "ctime")
                 ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                  "ext")
                 ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                  "size")
                 ((string-match "^-[^SXUt]*$" dired-actual-switches)
                  "name")
                 (t
                  dired-actual-switches))
           (when (string-match "r" dired-actual-switches) " (r)")
           ))
    (with-eval-after-load "diminish"
      ;; (diminish 'ranger-mode)
      (diminish 'auto-revert-mode))
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
            (delete-region (progn (forward-line 0) (point))
                           (progn (forward-line 1) (point)))
          (forward-line 1))
        ;; check sorting mode and sort with directories first
        ;; (when (and ranger-listing-dir-first
        ;;            (not (string-match "[XStU]+" switches)))
        ;;   ;; [d]rwxrwxrwx should come before [-]rw-rw-rw-
        ;;   (sort-regexp-fields (not (string-match "r" switches))
        ;;                       "^.*\n"
        ;;                       "^[ ]*."
        ;;                       (point)
        ;;                       (point-max)))
        (set-buffer-modified-p nil)))))

;;;###autoload
(defun deer (&optional path)
  "Launch dired in a minimal ranger window."
  (interactive)
  (let* ((file (or path (buffer-file-name)))
         (dir (if file (file-name-directory file) default-directory)))
    (when dir
      (r--fset ranger-minimal t)
      (ranger-find-file dir))))

(defun deer-jump-other-window (&optional path)
  "Launch dired in a minimal ranger window in other window."
  (interactive)
  (let* ((win-num (length (window-list-1)))
         (buffer-dir (file-name-directory
                             (or buffer-file-name
                                 default-directory)))
         (next-buffer (and (> win-num 1)
                           (window-buffer (next-window)))))
    (switch-to-buffer-other-window next-buffer)
    (deer (or path buffer-dir))
    (cond
     ;; if window was added, delete
     ((not (eq win-num (length (window-list-1))))
      (message "adding delete window hook")
      (add-hook 'kill-buffer-hook #'delete-window t t))
     ;; else restore previous buffer
     (t
      (add-hook 'kill-buffer-hook
                `(lambda () (pop-to-buffer ,next-buffer)) t t)))))

(defun deer-dual-pane (&optional left right)
  "Launch dired in a minimal ranger window in other window."
  (interactive)
  (delete-other-windows)
  (deer left)
  (split-window-horizontally)
  (deer right))

;;;###autoload
(define-minor-mode ranger-override-dired-mode
  "Toggle ranger to override dired using `ranger-override-dired-fn'."
  :group 'ranger :global t
  (if ranger-override-dired-mode
      (add-hook 'dired-mode-hook 'ranger-override-dired-fn)
    (remove-hook 'dired-mode-hook 'ranger-override-dired-fn)))

(defun deer-from-dired ()
  (interactive)
  (ranger-override-dired-mode t)
  (deer))

(defun ranger-minimal-toggle ()
  (interactive)
  (let ((minimal (r--fget ranger-minimal))
        (dir default-directory))
    (ranger-revert)
    (if minimal
        (ranger dir)
      (deer dir))))

;;;###autoload
(defun ranger (&optional path)
  "Launch dired in ranger-mode."
  (interactive)
  (let* ((file (or path (buffer-file-name)))
         (dir (if file (file-name-directory file) default-directory)))
    (when dir
      (r--fset ranger-minimal nil)
      (ranger-find-file dir))))

(defun ranger-enable ()
  "Interactively enable ranger-mode."
  (interactive)
  (ranger-mode))

(defun ranger-close ()
  "Close tab or disable ranger"
  (interactive)
  (if (> (length ranger-t-alist) 1)
      (ranger-close-tab)
    (ranger-disable)))

(defun ranger-disable ()
  "Interactively disable ranger-mode."
  (interactive)
  ;; don't kill ranger buffer if open somewhere else
  (if (> (length (get-buffer-window-list)) 1)
      ((delete-window)
       (delete-window ranger-preview-window))
    (ranger-revert)))

(defun ranger-to-dired ()
  "toggle from ranger to dired in directory." ;
  (interactive)
  (let ((dir default-directory))
    (ranger-revert-appearance (current-buffer))
    (remove-hook 'dired-mode-hook 'ranger-override-dired-fn)
    (remove-hook 'window-configuration-change-hook 'ranger-window-check)
    ;; (setq-local ranger-wdired t)
    (dired-mode dir)
    ))

(defun ranger-setup ()
  "Setup all associated ranger windows."
  (interactive)

  ;; (setq eshell-ls-use-in-dired t)
  (setq ls-lisp-ignore-case nil)
  (setq ls-lisp-verbosity nil)
  (setq ls-lisp-uid-s-fmt "%-8s")
  (setq ls-lisp-gid-s-fmt "%-8s")
  ;; don't mix dotfiles with directories
  (setq ls-lisp-UCA-like-collation nil)
  ;; (setq ls-lisp-use-insert-directory-program nil)

  (setq ls-lisp-format-time-list
        '("%Y-%m-%d %H:%M"
          "%Y-%m-%d %H:%M"))

  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)

  (unless (derived-mode-p 'dired-mode)
    (error "Run it from dired buffer"))

  (ranger-define-additional-maps)

  ;; load bookmarks
  (require 'bookmark)
  (unless bookmark-alist
    (bookmark-maybe-load-default-file))

  ;; disable projectile caching
  (remove-hook 'dired-before-readin-hook #'projectile-cache-projects-find-file-hook t)

  (require 'dired-x)

  ;; store previous settings
  (unless ranger-pre-saved
    (setq ranger-pre-header-format header-line-format)
    (setq ranger-pre-hl-mode hl-line-mode)
    (setq ranger-pre-arev-mode auto-revert-mode)
    (setq ranger-pre-dired-listing dired-listing-switches)
    (setq ranger-pre-saved t))

  ;; save window-config for frame unless already
  ;; specified from running `ranger'
  (ranger-save-window-settings)

  ;; ranger specific objects
  (setq ranger-buffer (current-buffer))
  (setq ranger-window (get-buffer-window (current-buffer)))
  (setq ranger-frame (window-frame ranger-window))

  ;; update with current settings
  (r--aput ranger-w-alist
           (selected-window)
           (cons
            (car-safe (r--aget ranger-w-alist (selected-window)))
            (current-buffer)))

  (setq ranger-preview-window nil)

  ;; hide groups, show human readable file sizes
  (setq dired-listing-switches ranger-listing-switches)

  ;; truncate lines for primary window
  (ranger-truncate)

  ;; clear out everything if not in deer mode
  (add-to-list 'ranger-visited-buffers ranger-buffer)

  (ranger-sort t)
  (ranger--message "sorting")
  (ranger-show-flags)
  (ranger--message "setting flags")
  (ranger--message "starting filter")
  (ranger-filter-files)

  ;; omit files after buffer refresh
  (add-hook 'dired-after-readin-hook 'ranger-filter-files nil t)

  ;; open new tab if ranger is in multiple frames.
  (if (> (length ranger-f-alist) 1)
      (ranger-new-tab nil t)
    (ranger-update-tab ranger-current-tab))

  (ranger-setup-parents)
  (ranger-setup-preview)

  ;; scroll back to left in case new windows affected primary buffer
  (set-window-hscroll ranger-window 0)

  ;; reset subdir optiona
  (setq ranger-subdir-p nil)

  (when ranger-modify-header
    (setq header-line-format `(:eval (,ranger-header-func))))

  (ranger-show-file-details)
  (ranger-set-modeline)
  (ranger-hide-the-cursor)
  (run-hooks 'ranger-mode-load-hook)

  (if (r--fget ranger-minimal)
      (if ranger-deer-show-details
          (ranger-show-details)
        (ranger-hide-details))
    (progn
        (ranger-hide-details)))

  ;; hide details line at top - show symlink targets
  (funcall 'add-to-invisibility-spec 'dired-hide-details-information)
  (funcall 'add-to-invisibility-spec 'dired-hide-details-hide-information-lines)

  ;; (setq dired-hide-details-hide-symlink-targets nil)

  (ranger--message "Ranger loaded")
  )

(defun ranger-hide-the-cursor ()
  (when (and buffer-read-only ranger-hide-cursor)
    (setq-local cursor-type nil))
  (hl-line-mode t))

(defvar ranger--debug nil)
(defvar ranger--debug-period 1.5)

;; TODO make a ranger debug pane as the bottom window

(defun ranger--message (format &rest args)
  (when ranger--debug
    (let (old-mess (current-message))
      (setq format (concat "(ranger): " format))
      (apply 'message format args)
      (redisplay)
      (sleep-for ranger--debug-period)
      (when old-mess
        (message old-mess)))))

;;;###autoload
(when ranger-override-dired
  (ranger-override-dired-mode t))

;;;###autoload
(defun ranger-override-dired-fn ()
  "Open dired as deer unless already in ranger-mode"
  (let ((ranger-windows (r--akeys ranger-w-alist)))
    (unless (memq (selected-window) ranger-windows)
      (ranger--message "Override attempted")
      (if (eq ranger-override-dired 'ranger)
          (ranger)
        (deer)))))

;;; preserve this variable when switching from `dired-mode' to another mode
(put 'dired-subdir-alist 'permanent-local t)

;;;###autoload
(define-derived-mode ranger-mode dired-mode "Ranger"
  "Major mode emulating the ranger file manager in `dired'.

\\{ranger-mode-map}"
  :group 'ranger
  (setq-local cursor-type nil)
  ;; (setq-local ranger-mode nil)
  (use-local-map ranger-mode-map)
  ;; (advice-add 'dired-readin :after #'ranger-setup-dired-buffer)
  (ranger-setup)
  (add-hook 'window-configuration-change-hook 'ranger-window-check)
  (setq-local mouse-1-click-follows-link nil)
  )

(provide 'ranger)

;;; ranger.el ends here
