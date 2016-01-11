# Ranger
[![MELPA](http://melpa.org/packages/ranger-badge.svg)](http://melpa.org/#/ranger)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Ranger](#ranger)
    - [Description](#description)
    - [Installation](#installation)
        - [Screenshot](#screenshot)
        - [Screencast](#screencast)
    - [Features](#features)
    - [Todo](#todo)
    - [Coming from dired](#coming-from-dired)
    - [Minimal Ranger Mode (deer)](#minimal-ranger-mode-deer)
    - [Key bindings](#key-bindings)
    - [Configuration](#configuration)
        - [Customizing](#customizing)
        - [Parent options](#parent-options)
        - [Preview options](#preview-options)

<!-- markdown-toc end -->

## Description

This is a minor mode that runs within dired emulating many of the features of
[ranger](http://ranger.nongnu.org/). This minor mode shows a stack of the parent
directories and updates the parent buffers while navigating the file system. The
preview window takes some of the ideas from [Peep-Dired](https://github.com/asok/peep-dired)
to display previews for selected files in the primary dired buffer.

## Installation

Once you have setup [Melpa](https://melpa.org/#/getting-started) you can
use `package-install` command to install Ranger. The package name is `ranger`.

### Screenshot

ranger mode active
![Ranger Mode Active](screenshots/ranger.png)

deer mode (minimal ranger mode)
![Deer Mode](screenshots/deer.png)

ranger mode with preview enabled
![Ranger Mode with Preview](screenshots/ranger-preview.png)

ranger mode showing image
![Ranger Mode with Image](screenshots/ranger-image.png)

### Screencast

ranger screencast
![Ranger Mode Screencast](screenshots/hello-ranger.gif)

## Features

* Preview of selected file / directory
* Multi-column display of parent directories
* History log and prompt to navigate through history
* Toggle between literal file viewing and actual
* Indirectly scroll through previewed files
* Show images inline in preview window
* Fit images to window when previewing (requires image-dired)
* Quick and persistent sorting across folders
* Quick access to shell
* Mouse support
* Emacs bookmarks support
* Create bindings to go up / down the next directory
* Minimal ranger mode (deer-mode)
* Copy / paste functionality
* Persistent flags showing recently copied items

## Todo

* Preview PDFs
* Better showing of archive files
* Set up tabs and navigation between
* Work with flattened subdirs and tree

## Coming from dired

Ranger works as a replacement to `dired`, but there are a couple of things you have to know:
* If you don't like seen windows with the parent folders, you can user the minimal `ranger` mode called `deer` (see next section).
* When you open a file, the `ranger` session gets finalized. If you want to restore it after closing the file, just try opening `ranger` again (it will remember exactly the path where you were before).
* If you want to execute a shell command just for the marked files, instead of all the files in the current directory, you have to use `dired-do-shell-command` (in `spacemacs` it is bound to `;!`).

## Minimal Ranger Mode (deer)

Termed as `deer-mode`, based on the zsh module developed by Vifon, we can use
ranger in a single window without preview or parent directories.  This allows
all the functionality builtin to ranger without modifying any other buffer
windows.  Toggle between `ranger` and `deer` with `zp`.

## Copy / Paste

Ranger utilizes a copy ring to save a persistent fileset that can then be
either moved or copied to a target directory.  This is consistent with the
way the ranger file manager manages file movement. `yy` specifies to copy the
marked or currently hovered files.  Additionally a flag denoted as "P" will
visually indicate what files are marked for movement. `dd` likewise specifies
that the files will be moved instead of copied.

To paste the files most recently added to the copy ring, use `pp` or `po` to
paste.  The latter binding will overwrite existing files.

## Key bindings

 Keybinding    | Description
 ------------- | -----------
 `?`           | show ranger help
 `!`           | run shell command
 `j`           | navigate down
 `k`           | navigate up
 `J`           | next subdir
 `K`           | previous subdir
 `G`           | goto last file
 `gg`          | goto first file
 `gh`          | goto home directory
 `B`           | show bookmark prompt
 `D`           | delete the selected file
 `R`           | rename/move the selected file
 ````          | goto bookmark
 `m`           | set bookmark
 `[`           | previous parent directory
 `]`           | next parent directory
 `f`           | search for file names
 `zz`          | search through history
 `H`           | history back
 `L`           | history next
 `zi`          | toggle showing literal / full-text previews
 `zh`          | toggle showing dotfiles
 `zf`          | toggle showing image full-size or fitted to window
 `ws`          | exit ranger and open selected file in vertical split
 `wv`          | exit ranger and open selected file in horizontal split
 `wf`          | exit ranger and open selected file in new frame
 `we`          | open the selected file in external app
 `o`           | sort options
 `h`           | go up directory
 `l`           | find file / enter directory
 `RET`         | find file / enter directory
 `q`           | quit
 `^R`          | revert buffer
 `z-`          | reduce number of parents
 `z+`          | increment number of parents
 `v`           | toggle all marks
 `V`           | visually select lines
 `u`           | unmark the selected file
 `yy`          | mark files to copy
 `dd`          | mark files to move
 `pp`          | paste files in copy ring
 `po`          | paste files in copy ring and overwrite existing
 `p?`          | show the copy contents
 `S`           | enter shell
 `C-SPC`/`TAB` | mark current file
 `C-r`         | refresh
 `i`           | show preview of current file
 `C-j`         | scroll preview window down
 `C-k`         | scroll preview window up
 `zp`          | toggle between full ranger and deer-mode
 `gn`          | create a new tab
 `gT`          | go to previous tab
 `gt`          | go to next tab
 `gc`          | close current tab

## Configuration

Most parameters can be toggled on and off and stay within the current emacs
session. Any settings that are desired on startup should be set below.

### Customizing

When disabling the mode you can choose to kill the buffers that were opened while browsing the directories.
```el
(setq ranger-cleanup-on-disable t)
```

Or you can choose to kill the buffer just after you move to another entry in the dired buffer.
```el
(setq ranger-cleanup-eagerly t)
```

You can choose to show dotfiles at ranger startup, toggled by `zh`.
```el
(setq ranger-show-dotfiles t)
```

Define custom function used to output header of primary ranger window. Must
return a string that is placed in the header-line.
```el
(setq ranger-header-func 'ranger-header-line)
```

Define custom function used to output header of parent and preview windows. Must
return a string that is placed in the header-line.
```el
(setq ranger-parent-header-func 'ranger-parent-header-line)
```

### Parent options

You can set the number of folders to nest to the left, adjusted by `z-` and `z+`.
```el
(setq ranger-parent-depth 2)
```

You can set the size of the parent windows as a fraction of the frame size.
```el
(setq ranger-width-parents 0.12)
```

When increasing number of nested parent folders, set max width as fraction of
frame size to prevent filling up entire frame with parents.
```el
(setq ranger-max-parent-width 0.12)
```

### Preview options

You can choose to show previews literally, or through find-file, toggled by `zi`.
```el
(setq ranger-show-literal t)
```

You can set the size of the preview windows as a fraction of the frame size.
```el
(setq ranger-width-preview 0.55)
```

You probably don't want to open certain files like videos when previewing. To ignore certain files when moving over them you can customize the following to your liking:

```el
(setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
```

To set the max files size (in MB), set the following parameter:

```el
(setq ranger-max-preview-size 10)
```
