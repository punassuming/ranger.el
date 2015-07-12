<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Evil Ranger](#evil-ranger)
    - [Description](#description)
    - [Installation](#installation)
        - [Screenshot](#screenshot)
        - [Screencast](#screencast)
    - [Todo](#todo)
    - [Key bindings](#key-bindings)
    - [Configuration](#configuration)
        - [Customizing](#customizing)
        - [Ignoring Certain File Extensions During Preview](#ignoring-certain-file-extensions-during-preview)

<!-- markdown-toc end -->
# Evil Ranger


## Description

This is a minor mode that runs within dired emulating many of the features of
ranger. This minor mode takes some of the ideas from Peep-Dired to display
previews for selected files in the primary dired buffer.


## Installation

<!-- Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can -->
<!-- use `package-install` command to install Evil Ranger. The package name is `evil-ranger`. -->

### Screenshot

evil-ranger mode active
![Ranger Mode Active](screenshots/evil-ranger.png)

evil-ranger mode with preview enabled
![Ranger Mode with Preview](screenshots/evil-ranger-preview.png)

### Screencast

evil-ranger screencast
![Ranger Mode Screencast](screenshots/hello-ranger.gif)


## Todo

* Create bindings to go up / down the next directory
* Log evil-ranger history and allow prompt to navigate back
* Add ranger style copy and pasted
* Improve headerline display
* Set up tabs and navigation between
* Improve sorting

## Key bindings

| Keybindings | Description                                 |
| ----------- | --------------------------------------      |
| `C-p`       | toggle evil-ranger in dired buffer          |
| `j`         | navigate down                               |
| `k`         | navigate up                                 |
| `C-j`       | scroll preview window down                  |
| `C-k`       | scroll preview window up                    |
| `f`         | search for file names                       |
| `i`         | show preview of current file                |
| `zi`        | toggle showing literal / full-text previews |
| `zh`        | toggle showing dotfiles                     |
| `o`         | sort options                                |
| `h`         | go up directory                             |
| `l` / `RET` | find file / enter directory                 |
| `q`         | quit                                        |
| `r`         | revert buffer                               |
| `z-`        | reduce number of parents                    |
| `z+`        | increment number of parents                 |
| `v`         | toggle all marks                            |
| `V`         | visually select lines                       |
| `S`         | enter shell                                 |
| `C-SPC`     | mark current file                           |

## Configuration

Most parameters can be toggled on and off and stay within the current emacs
session. Any settings that are desired on startup should be set below. 

### Customizing

When disabling the mode you can choose to kill the buffers that were opened while browsing the directories.
```el
(setq evil-ranger-cleanup-on-disable t)
```

Or you can choose to kill the buffer just after you move to another entry in the dired buffer.
```el
(setq evil-ranger-cleanup-eagerly t)
```

If you want the dired buffers that were peeped to have the mode enabled set it to true.
```el
(setq evil-ranger-enable-on-directories t)
```

You can set the number of folders to nest to the left
```el
(setq evil-ranger-parent-depth 2)
```

You can set the size of the preview and parent windows as a fraction of the
frame size
```el
(setq evil-ranger-width-parents 0.12)
(setq evil-ranger-width-preview 0.55)
```

You can set the size of the preview and parent windows as a fraction of the
frame size
```el
(setq evil-ranger-width-preview 0.55)
```

### Ignoring Certain File Extensions During Preview

You probably don't want to open certain files like videos when using Peep Dired. To ignore certain files when moving over them you can customize the following to your liking:

```el
(setq evil-ranger-ignored-extensions '("mkv" "iso" "mp4"))el
```

To set the max files size, set the following parameter:

```el
(setq evil-ranger-max-preview-size 10)
```
