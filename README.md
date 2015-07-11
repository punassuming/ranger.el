# Evil Ranger

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
![Ranger Mode Active](screenshots/evil-ranger-preview.png) 

## Todo

* Allow click of parent directories to jump primary buffer to that directory
* Create bindings to increment number of parent nests
* Create bindings to go up / down the next directory

## Configuration

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

## Ignoring Certain File Extensions During Preview

You probably don't want to open certain files like videos when using Peep Dired. To ignore certain files when moving over them you can customize the following to your liking:

```el
(setq evil-ranger-ignored-extensions '("mkv" "iso" "mp4"))el
```

To set the max files size, set the following parameter:

```el
(setq evil-ranger-max-preview-size 10)
```


