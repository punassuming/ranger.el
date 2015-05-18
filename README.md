# Peep Dired

This is a minor mode that can be enabled from a dired buffer.
Once enabled it will show the file from point in the other window.
Moving to the other file within the dired buffer with <kbd>down</kbd>/<kbd>up</kbd> or
<kbd>C-n</kbd>/<kbd>C-p</kbd> will display different file.
Hitting <kbd>SPC</kbd> will scroll the peeped file down, whereas
<kbd>C-SPC</kbd> and <kbd>backspace</kbd> will scroll it up.

![Screenshot](https://github.com/asok/peep-dired/raw/master/screenshots/peep-dired-cast.gif)

## Installation

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Peep Dired. The package name is `peep-dired`.

## Configuration

### Customizing

When disabling the mode you can choose to kill the buffers that were opened while browsing the directories.
```el
(setq peep-dired-cleanup-on-disable t)
```

Or you can choose to kill the buffer just after you move to another entry in the dired buffer.
```el
(setq peep-dired-cleanup-eagerly t)
```

If you want the dired buffers that were peeped to have the mode enabled set it to true.
```el
(setq peep-dired-enable-on-directories t)
```

### Evil integration
Adjust the state name depending on an evil state you open dired in:

```
(evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
                                             (kbd "C-<SPC>") 'peep-dired-scroll-page-up
                                             (kbd "<backspace>") 'peep-dired-scroll-page-up
                                             (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
```

## Ignoring Certain File Extensions

You probably don't want to open certain files like videos when using Peep Dired. To ignore certain files when moving over them you can customize the following to your liking:

```
(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
```

## Contribution

Install [cask](https://github.com/rejeep/cask.el) if you haven't already, then:

```bash
$ cd /path/to/peep-dired
$ cask
```

Run all tests with:

```bash
$ make test
```
