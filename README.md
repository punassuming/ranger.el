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

## Evil integration
Adjust the state name depending on an evil state you open dired in:

```
(evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down)
(evil-define-key 'normal peep-dired-mode-map (kbd "C-<SPC>") 'peep-dired-scroll-page-up)
(evil-define-key 'normal peep-dired-mode-map (kbd "<backspace>") 'peep-dired-scroll-page-up)
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
(evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
```

## Note about created buffers

For now the buffers opened when browsing a directory will not be killed until disabling the mode. If you want kill them manually you can run command `peep-dired-kill-buffers-without-window`.

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
