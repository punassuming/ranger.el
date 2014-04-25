# Peep Dired

This is a minor mode that can be enabled from a dired buffer.
Once enabled it will show the file from point in the other window.
Moving to the other file within the dired buffer with <kbd>down</kbd>/<kbd>up</kbd> or
<kbd>C-n</kbd>/<kbd>C-p</kbd> will display different file.
Hitting <kbd>SPC</kbd> will scroll the peeped file down, whereas
<kbd>C-SPC</kbd> and <kbd>backspace</kbd> will scroll it up.

![Screenshot](https://github.com/asok/peep-dired/raw/master/screenshots/peep-dired-cast.gif)

# Installation

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Peep Dired. The package name is `peep-dired`.

```
(add-to-list 'load-path "/path/to/peep-dired/")
(require 'peep-dired)
```

# Evil integration
Adjust the state name depending on an evil state you open dired in:

```
(evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down)
(evil-define-key 'normal peep-dired-mode-map (kbd "C-<SPC>") 'peep-dired-scroll-page-up)
(evil-define-key 'normal peep-dired-mode-map (kbd "<backspace>") 'peep-dired-scroll-page-up)
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
(evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-mode-hook 'evil-normalize-keymaps)
```
