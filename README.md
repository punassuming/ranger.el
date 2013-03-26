# Instalation

Clone the repository somewhere on your hard drive. And add this to your emacs setup:

```
(add-to-list 'load-path "/path/to/peep-dired/")
(require 'peep-dired)
```

# Usage

Run `peep-dired` from a dired buffer. That will open file at point in the other window.
`C-n` and `<down>` will show the next file in the other window.
`C-p` and `<up>` will show the previous file in the other window.
`<SPC>` will scroll the page down and `C-<SPC>` will scroll the page up.

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
