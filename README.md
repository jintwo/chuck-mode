Emacs ChucK mode
================

### Installation

1. Clone this repo
2. Add to Emacs `load-path`

```lisp
(add-to-list 'load-path "/path/to/repo")

(require 'chuck-mode)
;;; or if you are using use-package
(use-package chuck-mode)
```

### Usage

#### ChucK edit mode keybindings

<kbd>C-c C-s</kbd> – start ChucK process

<kbd>C-c C-c</kbd> – add current buffer to ChucK

<kbd>C-c C-k</kbd> – kill ChucK process

<kbd>C-c C-r</kbd> – replace shred associated with current buffer (or use prefix to specify shred)

<kbd>C-c C-d</kbd> – kill shred associated with current buffer (or use prefix)

<kbd>C-c C-l</kbd> – kill all shreds

#### ChucK console keybindings

<kbd>x</kbd> – kill selected shred

<kbd>r</kbd> – reload selected shred (from associated buffer)

<kbd>R</kbd> – refresh console

<kbd>Q</kbd> – kill console and ChucK process
