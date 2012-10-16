# testkick.el

testkick.el is Emacs Lisp provides a command.
Find the test code associated with the current buffer, and run it.

Supported test frameworks.
* Ruby
  * RSpec
* node.js
  * vows
  * mocha

## Usage

Run your testcode of current file.
```
M-x testkick
```

## Installation

1. Copy testkick.el file to your load-path.
2. Add this to your init.el 

```cl
(autoload 'testkick "testkick" nil t)
```

* Additional

```cl
;; Settings for your shorthand.
(global-set-key "\C-," 'testkick)

;; Coloring on the results of shell command. Handle escape sequences correctly
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
```

## TODO
* Interactive interface
* Test case filter and run (rspec --line, vows -r, etc..)
* Caching
* anything interface

