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

Run your test code associated of current buffere of current file.
```
M-x testkick
```

Run your test root directory of current project. 
```
M-x testkick-root
```

Run with specify file or directory
```
M-x testkick-at
```

## Installation

1. Copy testkick.el file to your load-path.
2. Add this one line to your init.el  

```cl
(require 'testkick)
```

Additional  

```cl
;; Settings for your shorthand.
(global-set-key "\C-," 'testkick)

;; Coloring on the results of shell command. Handle escape sequences correctly
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
```

## TODO
* More settings.
* Interactive interface
* Test case filter and run (rspec --line, vows -r, etc..)
* Caching
* anything interface

