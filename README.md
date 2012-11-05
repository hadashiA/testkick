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

Run your test code associated of current buffer.
```
M-x testkick
```

Run your test root directory of current project. 
```
M-x testkick-root
```

Find file  test-file/source-file toggle.
```
M-x testkick-toggle
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
(global-set-key (kbd "C-`") 'testkick)
(global-set-key (kbd "C-+") 'testkick-toggle)

;; Coloring on the results of shell command. Handle escape sequences correctly
(autoload 'ansi-color-apply-on-region "ansi-color"
  "Set `ansi-color-apply-on-region' to t." t)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (ansi-color-apply-on-region (point-min) (point-max))))
```

## TODO
* More settings.
* Test case filter and run (rspec --line, vows -r, etc..)
* anything interface

