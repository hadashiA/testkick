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

3. Settings for your shorthand.

```cl
(global-set-key "\C-," 'testkick)
```

## TODO
* Run test root directory
* Interactive interface
* Test case filter and run (rspec --line, vows -r, etc..)
* Caching
* anything interface

