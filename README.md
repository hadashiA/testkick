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

run your testcode of editing file.
```
M-x testkick
```

## Installation

1. copy testkick.el file to your load-path.
2. add this to your init.el 

```cl
(autoload 'testkick "testkick" nil t)
```

## TODO
* interactive interface
* some caching
* test case filter and run (rspec --line, vows -r, etc..)
* anything interface.

