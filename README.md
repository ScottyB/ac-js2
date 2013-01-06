# ac-js2


An attempt at context sensitive auto-completion in Emacs using Js2-mode's parser and Skewer-mode. Inspired by the work of others on projects such as Js2-mode, Skewer-mode and Js2-refactor I decided to add to the concept of Emacs as a Javascript IDE.

### Currently a work in progress

## Features

 * Auto complete with js2-browser-externs
 * Auto complete with js2-keywords
 * Auto complete with js2-ecma-262-externs
 * Variable and function names in current scope
 * Popup documentation for variables and function name in buffer (jsdoc and block)
 * Partial support for completing properties
 * Function interface or initial value for object properties
 * Experimental support for external libraries

![Completion](https://raw.github.com/ScottyB/ac-js2/master/images/function-interface.png)

See TODO.org for planned features.

## Installation

### Dependencies:

 * [skewer-mode](https://github.com/ScottyB/skewer-mode) My modified version.
 * [js2-mode](https://github.com/mooz/js2-mode) (ELPA)
 * [auto-complete](https://github.com/auto-complete/auto-complete) (ELPA)

For the moment, make sure you grab my modified version of skewer-mode the other dependencies can be installed using the package manager:

```
(add-to-list 'load-path "/path/to/skewer-mode")
(require 'skewer-mode)
```

```
(add-to-list 'load-path "/path/to/ac-js2")
(require 'ac-js2)
```

Open up test-script.js and the browser will open hopefully welcoming you to Skewer. Jump back to Emacs to test out the auto completion. Auto-completion does work on other files but throws errors on partially entered commands.

Thank you for testing out ac-js2. Feedback welcome.
