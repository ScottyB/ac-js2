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

![Completion](https://raw.github.com/ScottyB/ac-js2/master/images/function-interface.png)

* Function interface or initial value for object properties

![library](https://raw.github.com/ScottyB/ac-js2/master/images/external-library.png)

 * Experimental support for external libraries

See TODO.org for planned features.

## Installation

Intergration with a package manager is planned soon.

### Dependencies:

 * [skewer-mode](https://github.com/skeeto/skewer-mode) (MELPA)
 * [js2-mode](https://github.com/mooz/js2-mode) (ELPA)
 * [auto-complete](https://github.com/auto-complete/auto-complete) (ELPA)

```
(add-to-list 'load-path "/path/to/skewer-mode")
(require 'skewer-mode)
```

```
(add-to-list 'load-path "/path/to/ac-js2")
(require 'ac-js2)
```

Open up test-script.js and a browser window will open. Jump back to Emacs to test out the auto completion.

Thank you for testing out ac-js2. Feedback welcome.
