# ac-js2


An attempt at context sensitive auto-completion for Javascript in Emacs using
Js2-mode's parser and Skewer-mode. Inspired by the work of others on
projects such as Js2-mode, Skewer-mode and Js2-refactor I decided to
add to the concept of Emacs as a Javascript IDE.

Here is a screenshot of the auto completion in action.

![Completion](https://raw.github.com/ScottyB/ac-js2/master/images/function-interface.png)

## Features

 * Auto complete with js2-browser-externs
 * Auto complete with js2-keywords
 * Auto complete with js2-ecma-262-externs

Each of the above settings can be customized and are enabled by default.

 * Partial support for completing properties
 * Show function interface or initial value for object properties
 * Support for external libraries

Ac-js2 uses Skewer-mode to evaluate Javascript code in the browser
which could have undesired side effects. Therefore these features are
turned off by default, see Setup to enable these features anyway :).

 * Navigation commands to jump to variables, functions and object properties.

Jumping to values in an object literal isn't currently supported, it
is on my todo list though. Navigation works from anywhere along a dotted property reference.
i.e. Given the following proprety reference:

```
foo.bar.baz();
```

placing the cursor on `foo`, `bar` or `baz` and executing
`ac-js2-jump-to-definition` or `M-.` will take you straight to their respective
definitions. Executing `M-,` will jump you back to where you were.

 * Completion of variable and function names in current scope
 * Popup documentation for variables and function names in buffer

**Note:** Navigation only works if the definition is in the same file.

## Installation

Easiest way to get ac-js2 is to install it from MELPA. You may need this snippet

```
(add-to-list 'package-archives
              '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

if you don't have it already to fetch packages from MELPA.

### Dependencies

 * [skewer-mode](https://github.com/skeeto/skewer-mode) (MELPA)
 * [js2-mode](https://github.com/mooz/js2-mode) (ELPA)
 * [auto-complete](https://github.com/auto-complete/auto-complete) (ELPA)

Auto-complete mode is now activated by ac-js2 minor mode. All
dependencies will be installed by MELPA.

## Setup

Copy the snippet below if you want to evaluate your Javascript code
for candidates. Not setting this value will still provide you with basic completion.

```
(setq ac-js2-evaluate-calls t)
```

Add any external Javascript files to the variable below. Make sure you
have initialised `ac-js2-evaluate-calls` to t if you add any libraries.

```
(setq ac-js2-external-libraries '("full/path/to/a-library.js"))
```

## Usage

Call `run-skewer` and open up a Javascript file. Note that any code
you evaluate using Skewer may add completion candidates for the global
object.
