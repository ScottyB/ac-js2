# ac-js2

An attempt at context sensitive auto-completion for Javascript in Emacs using
js2-mode's parser and Skewer-mode (requires Emacs 24.3). Inspired by the work of others on
projects such as js2-mode, skewer-mode and js2-refactor I decided to
add to the concept of Emacs as a Javascript IDE.

Here is a screenshot of the auto completion in action using the
optional dependency, auto-complete-mode.

![Completion](https://raw.github.com/ScottyB/ac-js2/master/images/function-interface.png)

## Features

 * Auto complete with js2-browser-externs
 * Auto complete with js2-keywords
 * Auto complete with js2-ecma-262-externs

Each of the above settings can be customized and are enabled by default.

 * Partial support for completing properties
 * Show function interface or initial value for object properties
 * Support for external libraries

Ac-js2 uses skewer-mode to evaluate Javascript code in the browser
which could have undesired side effects. Therefore these features are
turned off by default, see Setup to enable these features anyway :).

 * Navigation commands to jump to variables, functions and object properties.

i.e. Given the following proprety reference:

```
foo.bar.baz();
```

placing the cursor on `foo`, `bar` or `baz` and executing
`ac-js2-jump-to-definition` or `M-.` will take you straight to their respective
definitions. Executing `M-,` will jump you back to where you were.

 * Completion of variable and function names in current scope
 * Popup documentation for variables and function names in buffer

**Experimental yansippet integration**

Recently added `ac-js2-expand-function` that will expand a function's
parameters bound to `C-c C-c`. Expansion will only work if the cursor
is after the function.

**Note:** Navigation only works if the definition is in the same file.

## Installation

Easiest way to get ac-js2 is to install it from MELPA. You may need this snippet

```
(add-to-list 'package-archives
              '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

if you don't have it already to fetch packages from MELPA.

Set up `ac-js2` for `auto-complete` like this:

```
(add-hook 'js2-mode-hook 'ac-js2-mode)
```

Set up `ac-js2` for `company` like this:

```
(add-to-list 'company-backends 'ac-js2-company)
```

### Dependencies

 * [skewer-mode](https://github.com/skeeto/skewer-mode) (MELPA)
 * [js2-mode](https://github.com/mooz/js2-mode) (ELPA/MELPA)

Optional, depending on which completion framework you want to use:

 * [auto-complete](https://github.com/auto-complete/auto-complete) (ELPA/MELPA)
 * [company](https://github.com/company-mode/company-mode) (ELPA/MELPA)

## Setup

Copy the snippet below if you want to evaluate your Javascript code
for candidates. Not setting this value will still provide you with
basic completion.

```
(setq ac-js2-evaluate-calls t)
```

Add any external Javascript files to the variable below. Make sure you
have initialised `ac-js2-evaluate-calls` to t if you add any libraries.

```
(setq ac-js2-external-libraries '("full/path/to/a-library.js"))
```

## Usage

Call `run-skewer` and open up a Javascript file. Type part of a
variable and call `completion-at-point` or if you have auto-complete
installed wait for the popup.

Note that any code you evaluate using Skewer may add completion
candidates for the global object.
