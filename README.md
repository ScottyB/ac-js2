ac-js2
======

An attempt at context sensitive auto-completion in Emacs using Js2-mode's parser and Skewer-mode. Inspired by the work of others on projects such as Js2-mode, Skewer-mode and Js2-refactor I decided to add to the concept of Emacs as a Javascript IDE.

Currently a work in progress.
-----------------------------

Dependencies:
-------------

Best to install all of these using the package manager that comes with Emacs 24.

 * [Skewer-mode](https://github.com/ScottyB/skewer-mode) Make sure you use my modified version as I have made some minor changes.
 * [Js2-mode](https://github.com/mooz/js2-mode)
 * [Autocomplete](https://github.com/auto-complete/auto-complete)

Installation
------------

* Make sure all dependencies are on the load path
* Add `(require 'ac-js2)` to your init.el file
* Start Skewer with `run-skewer'
* Open up a Javascript file and test out the auto completion

Features
--------

 * Auto complete with js2-browser-externs
 * Auto complete with js2-keywords
 * Auto complete with js2-ecma-262-externs
 * Variable and function names in current scope
 * Popup documentation for variables and function names in scope (jsdoc and block)
 * Partial support for completing properties

![Completion](https://raw.github.com/ScottyB/ac-js2/master/images/completion.png)

TODO
----
 * Improve handling of errors from Skewer
 * Add Support for external libraries
 * Possibly support multiple files
 * Improve completion of properties
 * Show function interface in document popup
 * Add tests
 * Add to package manager
