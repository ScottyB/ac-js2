;;; ac-js2.el --- Autocomplete source for Js2-mode

;; Copyright (C) 2013  Scott Barnett

;; Author: Scott Barnett <scott.n.barnett@gmail.com>
;; URL: https://github.com/ScottyB/ac-js2
;; Version: 1.0
;; Package-Requires: ((js2-mode "20090723") (auto-complete "1.4") (skewer-mode "1.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; An attempt to get context sensitive completion in Emacs.
;;

;;; Code:

(require 'js2-mode)
(require 'auto-complete)
(require 'skewer-mode)
(require 'cl)

(defgroup ac-js2 nil
  "Auto-completion for js2-mode."
  :group 'completion
  :prefix "js2ac-")

;;; Configuration variables

(defcustom js2ac-add-ecma-262-externs t
  "If non-nil add `js2-ecma-262-externs' to completion candidates.")

(defcustom js2ac-add-browser-externs t
  "If non-nil add `js2-browser-externs' to completion candidates.")

(defcustom js2ac-add-keywords t
  "If non-nil add `js2-keywords' to completion candidates.")

(defcustom js2ac-add-prototype-completions t
  "When non-nil traverse the prototype chain adding to completion candidates.")

(defcustom js2ac-external-libraries '()
    "List of absolute paths to external Javascript libraries.")

(defcustom js2ac-evaluate-calls nil
  "Warning. When true function calls will be evaluated in the browser.
This may cause undesired side effects however it will
  provide better completions. Use at your own risk.")

;;; Internal variables

(defvar js2ac-keywords '()
  "Cached string version of `js2-keywords'.")

(defvar js2ac-candidates '())

;; Types of skewer completion methods available
(defconst js2ac-method-eval 0)
(defconst js2ac-method-global 1
  "Return candidates for the global object.
Only keys of the object are returned as the other properties come
  from js2-mode's externs.")

(defvar skewer-hide-comments nil)

(defvar js2ac-data-root (file-name-directory load-file-name)
    "Location of data files needed for `js2ac-on-skewer-load'.")

;;; Skewer integration

(defvar js2ac-skewer-candidates '()
  "Cadidates obtained from skewering.")

(defun js2ac-on-skewer-load ()
  "Add skewer-addon.js to skewer for evaluation."
  (insert-file-contents (expand-file-name "skewer-addon.js" js2ac-data-root))
  (and js2ac-evaluate-calls
       (mapcar (lambda (library)
              (with-temp-buffer
                  (insert-file-contents (expand-file-name library))
                  (js2-mode)
                  (skewer-eval (buffer-substring-no-properties (point-min) (point-max))
                               #'js2ac-skewer-result-callback
                               :type "complete"))) js2ac-external-libraries)))

;;;###autoload
(add-hook 'skewer-js-hook 'js2ac-on-skewer-load)

(defun js2ac-skewer-completion-candidates ()
  "Return completions returned from skewer."
  (mapcar (lambda (candidate) (symbol-name (car candidate))) js2ac-skewer-candidates))

(defun js2ac-skewer-document-candidates (name)
  "Return document string for NAME from skewer."
  (let ((doc (cdr (assoc-string name js2ac-skewer-candidates))))
    (or (js2ac-format-function doc) doc)))

(defun js2ac-get-object-properties (name)
  "Find properties of NAME for completion."
  (js2ac-skewer-eval-wrapper name `((prototypes . ,js2ac-add-prototype-completions))))

(defun js2ac-skewer-eval-wrapper (str &optional extras)
    "Wrap `skewer-eval' to check if a skewer-client is avilable.
STR is the text to send to the browser for evaluation. Extra
parameters can be passed to the browser using EXTRAS. EXTRAS must
be of the form (param-string . value) where param-string is the
reference and value is the value that can be retrieved from the
request object in Javacript."
  (if skewer-clients
      (if (or js2ac-evaluate-calls
            (not (js2ac-has-funtion-calls str)))
        (skewer-eval str #'js2ac-skewer-result-callback
                     :type "complete"
                     :extra extras)
        (setq js2ac-skewer-candidates nil))
      (setq skewer-queue nil)
      (setq js2ac-skewer-candidates nil)))

(defun js2ac-skewer-result-callback (result)
  "Callback with RESULT passed from the browser."
  (let ((value (cdr (assoc 'value result))))
    (if (and (skewer-success-p result) value)
        (setq js2ac-skewer-candidates (append value nil))
        (setq js2ac-skewer-candidates nil))))

;; Auto-complete settings

(defun js2ac-ac-candidates()
  "Main function called to gather candidates for Auto-complete."
  (let ((node (js2-node-parent (js2-node-at-point (1- (point)))))
        beg
        (prop-get-regex "[a-zA-Z)]\\.")
        name)
    (setq js2ac-candidates nil)
    (cond
     ((looking-back "\\.")
      ;; TODO: Need to come up with a better way to extract object than this regex!!
      (save-excursion
        (setq beg (and (skip-chars-backward "[a-zA-Z_$][0-9a-zA-Z_$#\"())]+\\.") (point))))
      (setq name (buffer-substring-no-properties beg (1- (point))))
      (js2ac-get-object-properties name)
      (setq node (js2ac-initialized-node (if (string-match prop-get-regex name)
                                                 (reverse (split-string name prop-get-regex)) name)))
      (if (js2-object-node-p node)
          (setq js2ac-candidates
                (mapcar (lambda (elem)
                          (js2ac-format-node (js2-node-string (js2-object-prop-node-left elem))
                                             elem))
                        (js2-object-node-elems node))))
      (append (mapcar 'first js2ac-candidates)
              (js2ac-skewer-completion-candidates)))
     ((js2-prop-get-node-p node)
      (setq node (js2-prop-get-node-left node))
      (setq name (js2-node-string node))
      (js2ac-get-object-properties name)
      (js2ac-skewer-completion-candidates))
     (t
      (js2ac-skewer-eval-wrapper "" `((method . ,js2ac-method-global)))
      (append (js2ac-skewer-completion-candidates)
              (js2ac-add-extra-completions
               (mapcar 'first (js2ac-get-names-in-scope))))))))

(defun js2ac-ac-document(name)
  "Show documentation for NAME from local buffer if present
otherwise check skewer documentation."
  (let* ((docs (cdr (assoc name js2ac-candidates)))
         (doc (if (listp docs) (first docs) docs)))
    (if doc doc (js2ac-skewer-document-candidates name))))

(defun js2ac-ac-prefix()
  (or (ac-prefix-default) (ac-prefix-c-dot)))

;;;###autoload
(defun js2ac-setup-completion ()
    "Called by `js2-mode-hook' to setup buffer for completion.
Setup `before-save-hook', set `ac-sources' variable and evaluate buffer
if `js2ac-evaluate-calls' is true."
  (when (string= major-mode "js2-mode")
    (if (not (member 'js2ac-setup-completion 'before-save-hook))
            (add-hook 'before-save-hook 'js2ac-setup-completion))
    (unless (member 'ac-source-js2 'ac-sources)
        (add-to-list 'ac-sources 'ac-source-js2))
    (and js2ac-evaluate-calls (js2ac-skewer-eval-wrapper (buffer-substring-no-properties (point-min) (point-max)))))
  t)

;;;###autoload
(add-hook 'js2-mode-hook 'js2ac-setup-completion)

(ac-define-source "js2"
  '((candidates . js2ac-ac-candidates)
    (document . js2ac-ac-document)
    (prefix .  js2ac-ac-prefix)
    (requires . -1)))

;;; Helper functions

(defun js2ac-build-prop-name-list (prop-node)
  "Build a list of names from a PROP-NODE."
  (let* (names
         left
         left-node)
    (unless (js2-prop-get-node-p prop-node)
      (error "Node is not a property prop-node"))
    (while (js2-prop-get-node-p prop-node)
      (push (js2-name-node-name (js2-prop-get-node-right prop-node)) names)
      (setq left-node (js2-prop-get-node-left prop-node))
      (when (js2-name-node-p left-node)
        (setq left (js2-name-node-name left-node)))
      (setq prop-node (js2-node-parent prop-node)))
    (append names `(,left))))

(defun js2ac-prop-names-left (name-node)
  "Create a list of all of the names in the property NAME-NODE.
NAME-NODE must have a js2-prop-get-node as parent. Only adds
properties to the left of point. This is so individual jump
points can be found for each property in the chain."
  (let* (name
         (parent (js2-node-parent name-node))
         left
         names)
    (unless (or (js2-prop-get-node-p parent) (js2-name-node-p name-node))
      (error "Not a name node or doesn't have a prop-get-node as parent"))
    (setq name (js2-name-node-name name-node)
          left (js2-prop-get-node-left parent))
    (if (and (js2-name-node-p left)
             (string= name (js2-name-node-name left)))
        (setq names name)
      (js2-visit-ast
       parent
       (lambda (node endp)
         (unless endp
           (if (js2-name-node-p node)
               (push (js2-name-node-name node) names)
               t))))
      names)))

(defun js2ac-has-funtion-calls (string)
    "Check if the Javascript code in STRING has a Js2-call-node."
  (with-temp-buffer
    (insert string)
    (let* ((ast (js2-parse)))
      (catch 'call-node
        (js2-visit-ast-root
         ast
         (lambda (node end-p)
           (unless end-p
             (if (js2-call-node-p node)
                 (throw 'call-node t)
               t))))))))

(defun js2ac-add-extra-completions (completions)
  "Add extra candidates to COMPLETIONS."
  (append completions
          (if js2ac-add-keywords (or js2ac-keywords (setq js2ac-keywords (mapcar 'symbol-name js2-keywords))))
          (if js2ac-add-ecma-262-externs js2-ecma-262-externs)
          (if js2ac-add-browser-externs js2-browser-externs)))

(defun js2ac-root-or-node ()
  "Return the current node or js2-ast-root node."
  (let ((node (js2-node-at-point)))
    (if (js2-ast-root-p node)
        node
      (js2-node-get-enclosing-scope node))))

(defun js2ac-get-names-in-scope ()
  "Fetches all symbols in scope and formats them for completion."
  (let* ((scope (js2ac-root-or-node))
         result)
    (while scope
      (setq result (append result
                           (loop for item in (js2-scope-symbol-table scope)
                                 if (not (assoc (car item) result))
                                 collect item)))
      (setq scope (js2-scope-parent-scope scope)))
    (setq js2ac-candidates
          (mapcar (lambda (x)
                    (let* ((name (symbol-name (car x)))
                           (init (js2ac-initialized-node name)))
                      (js2ac-format-node name init)))
                  result))))

(defun js2ac-initialized-node (name)
  "Return initial value assigned to NAME.
NAME may be either a variable, a function or a variable that
holds a function. Returns nil if no initial value can be found."
  (let* ((node (if (listp name) (js2ac-find-property name)
                 (js2ac-name-declaration name)))
         (parent (if node (js2-node-parent node)))
         (init (cond
                ((js2-function-node-p parent)
                 parent)
                ((js2-function-node-p node)
                 node)
                ((js2-var-init-node-p parent)
                 (js2-var-init-node-initializer parent))
                ((js2-assign-node-p parent)
                 (js2-assign-node-right parent))
                (t
                 nil))))
    init))

(defun js2ac-name-declaration (name)
  "Return the declaration node for node named NAME."
  (let* ((node (js2ac-root-or-node))
         (scope-def (js2-get-defining-scope node name))
         (scope (if scope-def (js2-scope-get-symbol scope-def name) nil))
         (symbol (if scope (js2-symbol-ast-node scope) nil)))
    (if (not symbol)
        (js2ac-get-function-node name scope-def)
      symbol)))

;;; Completion candidate formating

(defun js2ac-format-node (name node)
  "Format NAME and NODE for completion.
Returned format is a list where the first element is the NAME of
the node (shown in completion candidate list) and the last
element is the text to show as documentation."
  (let ((node (if (js2-object-prop-node-p node) (js2-object-prop-node-right node) node))
        (doc (if (and (js2-function-node-p node)
                      (find name (js2-function-node-params node)
                            :test '(lambda (name param) (string= name (js2-name-node-name param)))))
                 "Function parameter"
               (js2ac-format-node-doc node))))
    `(,name . ,doc)))

(defun js2ac-format-object-node-doc (obj-node)
  "Format OBJ-NODE to display as documentation."
  (let (elems)
    (unless (js2-object-node-p obj-node)
      (error "Node is not an object node"))
    (setq elems (js2-object-node-elems obj-node))
    (if (not elems)
        "{}"
      (mapconcat '(lambda (x) (js2ac-format-js2-object-prop-doc x)) elems "\n"))))

(defun js2ac-format-node-doc (node)
  "Format NODE for displaying in a document string."
  (let* ((node-above (and node (js2-node-at-point
                                (save-excursion
                                  (goto-char (js2-node-abs-pos node))
                                  (forward-line -1)
                                  (point)))))
         (comment (if (js2-comment-node-p node-above)
                      (js2ac-format-comment (js2-node-string node-above))))
         (doc (cond
               ((js2-function-node-p node)
                (js2ac-format-function node))
               ((js2-object-node-p node)
                (js2ac-format-object-node-doc node))
               ((js2-object-prop-node-p node)
                (js2ac-format-node-doc (js2-object-prop-node-right node)))
               (t
                (if (js2-node-p node) (js2-node-string node) "")))))
    (if comment (concat comment "\n" doc) doc)))

(defun js2ac-format-js2-object-prop-doc (obj-prop)
  "Format an OBJ-PROP for displaying as a document string."
  (unless (js2-object-prop-node-p obj-prop)
    (error "Node is not an object property node"))
  (let* ((left (js2-object-prop-node-left obj-prop))
         (right (js2-object-prop-node-right obj-prop)))
    (concat (js2-node-string left) " : "
            (js2ac-format-node-doc right))))

(defun js2ac-format-function (func)
  "Formats a function for a document string.
FUNC can be either a function node or a string starting with
'function'. Returns nil if neither."
  (let ((str (or (and (js2-function-node-p func) (js2-node-string func))
                 (and (stringp func) (eq 0 (string-match "function" func)) func))))
    (if str (substring str 0 (1+ (string-match ")" str))))))

(defun js2ac-format-comment (comment)
    "Prepare a COMMENT node for displaying in a popup."
    (let* ((node-string (if (js2-comment-node-p comment)
                                (js2-node-string comment)
                            comment))
           (string (replace-regexp-in-string "[ \t]$" ""
                                             (replace-regexp-in-string "^[ \t\n*/*]+" "" node-string))))
        string))

;;; Navigation commands for js2-mode

(defun js2ac-find-property (list-names)
  "Find the property definition that consists of LIST-NAMES.
Currently only the form 'foo.bar = 3' is supported opposed to
'foo = {bar: 3}'."
  (catch 'prop-found
    (js2-visit-ast-root
     js2-mode-ast
     (lambda (node endp)
       (let ((parent (js2-node-parent node)))
         (unless endp
           (if (and (js2-prop-get-node-p node)
                    (not (or (js2-elem-get-node-p parent) (js2-call-node-p parent)))
                    (equal list-names (js2ac-build-prop-name-list node)))
               (throw 'prop-found node))
           t))))))

(defun js2ac-get-function-node (name scope)
  "Return node of function named NAME in SCOPE."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (string= name (js2ac-get-function-name node)))
         (throw 'function-found node))
       t))
    nil))

;;;###autoload
(defun js2ac-jump-to-definition ()
    "Jump to the definition of an object's property, variable or function.
Navigation to a property definend in an Object literal isn't
implemented."
  (interactive)
  (let* ((node (js2-node-at-point))
         (parent (js2-node-parent node))
         (prop-names (if (js2-prop-get-node-p parent)
                         (js2ac-prop-names-left node)))
         (name (if (and (js2-name-node-p node)
                        (not (js2-object-prop-node-p parent)))
                   (js2-name-node-name node)
                 (error "Node is not a supported jump node")))
         (node-init (if (and prop-names (listp prop-names))
                        (js2ac-find-property prop-names)
                      (js2ac-name-declaration name))))
    (unless node-init
      (error "No jump location found"))
    (push-mark)
    (goto-char (js2-node-abs-pos node-init))))

(defun js2ac-get-function-name (fn-node)
  "Return the name of the function FN-NODE.
Value may be either function name or the variable name that holds
the function."
  (let ((parent (js2-node-parent fn-node)))
    (if (js2-function-node-p fn-node)
        (or (js2-function-name fn-node)
            (if (js2-var-init-node-p parent)
                (js2-name-node-name (js2-var-init-node-target parent)))))))

(define-key js2-mode-map (kbd "M-.") 'js2ac-jump-to-definition)

(provide 'ac-js2)

;;; ac-js2.el ends here
