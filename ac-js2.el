;;; ac-js2.el --- Autocomplete source for Js2-mode

;;; Commentary:
;;
;; An attempt to get context sensitive completion in Emacs.
;;

;;; Code:

(require 'js2-mode)
(require 'auto-complete)
(require 'skewer-mode)
(require 'json)
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

(defcustom js2ac-external-javscript-libraries '()
  "List of absolute paths to external Javascript libraries. ")

(defcustom js2ac-evaluate-calls nil
  "Warning!!! When true function calls will be evaluated in the
  browser. This may cause undesired side effects however it will
  provide better completions. Use at your own risk.")

;;; Internal variables

(defvar js2ac-keywords '()
  "Cached string version of js2-keywords")

(defvar js2ac-candidates '())

;; Types of skewer completion methods available
(defconst js2ac-method-eval 0)
(defconst js2ac-method-global 1
  "For the time being only return keys of the global object due
  to including the externs from Js2. This is done to provide
  configuration options for the user.")

(defvar skewer-hide-comments nil)

;;; Skewer integration

(defvar js2ac-skewer-candidates '()
  "Cadidates obtained from skewering")

(defvar js2ac-data-root (file-name-directory load-file-name)
  "Location of data files needed for js2ac-on-skewer-load")

(defun js2ac-on-skewer-load ()
  (insert-file-contents (expand-file-name "skewer-addon.js" js2ac-data-root)))

(add-hook 'skewer-js-hook 'js2ac-on-skewer-load)

(defun js2ac-skewer-completion-candidates ()
  (mapcar (lambda (candidate) (symbol-name (car candidate))) js2ac-skewer-candidates))

(defun js2ac-skewer-document-candidates (name)
  (cdr (assoc-string name js2ac-skewer-candidates)))

(defun js2ac-get-object-properties (name)
  "Find properties of NAME for completion."
  (js2ac-skewer-eval-wrapper name `((prototypes . ,js2ac-add-prototype-completions))))

(defun js2ac-skewer-eval-wrapper (name extras)
  "Ping the client to see if there are any browsers connected
before issuing a request."
  (if (skewer-ping)
      (progn
        (if (or js2ac-evaluate-calls
                (not (js2ac-has-funtion-calls name)))
            (skewer-eval name #'js2ac-skewer-result-callback
                         :type "complete"
                         :extra extras)
          (setq js2ac-skewer-candidates nil))
        (when skewer-hide-comments
          (js2-mode-toggle-warnings-and-errors)
          (setq skewer-hide-comments nil))
        )
    (when (and js2-mode-show-parse-errors js2-mode-show-strict-warnings (not skewer-hide-comments))
      (setq skewer-hide-comments t)
      (js2-mode-toggle-warnings-and-errors)
      (setq js2ac-skewer-candidates nil
            skewer-queue nil))
    (message "No skewer connected")))

(defun js2ac-skewer-result-callback (result)
  "Callback called once browser has evaluated the properties for an object."
  (let ((value (cdr (assoc 'value result))))
    (if (and (skewer-success-p result) value)
        (setq js2ac-skewer-candidates (append value nil))
      (setq js2ac-skewer-candidates nil))))

;; Auto-complete settings

(defun js2ac-ac-candidates()
  "Main function called to gather candidates for Auto-complete."
  (let ((node (js2-node-parent (js2-node-at-point (1- (point)))))
        beg
        name)
    (setq js2ac-candidates nil)
    (cond
     ((looking-back "\\.")
      ;; TODO: Need to come up with a better way to extract object than this regex!!
      (save-excursion
        (setq beg (and (skip-chars-backward "[a-zA-Z_$][0-9a-zA-Z_$#\"())]+\\.") (point))))
      (setq name (buffer-substring-no-properties beg (1- (point))))
      (js2ac-get-object-properties name)
      (setq node (js2ac-initialized-node name))
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
      (setq name (if (js2-call-node-p node) (js2-node-string node) (js2-name-node-name node)))
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

(defun js2ac-mode-sources ()
  "Starts skewer if not already running and loads external
libraries if required."
  (when (not skewer-clients)
    (run-skewer)
    (if js2ac-evaluate-calls
        (mapcar (lambda (library)
                  (with-temp-buffer
                    (insert-file-contents library)
                    (skewer-load-buffer))) js2ac-external-javscript-libraries)))
  (if js2ac-evaluate-calls (skewer-load-buffer))
  (setq ac-sources '(ac-source-js2)))

(add-hook 'js2-mode-hook 'js2ac-mode-sources)

(defun js2ac-skewer-load-buffer ()
  (and (string= major-mode "js2-mode")
       js2ac-evaluate-calls
       (skewer-load-buffer)))

(add-hook 'before-save-hook 'js2ac-skewer-load-buffer)

(ac-define-source "js2"
  '((candidates . js2ac-ac-candidates)
    (document . js2ac-ac-document)
    (prefix .  js2ac-ac-prefix)
    (requires . -1)))

;;; Helper functions

(defun js2ac-build-prop-name-list (prop-node)
  "Build a list of names from a js2-prop-get-node."
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
  "Creates a list of all of the names of the property NAME-NODE.
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
  "Checks if STRING contains a Js2-call-node."
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

(defun js2ac-tidy-comment (comment)
  (let* ((node-string (if (js2-comment-node-p comment)
                          (js2-node-string comment)
                        comment))
         (string (replace-regexp-in-string "[ \t]$" ""
                                           (replace-regexp-in-string "^[ \t\n*/*]+" "" node-string))))
    string))

(defun js2ac-root-or-node ()
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
                      (js2ac-build-document-string name init)))
                  result))))

(defun js2ac-initialized-node (name)
  "Return initial value for NAME. NAME may be either a variable,
a function or a variable that holds a function. Returns nil if no
initial value can be found."
  (let* ((node (js2ac-name-declaration name))
         (parent (if node (js2-node-parent node)))
         (init (cond
                ((js2-function-node-p parent)
                 parent)
                ((js2-function-node-p node)
                 node)
                ((js2-var-init-node-p parent)
                 (js2-var-init-node-initializer parent))
                (t
                 nil))))
    init))

(defun js2ac-build-document-string (name node)
  (let* ((node-above (js2-node-at-point
                      (save-excursion
                        (goto-char (js2-node-abs-pos node))
                        (forward-line -1)
                        (point))))
         (comment (if (js2-comment-node-p node-above)
                      (js2-node-string node-above)))
         (info (js2ac-format-node name node))
         (interface (first (last info))))
    (if comment
        (setcdr info (concat (js2ac-tidy-comment comment) "\n" interface)))
    info))

(defun js2ac-name-declaration (name)
  "Returns the declaration node for node named NAME."
  (let* ((node (js2ac-root-or-node))
         (scope-def (js2-get-defining-scope node name))
         (scope (if scope-def (js2-scope-get-symbol scope-def name) nil))
         (symbol (if scope (js2-symbol-ast-node scope) nil)))
    (if (not symbol)
        (js2ac-get-function-node name scope-def)
      symbol)))

;;; Completion candidate formating

(defun js2ac-format-node (name node)
  "Formats NODE for completions. Returned format is a list
where the first element is the NAME of the node (shown in
completion candidate list) and the last element is the text to
show as documentation."
  (let ((doc (cond
              ((js2-object-node-p node)
               (js2ac-format-object-node-doc node))
              ((js2-object-prop-node-p node)
               (js2ac-format-node-doc (js2-object-prop-node-right node)))
              ((js2-function-node-p node)
               (if (find name (js2-function-node-params node)
                         :test 'js2ac-param-name-p)
                   "Function parameter"
                 (js2ac-format-function node)))
              (t
               (js2-node-string node)))))
    `(,name . ,doc)))

(defun js2ac-param-name-p (name param)
  "Used to check if NAME matches PARAM name."
  (string= name (js2-name-node-name param)))

(defun js2ac-format-object-node-doc (obj-node)
  (let (elems)
    (unless (js2-object-node-p obj-node)
      (error "Node is not an object node"))
    (setq elems (js2-object-node-elems obj-node))
    (if (not elems)
        "{}"
      (mapconcat '(lambda (x) (js2ac-format-js2-object-prop-doc x)) elems "\n"))))

(defun js2ac-format-node-doc (node)
  "Format NODE to display in a document string."
  (let ((string (js2-node-string node)))
    (cond
     ((js2-function-node-p node)
      (js2ac-format-function node))
     ((js2-object-node-p node)
      "[Object]")
     (t
      string))))

(defun js2ac-format-js2-object-prop-doc (obj-prop)
  "Format an OBJ-PROP for displaying as a document string."
  (unless (js2-object-prop-node-p obj-prop)
    (error "Node is not an object property node"))
  (let* ((left (js2-object-prop-node-left obj-prop))
         (right (js2-object-prop-node-right obj-prop)))
    (concat (js2-node-string left) " : "
            (js2ac-format-node-doc right))))

(defun js2ac-format-function (fun-node)
  (unless (js2-function-node-p fun-node)
    (error "Node is not a function node"))
  (let ((str (js2-node-string fun-node)))
    (substring str 0 (1+ (string-match ")" str)))))

;;; Navigation commands for js2-mode

(defun js2ac-find-property (list-names)
  "Find the property definition that matches the list of
LIST-NAMEs. Currently only the form 'foo.bar = 3' is supported
opposed to 'foo = {bar: 3}'."
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
  "Find NAME of function in SCOPE. Returns nil if node could not
be found."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (string= name (js2ac-get-function-name node)))
         (throw 'function-found node))
       t))
    nil))

(defun js2ac-jump-to-var ()
  "Jump to the definition of an object's property, variable or
function. Navigation to a property definend in an Object literal
isn't implemented."
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
  "Returns the name of the function or the variable that holds
the function if present."
  (let ((parent (js2-node-parent fn-node)))
    (if (js2-function-node-p fn-node)
        (or (js2-function-name fn-node)
            (if (js2-var-init-node-p parent)
                (js2-name-node-name (js2-var-init-node-target parent)))))))

(define-key js2-mode-map (kbd "M-.") 'js2ac-jump-to-var)

(provide 'ac-js2)

;;; ac-js2.el ends here
