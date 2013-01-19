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
  "List of absolute paths to external Javascript libraries")

;;; Internal variables

(defvar js2ac-keywords '()
  "Cached string version of js2-keywords")

;; Types of completion methods available
(defconst js2ac-method-eval 0)
(defconst js2ac-method-global 1
  "For the time being only return keys of the global object due
  to including the externs from Js2. This is done to provide
  configuration options for the user.")
(defconst js2ac-method-reset 2)

(defvar js2ac-abs-scope-pos nil
  "Used to keep track of the current scope. Needed to use a position for anonomous functions.")

;;; Skewer integration

(defvar js2ac-skewer-candidates '()
  "Cadidates obtained from skewering")

(defvar js2ac-data-root (file-name-directory load-file-name)
  "Location of data files needed for js2ac-on-skewer-load")

(defconst js2ac-scope-object "ScopeObject"
  "Name of the Javascript object that holds completions for the current scope.")

(defun js2ac-on-skewer-load ()
  (insert-file-contents (expand-file-name "skewer-addon.js" js2ac-data-root)))

(add-hook 'skewer-js-hook 'js2ac-on-skewer-load)

(defun js2ac-skewer-completion-candidates ()
  (mapcar (lambda (candidate) (symbol-name (car candidate))) js2ac-skewer-candidates))

(defun js2ac-skewer-document-candidates (name)
  (cdr (assoc-string name js2ac-skewer-candidates)))

(defun js2ac-get-object-properties (beg object)
  "Find properties of OBJECT for completion. BEG is the position
in the buffer of the name of the OBJECT."
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (name (or object (buffer-substring-no-properties beg end)))
        (end (point)))
    (skewer-eval name #'js2ac-skewer-result-callback
                 :type "complete"
                 :extra `((prototypes . ,js2ac-add-prototype-completions)))))

(defun js2ac-skewer-result-callback (result)
  "Callback called once browser has evaluated the properties for an object."
  (let ((value (cdr (assoc 'value result))))
    (if (and (skewer-success-p result) value)
        (setq js2ac-skewer-candidates (append value nil))
      (setq js2ac-skewer-candidates nil))))


(defun js2ac-prepare-scope (block-node scope-name)
  "Prepare a BLOCK-NODE to be sent for evaluation for
completions. SCOPE-NAME is the name to attach all vars and
functions to."
  (let (ast
        buf
        expr
        (offset 0)
        len
        pos)
    (with-temp-buffer
      (js2-print-block block-node 0)
      (delete-char -1)
      (goto-char (point-min))
      (delete-char 1)
      (setq ast (js2-parse))
      (setq ast(js2-ast-root-kids ast))
      (dolist (node ast)
        ;; Prepare variable statements
        (when (and (js2-expr-stmt-node-p node)
                   (js2-var-decl-node-p (setq expr (js2-expr-stmt-node-expr node))))
          (setq pos (+ (js2-node-abs-pos expr) offset))
          (goto-char pos)
          (delete-char 3)
          (insert "   ")
          (dolist (init-node (js2-var-decl-node-kids expr))
            (goto-char (+ (js2-node-abs-pos init-node) offset))
            (insert scope-name ".")
            (setq offset (+ offset (length scope-name) 1))))
        ;; Prepare function statements
        (when (js2-function-node-p node)
          (goto-char (+ (js2-node-abs-pos node) offset))
          (insert scope-name "." (js2-function-name node) " = ")
          (setq offset (+ offset (length scope-name) (length (js2-function-name node)) 4)))
        ;; Remove return statement
        (when (js2-return-node-p node)
          (setq pos (+ (js2-node-abs-pos node) offset))
          (goto-char pos)
          (setq len (js2-node-len node))
          (delete-char len)
          (setq offset (- offset len))))
      (setq buf (buffer-substring-no-properties (point-min)(point-max)))
      ;; (skewer-eval "" #'js2ac-blank-callback)
      (skewer-load-buffer)
      )
    (print buf)
    ))

(defun js2ac-evaluate-scope ()
  "Evaluates the enclosing scope at point and all parent nodes."
  (let* ((scope (js2ac-root-or-node))
         (block-nodes '()))
    (while scope
      (if (js2-function-node-p scope)
          (add-to-list 'block-nodes (js2-function-node-body scope)))
      (setq scope (js2-node-parent scope)))
    (dolist (block-node (nreverse block-nodes))
      (js2ac-prepare-scope block-node js2ac-scope-object))))


(defun js2ac-enclosing-function-pos ()
  (let* ((node (js2-node-at-point)))
    (unless (js2-ast-root-p node)
      (setq node (js2-node-parent node))
      (while (or (not (js2-function-node-p node)) (js2-ast-root-p node))
        (setq node (js2-node-parent node))))
    (js2-node-abs-pos node)))

(defun js2ac-blank-callback (result)
  (assoc 'status result))

;; Auto-complete settings

(defun js2ac-ac-candidates()
  "Main function called to gather candidates for Auto-complete."
  (let ((node (js2-node-parent (js2-node-at-point (1- (point)))))
        beg
        name)
    ;; (when (not (= js2ac-abs-scope-pos (js2ac-enclosing-function-pos)))
    ;;   (setq js2ac-abs-scope-pos (js2ac-enclosing-function-pos))
    ;; (skewer-eval "blank" #'js2ac-blank-callback :type "complete" :extra `((method . ,js2ac-method-reset)));)

    ;;     (js2ac-evaluate-scope)

    ;; )
    (cond
     ((js2-prop-get-node-p node)
      (setq beg (js2-node-abs-pos node))
      (setq node (js2-prop-get-node-left node))
      (setq name (if (js2-call-node-p node) (js2-node-string node) (js2-name-node-name node)))
      (js2ac-get-object-properties beg name)
      (js2ac-skewer-completion-candidates))
     ((looking-back "\\.")
      ;; TODO: Need to come up with a better way to extract object than this regex!!
      (save-excursion
        (setq beg (and (skip-chars-backward "[a-zA-Z_$][0-9a-zA-Z_$#\"())]+\\.") (point))))
      (setq name (buffer-substring-no-properties beg (1- (point))))
      (js2ac-get-object-properties beg name)
      (js2ac-skewer-completion-candidates))
     (t
      (skewer-eval "" #'js2ac-skewer-result-callback
                   :type "complete" :extra `((method . ,js2ac-method-global)))
      (append (js2ac-add-extra-completions
               (mapcar (lambda (node)
                         (let ((name (symbol-name (first node))))
                           (unless (string= name (thing-at-point 'symbol)) name)))
                       (js2ac-get-names-in-scope)))
              (js2ac-skewer-completion-candidates))))))

(defun js2ac-ac-grab-names (node name)
  "Search through scope of NODE looking for NAME."
  (let* ((scope (if (js2-function-node-p node)
                    (js2-function-node-body node)
                  node)))
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (unless (js2-object-node-p node)
         (when (not end-p)
           (cond
            ((js2-function-node-p node)
             (let ((name-node (js2-function-node-name node)))
               (if name-node (js2ac-comment-for-name name-node name))nil))
            ((js2-var-init-node-p node)
             (let ((target-node (js2-var-init-node-target node)))
               (if (js2-name-node-p target-node) (js2ac-comment-for-name target-node name))))
            (t t))))))))

(defun js2ac-ac-document(name)
  "Loops over the names in the current scope and on all name nodes in parent nodes."
  (let* ((node (js2ac-root-or-node))
         (scope (js2-get-defining-scope node name)))
    (concat
     (catch 'found
       (while node
         (js2ac-ac-grab-names node name)
         (setq node (js2-node-parent node))))
     (js2ac-skewer-document-candidates name))))

(defun js2ac-comment-for-name (node name)
  "NODE is the node to check and NAME is the name of the node to find."
  (if (string= (js2-name-node-name node) name)
      (let ((pos (js2-node-abs-pos node))
            beg-comment
            end-comment)
        (dolist (comment (js2-ast-root-comments js2-mode-ast) nil)
          (setq beg-comment (js2-node-abs-pos comment)
                end-comment (+ beg-comment (js2-node-len comment)))
          (if (= 1 (- (line-number-at-pos pos) (line-number-at-pos end-comment)))
              (throw 'found (js2ac-tidy-comment comment)))))))

(defun js2ac-ac-prefix()
  (or (ac-prefix-default) (ac-prefix-c-dot)))

(defun js2ac-mode-sources ()
  (when (not skewer-clients)
    (run-skewer)
    (mapcar (lambda (library)
              (with-temp-buffer
                (insert-file-contents library)
                (skewer-load-buffer))) js2ac-external-javscript-libraries))
  (skewer-load-buffer)
  (setq ac-sources '(ac-source-js2)))

(add-hook 'js2-mode-hook 'js2ac-mode-sources)

(defun js2ac-skewer-load-buffer ()
  (if (string= major-mode "js2-mode")
      (skewer-load-buffer)))

(add-hook 'before-save-hook 'js2ac-skewer-load-buffer)

(ac-define-source "js2"
  '((candidates . js2ac-ac-candidates)
    (document . js2ac-ac-document)
    (prefix .  js2ac-ac-prefix)
    (requires . -1)))

;;; Helper functions

(defun js2ac-add-extra-completions (completions)
  "Add extra candidates to COMPLETIONS."
  (append completions
          (if js2ac-add-keywords (or js2ac-keywords (setq js2ac-keywords (mapcar 'symbol-name js2-keywords))))
          (if js2ac-add-ecma-262-externs js2-ecma-262-externs)
          (if js2ac-add-browser-externs js2-browser-externs)))

(defun js2ac-tidy-comment (comment)
  (let* ((node-string (js2-node-string comment))
         (string (replace-regexp-in-string "[ \t]$" ""
                                           (replace-regexp-in-string "^[ \t\n*/*]+" "" node-string))))
    string))

(defun js2ac-root-or-node ()
  (let ((node (js2-node-at-point)))
    (if (js2-ast-root-p node)
        node
      (js2-node-get-enclosing-scope node))))



(defun js2ac-get-names-in-scope ()
  (let* ((scope (js2ac-root-or-node))
         result)
    (while scope
      (setq result (append result (js2-scope-symbol-table scope)))
      (setq scope (js2-scope-parent-scope scope)))
    (setq js2-test-scope result)
    result))

;;; TEST
(defun js2ac-names-in-scope (name)
  (let* ((scope (js2ac-root-or-node))
         (node (js2-get-defining-scope scope name))
         (var-init-node (js2-node-parent (js2-symbol-ast-node
                                          (js2-scope-get-symbol node name))))
         (initializer (js2-var-init-node-initializer
                       var-init-node)))
    (unless initializer
      (error "Var isn't intialized when defined"))
    (if (js2-object-node-p initializer)
        (let ((elems (js2-object-node-elems initializer)))
          (mapcar 'js2ac-format-js2-object-prop elems)))))

(defun js2ac-format-js2-object-prop (obj-prop)
  (unless (js2-object-prop-node-p obj-prop)
    (error "Node is not an object property node"))
  (let* ((left (js2-object-prop-node-left obj-prop))
         (right (js2-object-prop-node-right obj-prop))
         (right-string (js2-node-string right)))
    `(,(js2-node-string left) .
      ,(if (js2-function-node-p right)
           (js2ac-format-function right-string)
         right-string))))

(defun js2ac-format-function (str)
  (substring str 0 (1+ (string-match ")" str))))

;;; Navigation commands for js2-node

(defun js2ac-get-init-node (name-node)
  "Find the initial declaration of NAME-NODE. "
  (let* ((parent (js2-node-parent name-node))
         (name (if (and (js2-name-node-p name-node)
                        (not (js2-prop-get-node-p parent))
                        (not (js2-object-prop-node-p parent)))
                   (js2-name-node-name name-node)
                 (error "Node is not a supported jump node")))
         (scope (js2-node-get-enclosing-scope node))
         (scope (js2-get-defining-scope scope name))
         (end-pos (js2-node-abs-end name-node)))
    (save-excursion
      (cond
       ((and (js2-call-node-p parent)
             (goto-char end-pos)
             (looking-at "[\n\t ]*("))
        (js2ac-get-function-node name scope))
       ;; TODO Add support for jumping to object property
       ;; ((js2-propt-get-node-p parent)
       ;;  ())
       (t
        (js2-symbol-ast-node
         (js2-scope-get-symbol scope name)))))))

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
  (interactive)
  (let* ((node (js2-node-at-point))
         (node-init (js2ac-get-init-node node)))
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
