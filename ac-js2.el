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

(defvar js2ac-js2-show-comments nil
  "This is used to keep track of a users js2 settings for showing
errors and warnings. Used to allow js2ac to show messages.")

;; Types of completion methods available
(defconst js2ac-method-global 1
  "For the time being only return keys of the global object due
  to including the externs from Js2. This is done to provide
  configuration options for the user.")

(defconst js2ac-method-eval 0)

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

(defmacro js2ac-call-skewer (&rest skewer-call)
  "Macro to make sure that no requests get sent to skewer when
there are no clients connected."
  `(if skewer-clients
       ,@skewer-call
     (setq js2ac-skewer-candidates nil)
     (when (and js2-mode-show-parse-errors js2-mode-show-strict-warnings)
       (setq js2ac-js2-show-comments t)
       (js2-mode-hide-warnings-and-errors))
     (message "No skewer clients connected or in a break point.")))

(defun js2ac-get-object-properties (beg object)
  "Find properties of OBJECT for completion. BEG is the position
in the buffer of the name of the OBJECT."
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (name (or object (buffer-substring-no-properties beg end)))
        (end (point)))
    (js2ac-call-skewer (skewer-eval name #'js2ac-skewer-result-callback
                                    :type "complete" :extra `((prototypes . ,js2ac-add-prototype-completions))))))

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
          ;; (replace-string "var" "   " nil pos (+ pos 3))
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
      (skewer-load-buffer))))

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


;; Auto-complete settings

(defun js2ac-ac-candidates()
  "Main function called to gather candidates for Auto-complete."
  (let ((node (js2-node-parent (js2-node-at-point (1- (point)))))
        beg
        name)
    (if js2ac-js2-show-comments (js2-mode-display-warnings-and-errors))
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
      (js2ac-call-skewer (skewer-eval "" #'js2ac-skewer-result-callback
                                      :type "complete" :extra `((method . ,js2ac-method-global))))
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
    result))

;; Borrowed from js2r-functions.el
(defun js2r--is-var-function-expression (node)
  (and (js2-function-node-p node)
       (js2-var-init-node-p (js2-node-parent node))))

(defun js2ac-determine-node-name (node)
  "Determines the name for the node "
  (cond
   ((js2-function-node-p node) (js2-function-name node))

   ((js2r--is-var-function-expression node) (js2-name-node-name
                                             (js2-var-init-node-target (js2-node-parent node))))
   ((js2-var-init-node-p node) (js2-name-node-name
                                (js2-var-init-node-target node)))
   (t
    nil)))

(provide 'ac-js2)

;;; ac-js2.el ends here
