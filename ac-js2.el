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

(defvar js2ac-candidates '())

;; Types of completion methods available
(defconst js2ac-method-eval 0)
(defconst js2ac-method-global 1
  "For the time being only return keys of the global object due
  to including the externs from Js2. This is done to provide
  configuration options for the user.")

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

(defun js2ac-get-object-properties (beg object)
  "Find properties of OBJECT for completion. BEG is the position
in the buffer of the name of the OBJECT."
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (name (or object (buffer-substring-no-properties beg end)))
        (end (point)))
    ;; (skewer-eval name #'js2ac-skewer-result-callback
    ;;              :type "complete"
    ;;              :extra `((prototypes . ,js2ac-add-prototype-completions)))
    ))

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
      ;; (skewer-eval "" #'js2ac-skewer-result-callback
      ;;              :type "complete" :extra `((method . ,js2ac-method-global)))
      (append (js2ac-add-extra-completions
               (mapcar (lambda (x)
                         (first x))
                       (js2ac-get-names-in-scope))
               ;;(js2ac-skewer-completion-candidates)
               ))))))

(defun js2ac-ac-document(name)
  "Loops over the names in the current scope and on all name nodes in parent nodes."
  (let* ((doc (cdr (assoc name js2ac-candidates))))
    (if (listp doc) (first doc) doc)))

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
                           (node (js2ac-name-declaration name))
                           (parent (js2-node-parent node))
                           (init (cond
                                  ((js2-function-node-p parent)
                                   parent)
                                  ((js2-function-node-p node)
                                   node)
                                  (t
                                   (js2-var-init-node-initializer parent)))))
                      (js2ac-build-document-string name init)))
                  result))))

(defun js2ac-build-document-string (name node)
  (let* ((node-above (js2-node-at-point
                      (save-excursion
                        (goto-char (js2-node-abs-pos node))
                        (forward-line -1)
                        (point))))
         (comment (if (js2-comment-node-p node-above)
                      (js2-node-string node-above)))
         (info (js2ac-format-node name init))
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
completion candidate list), the second element is the text to
show as documentation and the last element the absolute position of NODE."
  (let ((doc (cond
              ((js2-object-node-p node)
               (js2ac-format-object-node node))
              ((js2-function-node-p node)
               (if (find name (js2-function-node-params node)
                         :test 'js2ac-param-name-p)
                   "Function parameter"
                 (js2ac-format-function node)))
              (t
               (js2-node-string node)))))
    `(,name ,doc)))

(defun js2ac-param-name-p (name param)
  "Used to check if NAME matches PARAM name."
  (string= name (js2-name-node-name param)))

(defun js2ac-format-object-node (obj-node)
  (let (elems)
    (unless (js2-object-node-p obj-node)
      (error "Node is not an object node"))
    (setq elems (js2-object-node-elems obj-node))
    (if (not elems)
        "{}"
      (mapconcat '(lambda (x) (js2ac-format-js2-object-prop x)) elems "\n"))))

(defun js2ac-format-js2-object-prop (obj-prop)
  (unless (js2-object-prop-node-p obj-prop)
    (error "Node is not an object property node"))
  (let* ((left (js2-object-prop-node-left obj-prop))
         (right (js2-object-prop-node-right obj-prop))
         (right-string (js2-node-string right)))
    (concat (js2-node-string left) " : "
            (cond
             ((js2-function-node-p right)
              (js2ac-format-function right))
             ((js2-object-node-p right)
              "[Object]")
             (t
              right-string)))))

(defun js2ac-format-function (fun-node)
  (unless (js2-function-node-p fun-node)
    (error "Node is not a function node"))
  (let ((str (js2-node-string fun-node)))
    (substring str 0 (1+ (string-match ")" str)))))

;;; Navigation commands for js2-node

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
         (parent (js2-node-parent node))
         (name (if (and (js2-name-node-p node)
                        (not (js2-prop-get-node-p parent))
                        (not (js2-object-prop-node-p parent)))
                   (js2-name-node-name node)
                 (error "Node is not a supported jump node")))
         (node-init (js2ac-name-declaration name)))
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
