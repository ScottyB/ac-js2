;;; ac-js2.el --- Autocomplete source for Js2-mode


;;; Commentary:
;;
;; TODO:
;; - Add autocompletion for function prototypes
;; - Add support for external libraries
;; - Add support for dictionaries
;; - Add support for Javascript externs provided by js2

(require 'js2-mode)
(require 'auto-complete)

;;; Code:
(defvar js2ac-var-regex "[a-zA-Z_$][0-9a-zA-Z_$]+\\."
  "Regex string for characters used in a Javascript var. Assuming
  nobody wants to use unicode.")

(defun js2ac-root-or-node (&optional node)
  (let ((outer-scope (or node (js2-node-at-point))))
    (if (js2-ast-root-p outer-scope)
        outer-scope
      (js2-node-get-enclosing-scope outer-scope))))

(defun js2ac-get-names-in-scope ()
  (let* ((scope (js2ac-root-or-node))
         result)
    (while scope
      (setq result (append result (js2-scope-symbol-table scope)))
      (setq scope (js2-scope-parent-scope scope)))
    result))

(defun js2ac-find-symbol-in-table (name node)
  (let* ((scope (js2-node-get-enclosing-scope node))
         (scope (js2-get-defining-scope scope name))
         (found (js2-scope-get-symbol scope name))
         val)
    (catch 'done
      (js2-visit-ast
       scope
       (lambda (node end-p)
         (unless end-p
           (when (js2-object-node-p node)
             (setq val (js2-object-node-elems node))
             (throw 'done (mapcar (lambda (value)
                                    ;; TODO: Add support for numbers and string values
                                    (js2-name-node-name (js2-object-prop-node-left value))
                                    ) val))))
         t)))))

;; Borrowed from js2r-functions.el
(defun js2r--is-var-function-expression (node)
  (and (js2-function-node-p node)
       (js2-var-init-node-p (js2-node-parent node))))

(defun js2ac-var-name (node)
  "Returns the name node"
  (js2-name-node-name
   (js2-var-init-node-target (if (js2-var-init-node-p node)
                                 node
                               (js2-node-parent node)))))

(defun js2ac-determine-node-name (node)
  (cond
   ((js2-function-node-p node) (js2-function-name node))

   ((js2r--is-var-function-expression node) (js2-name-node-name
                                             (js2-var-init-node-target (js2-node-parent node))))
   ((js2-var-init-node-p node) (js2-name-node-name
                                (js2-var-init-node-target node)))
   (t
    nil)))

(defun js2ac-object-definition (name)
  (let* ((scope (js2ac-root-or-node))
         (scope (js2-get-defining-scope scope name))
         pos
         beg-comment
         end-comment)
    (catch 'done
      (js2-visit-ast
       scope
       (lambda (node end-p)
         (unless end-p
           (setq node-name (js2ac-determine-node-name node))
           (when (string= node-name name)
             (setq pos (js2-node-abs-pos node))
             (dolist (comment (js2-ast-root-comments js2-mode-ast) nil)
               (setq beg-comment (js2-node-abs-pos comment)
                     end-comment (+ beg-comment (js2-node-len comment)))
               (if (= 1 (- (line-number-at-pos pos) (line-number-at-pos end-comment)))
                   (throw 'done (js2ac-tidy-comment comment))))))
         t)))))

(defun js2ac-tidy-comment (comment)
  (let* ((string (js2-node-string comment))
         (string (replace-regexp-in-string "[ \t\n]$" ""
                                           (replace-regexp-in-string "^[ \t\n*/*]+" "" string))))
    string))



(defun js2ac-get-object-properties ()
  (let ((end (1- (point)))
        beg
        name
        result)
    (save-excursion
      (setq beg (+ (skip-chars-backward "[a-zA-Z_$][0-9a-zA-Z_$]+\\.") end))
      (setq name (buffer-substring-no-properties (1+ beg) end))
      (setq result (js2ac-find-symbol-in-table name (js2-node-at-point))))))

(defun js2ac-ac-candidates()
  (message "Autocomplete candidates working")
  (if (looking-back "\\.")
      ;; TODO: Need to check for prototype chain
      (js2ac-get-object-properties)
    (mapcar (lambda (node)
              (symbol-name (first node)))
            (js2ac-get-names-in-scope))))

;; (makunbound 'js2ac-complete)
;; (makunbound 'js2ac-complete-on-dot)
(defun js2ac-complete-on-dot ()
  (interactive)
  (let ((ac-expand-on-auto-complete nil))
    (auto-complete '(ac-source-js2))))

(defun js2ac-prefix()
  (or (ac-prefix-default) (ac-prefix-c-dot)))

(ac-define-source "js2"
  '((candidates . js2ac-ac-candidates)
    (document . js2ac-object-definition)
    (prefix .  js2ac-prefix)
    (requires . -1)))

(provide 'ac-js2)

;;; ac-js2.el ends here
;; js2-mode-dev-mode-p
