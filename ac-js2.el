;;; ac-js2.el --- Autocomplete source for Js2-mode


;;; Commentary:
;;
;; TODO:
;; - Add autocompletion for function prototypes
;; - Cannot handle closures


;;; Code:
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

(defun js2ac-ac-candidates()
  (mapcar (lambda (node)
            (symbol-name (first node)))
          (js2ac-get-names-in-scope)))

(ac-define-source "js2"
  '((candidates . js2ac-ac-candidates)
    (document . js2ac-object-definition)))

(provide 'ac-js2)

;;; ac-js2.el ends here
