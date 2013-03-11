;;; Tests for ac-js2

(require 'ert)
(require 'skewer-mode)
(require 'js2-mode)
(require 'ac-js2)

;;; Must have a skewer client connected before running the tests
;; Need to call httpd-stop from main Emacs if running tests in batch mode
(unless skewer-clients
  (run-skewer))

(defun ac-js2-flush-buffer-response (completion-func)
  "Need to call COMPLETION-FUNC after sending a request to the
browser as the buffer contents get sent first and then the
request for candidates."
  (let ((timer 0)
        (wait '(lambda () (while (and (< timer 10)
                                 (not ac-js2-skewer-candidates))
                       (sit-for 0.5)
                       (incf timer)))))
    (setq ac-js2-skewer-candidates nil)
    (funcall wait)
    (setq timer 0)
    (setq ac-js2-skewer-candidates nil)
    (funcall completion-func)
    (funcall wait)))

(defmacro ac-js2-test-completion (test-function primary-activation secondary-activation)
  "Macro to help test the various completion frontends.
TEST-FUNCTION is the function to test, PRIMARY-ACTIVATION is the
method to activate automatic completion and SECONDARY-ACTIVATION
is a list or a function that returns a list of candidates."
  `(ert-deftest ,test-function ()
     (let (property
           property-dot
           func-call
           var)
       (with-temp-buffer
         (insert "
  var temp = function(param1, param2) {
    var localParam = 15;
    return param1 + param2;
  };

  var look;

temp.aFun = function(lolParam) {};
temp.anotherFunction = function() { return {about: 3};}")
         (setq ac-js2-evaluate-calls t)
         (setq ac-js2-external-libraries nil)

         (js2-mode)
         (js2-parse)

         (insert "tem")
         (funcall ',primary-activation)
         (setq var (thing-at-point 'word))

         (insert ".")
         (funcall ',secondary-activation)
         (ac-js2-flush-buffer-response ',secondary-activation)
         (setq property-dot ac-js2-skewer-candidates)

         (insert "aF")
         (funcall ',primary-activation)
         (setq property (thing-at-point 'word))

         (backward-kill-word 1)
         (insert "anotherFunction().")
         (funcall ',secondary-activation)
         (ac-js2-flush-buffer-response ',secondary-activation)
         (setq func-call ac-js2-skewer-candidates))

       (should (string= var "temp"))
       (should (string= "aFun" property))
       (should (and (assoc 'aFun property-dot) (assoc 'name property-dot)))
       (should (assoc 'about func-call)))))

(ac-js2-test-completion ac-js2-candidates auto-complete ac-js2-ac-candidates)
(ac-js2-test-completion ac-js2-completion-function completion-at-point completion-at-point)
