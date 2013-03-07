;;; Tests for ac-js2

(require 'ert)

(defun ac-js2-flush-buffer-response ()
  "Need to call `ac-js2-ac-candidates' twice, once for the whole
   buffer and once for the completion at point."
  (setq ac-js2-skewer-candidates nil)
  (while (not ac-js2-skewer-candidates) (sit-for 0.5))
  (setq ac-js2-skewer-candidates nil)
  (ac-js2-ac-candidates)
  (while (not ac-js2-skewer-candidates) (sit-for 0.5)))

(ert-deftest ac-js2-candidates ()
  "Tests completions for `tem', `temp.', `temp.aF' and `temp.anotherFunction().' "
  (let (property
        property-dot
        func-call
        var)
    (unless skewer-clients
      (run-skewer))
    (with-temp-buffer

      (insert "
  var temp = function(param1, param2) {
    var localParam = 15;
    return param1 + param2;
  };

  var look;

temp.aFun = function(lolParam) {};
temp.anotherFunction = function() { return {about: 3};}
")
      (setq ac-js2-evaluate-calls t)
      (setq ac-js2-external-libraries nil)
      (js2-mode)
      (js2-parse)

      (insert "tem")
      (auto-complete)
      (setq var (thing-at-point 'word))

      (insert ".")
      (ac-js2-ac-candidates)
      (ac-js2-flush-buffer-response)
      (setq property-dot ac-js2-skewer-candidates)

      (insert "aF")
      (auto-complete)
      (setq property (thing-at-point 'word))

      (backward-kill-word 1)
      (insert "anotherFunction().")
      (ac-js2-ac-candidates)
      (ac-js2-flush-buffer-response)
      (setq func-call ac-js2-skewer-candidates))
    (should (string= "temp" var))
    (should (string= "aFun" property))
    (should (and (assoc 'aFun property-dot) (assoc 'name property-dot)))
    (should (assoc 'about func-call))))
