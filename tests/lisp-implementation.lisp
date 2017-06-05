(in-package "TEMPLATE-FUNCTION.TESTS")
(in-suite all-template-function-tests)

(defun shadowable-operator (value)
  (1+ value))

(define-compiler-macro shadowable-operator (value)
  `(+ ,value 1))

(test shadowed-compiler-macro-functions
  (macrolet ((trial (&environment env)
               (and (compiler-macro-function 'shadowable-operator env)
                    t)))
    (is-true (trial))
    (flet ((shadowable-operator (value)
             (1- value)))
      (declare (ignorable (function shadowable-operator)))
      (is-false (trial)))))
