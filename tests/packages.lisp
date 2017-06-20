(defpackage "TEMPLATE-FUNCTION.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-TEMPLATE-FUNCTION-TESTS"))

(defpackage "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION.TESTS"
  (:use "COMMON-LISP"
        "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION"
        "FIVEAM")
  (:export "ALL-ARGUMENT-SPECIFICATION-TESTS"))

(5am:def-suite template-function.tests:all-template-function-tests)
(5am:def-suite template-function.argument-specification.tests:all-argument-specification-tests
  :in template-function.tests:all-template-function-tests)
