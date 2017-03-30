(in-package "TEMPLATE-FUNCTION.TESTS")
(in-suite all-template-function-tests)

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "template-function-tests"))))
  (run! 'all-template-function-tests))
