(in-package "TEMPLATE-FUNCTION.TESTS")
(in-suite all-template-function-tests)

(defparameter *sandbox-tests-directory* (directory-namestring #.(or *compile-file-truename*
                                                                    *load-truename*)))
(defparameter *glue-layer-tests-pathname* (merge-pathnames "glue-layer-tests.lisp" *sandbox-tests-directory*))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "template-function-tests"))))
  (run! 'all-template-function-tests)
  (evaluate-sandbox-tests-in-file *glue-layer-tests-pathname*))
