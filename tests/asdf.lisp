(in-package "TEMPLATE-FUNCTION.TESTS")
(in-suite all-template-function-tests)

(defparameter *sandbox-tests-directory* (directory-namestring #.(or *compile-file-truename*
                                                                    *load-truename*)))
(defparameter *glue-layer-tests-pathname* (merge-pathnames "glue-layer-tests.lisp" *sandbox-tests-directory*))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "template-function-tests"))))
  (run! 'all-template-function-tests)
  (evaluate-sandbox-tests-in-file *glue-layer-tests-pathname*))

(defun evaluate-test-with-name (test-type test-name)
  (labels ((predicate (type name)
             (and (string-equal test-type type)
                  (cond ((symbolp name)
                         (string-equal test-name name))
                        (t
                         (string-equal test-name (first name)))))))
    (evaluate-sandbox-tests-in-file-if #'predicate *glue-layer-tests-pathname*)))
