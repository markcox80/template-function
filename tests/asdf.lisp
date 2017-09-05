(in-package "TEMPLATE-FUNCTION.TESTS")
(in-suite all-template-function-tests)

(defparameter *sandbox-tests-directory* (directory-namestring #.(or *compile-file-truename*
                                                                    *load-truename*)))
(defparameter *glue-layer-tests-pathname* (merge-pathnames "glue-layer-tests.lisp" *sandbox-tests-directory*))
(defparameter *syntax-layer-tests-pathname* (merge-pathnames "syntax-layer-tests.lisp" *sandbox-tests-directory*))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "template-function-tests"))))
  (run! 'all-template-function-tests)
  (let* ((glue? (evaluate-sandbox-tests-in-file *glue-layer-tests-pathname*))
         (syntax? (evaluate-sandbox-tests-in-file *syntax-layer-tests-pathname*)))
    (if (and glue? syntax?)
        (format t "~&;;;; All sandbox tests passed.~%")
        (format t "~&;;;; Some sandbox tests failed.~%"))))

(defun evaluate-test-with-name (test-type test-name &key pathname (if-exists :error))
  (labels ((predicate (type name)
             (and (string-equal test-type type)
                  (cond ((symbolp name)
                         (string-equal test-name name))
                        (t
                         (string-equal test-name (first name)))))))
    (evaluate-sandbox-tests-in-file-if #'predicate *glue-layer-tests-pathname*
                                       :code-pathname pathname
                                       :if-code-exists if-exists)
    (evaluate-sandbox-tests-in-file-if #'predicate *syntax-layer-tests-pathname*
                                       :code-pathname pathname
                                       :if-code-exists if-exists)
    (values)))
