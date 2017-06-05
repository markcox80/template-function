(in-package "TEMPLATE-FUNCTION.TESTS")

;;;; For each GLUE-LAYER-TEST or SYNTAX-LAYER-TEST form in a file do
;;;; the following:
;;;;
;;;; 1. Delete the package TEMPLATE-FUNCTION.TESTS.SANDBOX.
;;;; 2. Create the package TEMPLATE-FUNCTION.TESTS.SANDBOX which
;;;;    uses COMMON-LISP, TEMPLATE-FUNCTION and FIVEAM. The symbol
;;;;    ALL-SANDBOX-TESTS should be exported.
;;;; 3. Write the following to a file:
;;;;    i. A defpackage form for TEMPLATE-FUNCTION.TESTS.SANDBOX which
;;;;       performs the same actions as step 2.
;;;;   ii. Create the test suite ALL-SANDBOX-TESTS.
;;;;  iii. Switch to the test suite ALL-SANDBOX-TESTS.
;;;;   iv. Write all forms inside the GLUE-LAYER-TEST or
;;;;       SYNTAX-LAYER-TEST form.
;;;;
;;;;   The *package* dynamic variable must be bound to the
;;;;   TEMPLATE-FUNCTION.TESTS.SANBOX package.
;;;;
;;;; 4. Delete the package TEMPLATE-FUNCTION.TESTS.SANDBOX.
;;;; 5. Compile the file.
;;;; 6. Load the file.
;;;; 7. Run the ALL-SANDBOX-TESTS test suite.

(defparameter *sandbox-package* "TEMPLATE-FUNCTION.TESTS.SANDBOX")
(defparameter *sandbox-package-use-list* '("COMMON-LISP" "TEMPLATE-FUNCTION" "FIVEAM"))

(defun output-sandbox-header (stream)
  (let* ((*standard-output* stream))
    (format t "~c~%" #\Page)
    (format t ";;;; Automatically generated sandbox header.~%")
    (pprint `(defpackage ,*sandbox-package*
              (:use ,@*sandbox-package-use-list*)
              (:export "ALL-SANDBOX-TESTS")))
    (terpri)
    (pprint `(in-package ,*sandbox-package*))
    (terpri)
    (format t "(fiveam:def-suite template-function.tests.sandbox:all-sandbox-tests)~%")
    (format t "(fiveam:in-suite template-function.tests.sandbox:all-sandbox-tests)~%")
    (format t "~%")
    (format t "~c~%" #\Page)
    (format t ";;;; Sandbox tests.~%")))

(defun output-sandbox-test (stream forms)
  (let* ((print-case *print-case*)
         (package *package*))
    (with-standard-io-syntax
      (let* ((*print-case* print-case)
             (*package* package))
        (output-sandbox-header stream)
        (let* ((*standard-output* stream))
          (dolist (form forms)
            (pprint form)
            (terpri)))))))

(defun do-with-temporary-directory (function)
  (assert (< (char-code #\A) (char-code #\Z)))
  (let* ((name (make-string 6))
         (min-code (char-code #\A))
         (max-code (char-code #\Z))
         (code-range (- max-code min-code)))
    (flet ((randomize-name ()
             (dotimes (i (length name))
               (let* ((ch (code-char (+ min-code (random code-range)))))
                 (setf (aref name i) (ecase (random 2)
                                       (0 (char-downcase ch))
                                       (1 ch)))))))
      (loop
        with directory-list = (append (pathname-directory (uiop:temporary-directory))
                                      (list name))
        for directory = (progn
                          (randomize-name)
                          (make-pathname :directory directory-list))
        for createdp = (nth-value 1 (ensure-directories-exist directory))
        until createdp
        finally
           (return
             (let* ((filter (merge-pathnames (make-pathname :name :wild
                                                            :type :wild
                                                            :directory '(:relative :wild-inferiors))
                                             directory)))
               (flet ((validate (pathname)
                        (or (pathname-match-p pathname filter)
                            (pathname-match-p pathname directory))))
                 (unwind-protect (funcall function directory)
                   (uiop:delete-directory-tree directory :validate #'validate)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-temporary-directory ((var) &body body)
    `(do-with-temporary-directory (lambda (,var)
                                    ,@body))))

(defun evaluate-sandbox-test (form &key pathname (if-exists :error))
  (flet ((compile-and-load (pathname)
           (with-temporary-directory (tmpdir)
             (multiple-value-bind (fasl-pathname warningsp failurep)
                 (compile-file pathname :output-file (make-pathname :name "sandbox" :type "fasl" :defaults tmpdir)
                                        :verbose t)
               (declare (ignore warningsp))
               (let* ((old-package *package*)
                      (old-name (package-name *package*))
                      (old-use-list (package-use-list old-package)))
                 (setf *package* (find-package "COMMON-LISP"))
                 (ignore-errors (delete-package old-package))
                 (setf *package* (make-package old-name :use old-use-list)))
               (unless failurep
                 (load fasl-pathname))))))
    (let* ((delete-pathname-p nil)
           (pathname (or pathname
                         (uiop:with-temporary-file (:pathname pathname :keep t :type "lisp")
                           (setf if-exists :supersede
                                 delete-pathname-p t)
                           pathname))))
      (unwind-protect
           (progn
             (destructuring-bind (test-type test-name . test-forms) form
               (with-open-file (stream pathname :direction :output :if-exists if-exists)
                 (output-sandbox-test stream test-forms))

               (format t "~&~c~%;;;; Sandbox test ~A: ~A~%" #\Page test-type test-name)
               (format t "~%;;; Compiling and loading~%")
               (cond ((compile-and-load pathname)
                      (format t "~&~%;;; Running sandbox test.~%")
                      (fiveam:run! (intern "ALL-SANDBOX-TESTS" *sandbox-package*)))
                     (t
                      (error "Unable to compile and load sandbox test (~A ~A)." test-type test-name)))))
        (when delete-pathname-p
          (ignore-errors (delete-file pathname)))))))

(defun evaluate-sandbox-tests-in-file-if (function pathname &key code-pathname (if-code-exists :error))
  (flet ((process (stream eof-value)
           (ignore-errors (delete-package *sandbox-package*))
           (unwind-protect (let* ((*package* (make-package *sandbox-package* :use *sandbox-package-use-list*))
                                  (form (read stream nil eof-value)))
                             (cond ((eql form eof-value)
                                    form)
                                   ((and (listp form)
                                         (>= (length form) 2)
                                         (symbolp (first form))
                                         (or (symbolp (second form))
                                             (listp (second form))))
                                    (when (funcall function (first form) (second form))
                                      (evaluate-sandbox-test form :pathname code-pathname :if-exists if-code-exists)))
                                   (t
                                    (error "Encountered an invalid sandbox test form:~%~A~%." form))))
             (delete-package *sandbox-package*))))
    (with-open-file (in pathname)
      (loop
        with eof-value = '#:eof
        for form = (process in eof-value)
        until (eql form eof-value)))))

(defun evaluate-sandbox-tests-in-file (pathname &key code-pathname (if-code-exists :error))
  (format t "~&~c~%;;;; Running sandbox tests in file: ~A~%" #\Page pathname)
  (evaluate-sandbox-tests-in-file-if (constantly t) pathname
                                     :code-pathname code-pathname
                                     :if-code-exists if-code-exists))
