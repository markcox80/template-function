(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "template-function"))

(defpackage "TUT3"
  (:use "COMMON-LISP"
        "TEMPLATE-FUNCTION"))
(in-package "TUT3")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-lambda-form (argspec)
     (destructuring-argument-specification (<array> &key ((:mean <mean>) 'number)) argspec
       `(lambda (array &key mean)
          (declare (type ,<array> array)
                   (type ,<mean> mean))
          (let* ((mean (coerce mean (array-element-type array))))
            (dotimes (i (array-total-size array))
              (setf (row-major-aref array i) (- (row-major-aref array i)
                                                mean))))
          (values)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-function-type (argspec)
    `(function ,argspec (values))))

(defun compute-mean (array)
  (let* ((count (array-total-size array)))
    (when (zerop count)
      (error "Invalid array."))
    (/ (loop
          for i from 0 below (array-total-size array)
          sum (row-major-aref array i))
        count)))

(define-template nremove-mean (array &key (mean (the number (compute-mean array))))
  (:lambda-form-function #'make-lambda-form)
  (:function-type-function #'make-function-type))

(require-instantiations (nremove-mean (array)
                                      ((simple-array double-float))))
