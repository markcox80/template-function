(defpackage "TUT4"
  (:use "COMMON-LISP"
        "TEMPLATE-FUNCTION"))
(in-package "TUT4")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-compute-mean-lambda-form (argument-specification)
    (destructuring-argument-specification (<array>) argument-specification
      `(lambda (array)
         (declare (type ,<array> array))
         (let* ((mean (coerce 0 (array-element-type array)))
                (count (array-total-size array)))
           (when (zerop count)
             (error "Array length is zero."))
           (dotimes (i count)
             (incf mean (row-major-aref array i)))
           (/ mean count)))))

  (defun make-compute-mean-function-type (argument-specification)
    `(function ,argument-specification number)))

(define-template compute-mean (array)
  (:lambda-form-function #'make-compute-mean-lambda-form)
  (:function-type-function #'make-compute-mean-function-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-nremove-mean-lambda-form (argspec)
    (destructuring-argument-specification (<array> &key ((:mean <mean>))) argspec
      `(lambda (array &key mean)
         (declare (type ,<array> array)
                  (type (or null ,<mean>) mean))
         (let* ((mean (coerce (or mean (compute-mean (the ,<array> array)))
                              (array-element-type array))))
           (dotimes (i (array-total-size array))
             (decf (row-major-aref array i) mean)))
         (values))))

  (defun make-nremove-mean-function-type (argspec)
    `(function ,argspec (values))))

(define-template nremove-mean (array &key mean)
  (:lambda-form-function #'make-nremove-mean-lambda-form)
  (:function-type-function #'make-nremove-mean-function-type))

(require-instantiations (nremove-mean (array)
                                      ((simple-array double-float))
                                      ((simple-array single-float))))

(defun show-instantiations (template-function-name)
  (format t "~&Instantiations for ~A:" template-function-name)
  (loop
    for instantiation in (instantiations (find-template-function template-function-name))
    for index from 1
    do
       (format t "~&~d: ~A" index (instantiation-argument-specification instantiation)))
  (terpri))

(show-instantiations 'compute-mean)
(show-instantiations 'nremove-mean)
