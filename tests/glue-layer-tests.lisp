(glue-layer-test basic
  (eval-when (:compile-toplevel :load-toplevel)
    (defun example-function-type (x y alpha)
      `(function (,x ,y ,alpha) ,y))

    (defun example-lambda-form (<x> <y> <alpha>)
      `(lambda (x y alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (setf (elt y i) (+ (* alpha (elt x i))
                              (elt y i))))
         y))

    (template-function:ensure-template-function 'example '(x y alpha)
                                                :lambda-form-function #'example-lambda-form
                                                :function-type-function #'example-function-type)

    (template-function:ensure-instantiation 'example '(array array number)))

  (test example
    (let* ((x (make-array 5 :initial-element 1))
           (y (make-array 5 :initial-element 2)))
      (example (the array x) (the array y) 1)
      (is (equalp #(3 3 3 3 3) y)))))
