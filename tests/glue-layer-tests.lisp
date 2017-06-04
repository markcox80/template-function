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

(glue-layer-test recursion
  (eval-when (:compile-toplevel :load-toplevel)
    (defun random-product-lambda-form (<number> <limit>)
      `(lambda (start limit)
         (check-type start ,<number>)
         (check-type limit ,<limit>)
         (let* ((value (* start (random limit))))
           (ecase (random 2)
             (0 value)
             (1 (random-product (the real value) (the ,<limit> limit)))))))

    (defun random-product-function-type (number limit)
      `(function (,number ,limit) real))

    (template-function:ensure-template-function 'random-product '(number limit)
                                                :lambda-form-function #'random-product-lambda-form
                                                :function-type-function #'random-product-function-type)

    (template-function:ensure-instantiation* 'random-product 'integer 'integer))

  (test random-product
    ;; The ensure-instantiation* requests an instantiation for
    ;; arguments specialized to integer. This should produce the a
    ;; function with the name random-product/i_i. The call to
    ;; random-product within random-product-lambda-form should be
    ;; detected by the machinery and produce another instantiation
    ;; with the name random-product/r_i.
    (is-true (functionp (fdefinition 'random-product/i_i)))
    (is-true (functionp (fdefinition 'random-product/r_i)))
    (is-true (integerp (random-product 10 10)))))
