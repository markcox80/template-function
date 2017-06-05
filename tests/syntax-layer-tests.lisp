(syntax-layer-test basic/required
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun make-lambda-form (<x> <y> <alpha>)
      `(lambda (x y alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (defun make-function-type (x y alpha)
      `(function (,x ,y ,alpha) ,y)))

  (template-function:define-template xpy (x y alpha)
    (:lambda-form-function #'make-lambda-form)
    (:function-type-function #'make-function-type))

  (template-function:require-instantiations (xpy (array array real)
                                                 (list list real)))

  (test global-environment
    (let* ((array-name (compute-name 'xpy '(array array real)))
           (list-name (compute-name 'xpy '(list list real))))
      (is-true (fboundp array-name))
      (is-true (fboundp list-name))))

  (test usage
    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(7 8 9 10 11)))
      (is (equalp expected (xpy (the array x) (the array y) 2))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(7 8 9 10 11)))
      (is (equalp expected (xpy (the list x) (the list y) 2))))))
