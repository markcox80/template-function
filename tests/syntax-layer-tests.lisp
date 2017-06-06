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

(syntax-layer-test basic/optional
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun make-lambda-form (<x> <y> &optional (<alpha> 'number))
      `(lambda (x y alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (defun make-function-type (x y &optional (alpha 'number))
      `(function (,x ,y &optional ,alpha) ,y))

    (defun make-type-completion-function (continuation)
      (lambda (x y &optional (alpha '(eql 1)))
        (funcall continuation x y alpha)))

    (flet ((compute-alpha (x y)
             (declare (ignore x y))
             1))
      (template-function:define-template xpy (x y &optional (alpha (compute-alpha x y)))
        (:lambda-form-function #'make-lambda-form)
        (:function-type-function #'make-function-type)
        (:type-completion-function #'make-type-completion-function))))

  (template-function:require-instantiations (xpy (array array)
                                                 (array array real)
                                                 (list list)
                                                 (list list real)))

  (test global-environment
    (let* ((array-name (compute-name 'xpy '(array array)))
           (array-alpha-name (compute-name 'xpy '(array array real)))
           (list-name (compute-name 'xpy '(list list)))
           (list-alpha-name (compute-name 'xpy '(list list real))))

      (is-true (fboundp array-name))
      (is-true (fboundp array-alpha-name))
      (is-true (fboundp list-name))
      (is-true (fboundp list-alpha-name))))

  (test usage
    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(6 6 6 6 6)))
      (is (equalp expected (xpy (the array x) (the array y)))))

    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(7 8 9 10 11)))
      (is (equalp expected (xpy (the array x) (the array y) 2))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(6 6 6 6 6)))
      (is (equalp expected (xpy (the list x) (the list y)))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(7 8 9 10 11)))
      (is (equalp expected (xpy (the list x) (the list y) 2))))))

(syntax-layer-test basic/keywords
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun make-lambda-form (<x> <y> &key ((:alpha <alpha>)))
      `(lambda (x y &key alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (defun make-function-type (x y &key (alpha 'number))
      `(function (,x ,y &key (:alpha ,alpha)) ,y))

    (defun make-type-completion-function (continuation)
      (lambda (x y &key (alpha '(eql 1)))
        (funcall continuation x y :alpha alpha)))

    (flet ((compute-alpha ()
             1))
      (template-function:define-template xpy (x y &key (alpha (compute-alpha)))
        (:lambda-form-function #'make-lambda-form)
        (:function-type-function #'make-function-type)
        (:type-completion-function #'make-type-completion-function))))

  (template-function:require-instantiations (xpy (array array)
                                                 (array array :alpha real)
                                                 (list list)
                                                 (list list :alpha real)))

  (test global-environment
    (let* ((array-name (compute-name 'xpy '(array array)))
           (array-alpha-name (compute-name 'xpy '(array array :alpha real)))
           (list-name (compute-name 'xpy '(list list)))
           (list-alpha-name (compute-name 'xpy '(list list :alpha real))))

      (is-true (fboundp array-name))
      (is-true (fboundp array-alpha-name))
      (is-true (fboundp list-name))
      (is-true (fboundp list-alpha-name))))

  (test usage
    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(6 6 6 6 6)))
      (is (equalp expected (xpy (the array x) (the array y)))))

    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(7 8 9 10 11)))
      (is (equalp expected (xpy (the array x) (the array y) :alpha 2))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(6 6 6 6 6)))
      (is (equalp expected (xpy (the list x) (the list y)))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(7 8 9 10 11)))
      (is (equalp expected (xpy (the list x) (the list y) :alpha 2))))

    (signals error (xpy #(1 2 3) (list 1 2 3))))

  (test other-keys
    (let* ((x (list 1 2 3))
           (y (list 4 5 6)))
      (signals error (xpy x y :gamma 1))
      (is (equal '(5 7 9) (xpy x y :gamma 1 :allow-other-keys t))))))

(syntax-layer-test basic/rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun make-lambda-form (&rest <args>)
      (let* ((vars (alexandria:make-gensym-list (length <args>))))
        `(lambda (,@vars)
           ,@(loop
               for var in vars
               for <arg> in <args>
               collect `(check-type ,var ,<arg>))
           (+ ,@vars))))

    (defun make-function-type (&rest args)
      `(function ,args number))

    (template-function:define-template add (&rest args)
      (:lambda-form-function #'make-lambda-form)
      (:function-type-function #'make-function-type))

    (template-function:require-instantiations (add (double-float double-float))
                                              (add (integer integer integer))))

  (test global-environment
    (is-true (fboundp (template-function:compute-name* 'add 'double-float 'double-float)))
    (is-true (fboundp (template-function:compute-name* 'add 'integer 'integer 'integer))))

  (test usage
    (is (= 11d0 (add 5d0 6d0)))
    (is (= 1 (add 100 -50 -49)))

    (signals error (add 1d0))
    (signals error (add 1 2 3 4))))
