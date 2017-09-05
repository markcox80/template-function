(syntax-layer-test basic/required
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<x> <y> <alpha>)
      `(lambda (x y alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (template-function:defun/argument-specification make-function-type (x y alpha)
      `(function (,x ,y ,alpha) ,y)))

  (template-function:define-template xpy (x y alpha)
    (:lambda-form-function #'make-lambda-form)
    (:function-type-function #'make-function-type))

  (template-function:require-instantiations (xpy (array array real)
                                                 (list list real)))

  (test global-environment
    (flet ((check (&rest types)
             (is-true (fboundp (template-function:compute-name 'xpy types)))))
      (check 'array 'array 'real)
      (check 'list 'list 'real))
    (is-true (fboundp 'xpy)))

  (test usage
    (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
           (y (make-array 5 :initial-contents '(5 4 3 2 1)))
           (expected #(7 8 9 10 11)))
      (is (equalp expected (xpy (the array x) (the array y) 2))))

    (let* ((x (list 1 2 3 4 5))
           (y (list 5 4 3 2 1))
           (expected '(7 8 9 10 11)))
      (is (equalp expected (xpy (the list x) (the list y) 2))))

    (let* ((x (make-array 3 :initial-contents '(1 2 3)))
           (y (list 4 5 6)))
      (signals error (xpy (the array x) (the list y) 1))
      (signals error (xpy x y 1))
      (signals error (xpy x))
      (signals error (xpy x x 1 2)))))

(syntax-layer-test basic/optional
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<x> <y> &optional (<alpha> 'number))
      `(lambda (x y alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (template-function:defun/argument-specification make-function-type (x y &optional (alpha 'number))
      `(function (,x ,y ,alpha) ,y))

    (flet ((compute-alpha (x y)
             (declare (ignore x y))
             1))
      (template-function:define-template xpy (x y &optional (alpha (compute-alpha x y)))
        (:lambda-form-function #'make-lambda-form)
        (:function-type-function #'make-function-type))))

  (template-function:require-instantiations (xpy (array array)
                                                 (array array real)
                                                 (list list)
                                                 (list list real)))

  (test global-environment
    (is-true (fboundp 'xpy))
    (flet ((check (&rest types)
             (is-true (fboundp (template-function:compute-name 'xpy types)))))
      (check 'array 'array)
      (check 'array 'array 'real)
      (check 'list 'list)
      (check 'list 'list 'real)))

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
    (template-function:defun/argument-specification make-lambda-form (<x> <y> &key ((:alpha <alpha>)))
      `(lambda (x y &key alpha)
         (check-type x ,<x>)
         (check-type y ,<y>)
         (check-type alpha ,<alpha>)
         (dotimes (i (min (length x) (length y)))
           (incf (elt y i) (* alpha (elt x i))))
         y))

    (template-function:defun/argument-specification make-function-type (x y &key (alpha 'number))
      `(function (,x ,y &key (:alpha ,alpha)) ,y))

    (flet ((compute-alpha ()
             1))
      (template-function:define-template xpy (x y &key (alpha (compute-alpha)))
        (:lambda-form-function #'make-lambda-form)
        (:function-type-function #'make-function-type))))

  (template-function:require-instantiations (xpy (array array)
                                                 (array array &key (:alpha real))
                                                 (list list)
                                                 (list list &key (:alpha real))))

  (test global-environment
    (is-true (fboundp 'xpy))
    (flet ((check (argument-specification)
             (is-true (fboundp (template-function:compute-name 'xpy argument-specification)))))
      (check '(array array))
      (check '(array array &key (:alpha real)))
      (check '(list list))
      (check '(list list &key (:alpha real)))))

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
    (template-function:defun/argument-specification make-lambda-form (&others <others> &rest <rest>)
      (let* ((vars (alexandria:make-gensym-list (length <others>)))
             (args (gensym "ARGS")))
        `(lambda (,@vars &rest ,args)
           ,@(loop
               for var in vars
               for <other> in <others>
               collect `(check-type ,var ,<other>))
           (dolist (arg ,args)
             (check-type arg ,<rest>))
           (reduce #'+ ,args :initial-value (+ ,@vars)))))

    (template-function:defun/argument-specification make-function-type (&others <others> &rest <args>)
      `(function (,@<others> &rest ,<args>) number))

    (template-function:define-template add (&rest args)
      (:lambda-form-function #'make-lambda-form)
      (:function-type-function #'make-function-type))

    (template-function:require-instantiations (add (double-float double-float &rest double-float))
                                              (add (integer integer integer &rest integer))))

  (test global-environment
    (is-true (fboundp 'add))
    (flet ((check (argument-specification)
             (is-true (fboundp (template-function:compute-name 'add argument-specification)))))
      (check '(double-float double-float &rest double-float))
      (check '(integer integer integer &rest integer))))

  (test usage
    (is (= 11d0 (add 5d0 6d0)))
    (is (= 1 (add 100 -50 -49)))
    (let* ((args '(5d0 6d0 7d0)))
      (is (= 18d0 (apply #'add args))))

    (signals error (add 1d0 2))
    (signals error (add 1 2 3 4d0))))

(syntax-layer-test inlining/required
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<input>)
      `(lambda (input)
         (declare (type ,<input> input))
         (1+ input)))

    (template-function:defun/argument-specification make-function-type (<input>)
      `(function (,<input>) number))

    (template-function:define-template example (input)
      (:lambda-form-function #'make-lambda-form)
      (:function-type-function #'make-function-type)
      (:inline t))

    (template-function:require-instantiation example (real)))

  (defun foo (a)
    (example a))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    (is (= 2 (example 1)))
    (is (= 3 (example 2)))))

(syntax-layer-test inlining/optional
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (&optional (<input> 'number))
      `(lambda (input)
         (declare (type ,<input> input))
         (1+ input)))

    (template-function:defun/argument-specification make-function-type (&optional (<input> 'number))
      `(function (,<input>) number))

    (let ((x 0))
      (flet ((compute-input ()
               (prog1 x
                 (incf x))))
        (template-function:define-template example (&optional (input (the number (compute-input))))
          (:lambda-form-function #'make-lambda-form)
          (:function-type-function #'make-function-type)
          (:inline t)))))

  (template-function:require-instantiation example (number))

  (defun foo (&optional (v nil vp))
    (if vp
        (example (the number v))
        (example)))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    ;; x is 0
    (is (= 1 (foo)))
    ;; x is 1
    (is (= 2 (foo)))
    ;; x is 2
    (is (= 10 (foo 9)))))

(syntax-layer-test inlining/keywords/sans-rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<a> &key ((:b <b>) 'number) ((:c <c>) 'number))
      `(lambda (a &key b c)
         (declare (type ,<a> a)
                  (type ,<b> b)
                  (type ,<c> c))
         (+ a b c)))

    (template-function:defun/argument-specification make-function-type (<a> &key ((:b <b>) 'number) ((:c <c>) 'number))
      `(function (,<a> &key (:b ,<b>) (:c ,<c>)) number))

    (let ((x 0))
      (flet ((compute ()
               (prog1 x
                 (incf x))))
        (template-function:define-template example (a &key (b (the number (compute))) (c (+ a b)))
          (:lambda-form-function #'make-lambda-form)
          (:function-type-function #'make-function-type)
          (:inline t))))

    (template-function:require-instantiation example (number)))

  (defun foo (a &optional (b nil) (c nil))
    (cond ((and b c)
           (example (the number a) :b (the number b) :c (the number c)))
          (b
           (example (the number a) :b (the number b)))
          (c
           (example (the number a) :c (the number c)))
          (t
           (example (the number a)))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo ()
    ;; x = 0
    (is (= 0 (foo 0)))
    ;; x = 1
    (is (= 2 (foo 0)))
    ;; x = 2
    (is (= 6 (foo 1 2)))
    (is (= 6 (foo 1 nil 3)))
    (is (= 33 (foo 10 11 12)))))

(syntax-layer-test inlining/keywords/with-rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<a> &key ((:b <b>) 'number) ((:c <c>) 'number) &allow-other-keys)
      `(lambda (a &key b c)
         (declare (type ,<a> a)
                  (type ,<b> b)
                  (type ,<c> c))
         (+ a b c)))

    (template-function:defun/argument-specification make-function-type (<a> &key ((:b <b>) 'number) ((:c <c>) 'number) &allow-other-keys)
      `(function (,<a> &key (:b ,<b>) (:c ,<c>)) number))

    (let ((x 0))
      (flet ((compute ()
               (prog1 x
                 (incf x))))
        (template-function:define-template example (a &rest args &key (b (the number (compute))) (c (the number (length args))))
          (:lambda-form-function #'make-lambda-form)
          (:function-type-function #'make-function-type)
          (:inline t))))

    (template-function:require-instantiation example (number)))

  (defun foo (a &optional b c)
    (cond ((and b c)
           (example (the number a) :b (the number b) :c (the number c)))
          (b
           (example (the number a) :b (the number b)))
          (t
           (example (the number a)))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    ;; x = 0
    (is (= 0 (foo 0)))
    ;; x = 1
    (is (= 1 (foo 0)))
    (is (= 5 (foo 1 2)))
    (is (= 12 (foo 3 4 5)))))

(syntax-layer-test inlining/rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (template-function:defun/argument-specification make-lambda-form (<a> &others <others> &rest <args>)
      (assert (null <others>))
      `(lambda (a &rest args)
         (reduce #'+ args :initial-value a)))

    (template-function:defun/argument-specification make-function-type (<a> &others <others> &rest <args>)
      (assert (null <others>))
      `(function (,<a> &rest ,<args>) number))

    (template-function:define-template example (a &rest args)
      (:lambda-form-function #'make-lambda-form)
      (:function-type-function #'make-function-type)
      (:inline t)))

  (require-instantiation example (number &rest number))

  (defun foo (a &optional b c)
    (cond ((and b c)
           (example (the number a) (the number b) (the number c)))
          (b
           (example (the number a) (the number b)))
          (t
           (example (the number a)))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    (is (= 1 (foo 1)))
    (is (= 5 (foo 3 2)))
    (is (= 6 (foo 3 2 1)))))
