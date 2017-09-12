;;;; This file contains the code shown in tutorial 1.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "template-function"))

(defpackage "TUT1"
  (:use "COMMON-LISP"
        "TEMPLATE-FUNCTION"))
(in-package "TUT1")

(defun general-a-plus-b (c a b)
  (declare (optimize safety))
  (assert (= (array-total-size c)
             (array-total-size a)
             (array-total-size b)))
  (dotimes (i (array-total-size c))
    (setf (row-major-aref c i) (+ (row-major-aref a i)
                                  (row-major-aref b i))))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-a-plus-b-lambda-form (argument-specification)
    (destructuring-argument-specification (<c> <a> <b>) argument-specification
      `(lambda (c a b)
         (declare (optimize safety)
                  (type ,<c> c)
                  (type ,<a> a)
                  (type ,<b> b))
         #- (and)
         (print '(,<c> ,<a> ,<b>)) ;; For debugging
         (assert (= (array-total-size c)
                    (array-total-size a)
                    (array-total-size b)))
         (dotimes (i (array-total-size c))
           (setf (row-major-aref c i) (+ (row-major-aref a i)
                                         (row-major-aref b i))))
         (values)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-a-plus-b-function-type (argument-specification)
    `(function ,argument-specification (values))))

(define-template a-plus-b (c a b)
  (:lambda-form-function #'make-a-plus-b-lambda-form)
  (:function-type-function #'make-a-plus-b-function-type))

(require-instantiation a-plus-b (array array array))
(require-instantiations (a-plus-b ((simple-array double-float) (simple-array double-float) (simple-array double-float))
                                  ((simple-array single-float) (simple-array single-float) (simple-array single-float))))

(defun runtime-example (size element-type initial-a initial-b)
  (let* ((c (make-array size :element-type element-type))
         (a (make-array size :element-type element-type :initial-element initial-a))
         (b (make-array size :element-type element-type :initial-element initial-b)))
    (a-plus-b c a b)))

;; Runtime checks
(runtime-example 2 't 1 2)
(runtime-example 3 'double-float 1d0 2d0)
(runtime-example 4 'single-float 3s0 4s0)

;; Compile time
(let* ((fn (compiler-macro-function 'a-plus-b))
       (type '(simple-array double-float)))
  (pprint (funcall fn `(a-plus-b (the ,type c) (the ,type a) (the ,type b))
                  nil)))

;; Timing
(defun compute-timing ()
  (let* ((size 8000)
         (element-type 'double-float)
         (initial-a 1d0)
         (initial-b 2d0)
         (count 10000)
         (a (make-array size :element-type element-type :initial-element initial-a))
         (b (make-array size :element-type element-type :initial-element initial-b))
         (c (make-array size :element-type element-type)))
    (flet ((run-general ()
             (format t "~&General timing.~%")
             (time
               (dotimes (i count)
                 (general-a-plus-b c a b))))
           (run-runtime ()
             (format t "~&Runtime timing.~%")
             (time
               (dotimes (i count)
                 (a-plus-b c a b))))
           (run-compile-time ()
             (let* ((array-type `(simple-array ,element-type))
                    (fn (compile nil `(lambda (a b c)
                                        (dotimes (i ,count)
                                          (a-plus-b (the ,array-type a)
                                                    (the ,array-type b)
                                                    (the ,array-type c)))))))
               (format t "~&Compile time timing.~%")
               (time
                 (funcall fn a b c)))))
      (run-general)
      (run-runtime)
      (run-compile-time))))
