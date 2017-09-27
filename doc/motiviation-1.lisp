(defpackage "MOTIVE1"
  (:use "COMMON-LISP")
  (:import-from "ALEXANDRIA"
                "ARRAY-INDEX"
                "ARRAY-LENGTH"))
(in-package "MOTIVE1")


;;;; Part 1

;; Solve the following objective:
;;
;;   arg min{x}  alpha || x ||_1 + (lambda/2) || m - x ||_2^2

(defun nsolve (alpha lambda m)
  (declare (type (real 0) alpha)
           (type (real (0)) lambda)
           (type array m))
  (let* ((element-type (array-element-type m))
         (zero (coerce 0 element-type))
         (pos-k (coerce (/ alpha lambda) element-type))
         (neg-k (- pos-k)))
    (dotimes (i (array-total-size m))
      (let* ((m-i (row-major-aref m i))
             (v-i (cond ((> m-i pos-k)
                         (- m-i pos-k))
                        ((< m-i neg-k)
                         (+ m-i neg-k))
                        (t
                         zero))))
        (setf (row-major-aref m i) v-i))))
  (values))

;;;; Part 2

(declaim (inline %nsolve))
(defun %nsolve (alpha lambda m)
  (declare (type (real (0)) alpha)
           (type (real (0)) lambda)
           (type array m))
  (let* ((element-type (array-element-type m))
         (zero (coerce 0 element-type))
         (pos-k (coerce (/ alpha lambda) element-type))
         (neg-k (- pos-k)))
    (dotimes (i (array-total-size m))
      (let* ((m-i (row-major-aref m i))
             (v-i (cond ((> m-i pos-k)
                         (- m-i pos-k))
                        ((< m-i neg-k)
                         (+ m-i neg-k))
                        (t
                         zero))))
        (setf (row-major-aref m i) v-i))))
  (values))

(defun nsolve/double-float (alpha lambda m)
  (declare (type double-float alpha lambda)
           (type (simple-array double-float (*)) m)
           (optimize speed))
  (%nsolve alpha lambda m))

(defun nsolve/single-float (alpha lambda m)
  (declare (type single-float alpha lambda)
           (type (simple-array single-float (*)) m)
           (optimize speed))
  (%nsolve alpha lambda m))

(defun nsolve/default (alpha lambda m)
  (declare (type real alpha lambda)
           (type array m))
  (%nsolve alpha lambda m))


;;;; Timer

(defun time-nsolve ()
  (let* ((element-type 'double-float)
         (magnitude (coerce 20 element-type))
         (dimensions 10000)
         (count 3000)
         (alpha 1d0)
         (lambda 1d0)
         (m (make-array dimensions :element-type element-type)))
    (dotimes (i (array-total-size m))
      (let* ((v-i (random magnitude)))
        (setf (row-major-aref m i) (ecase (random 2)
                                     (0 v-i)
                                     (1 (- v-i))))))
    (macrolet ((time-function (function-name)
                 `(progn
                    (format t "Timing: ~A" ',function-name)
                    (time
                      (dotimes (i count)
                        (,function-name alpha lambda m))))))
      (time-function nsolve)
      (time-function nsolve/double-float)
      (time-function nsolve/default))))
