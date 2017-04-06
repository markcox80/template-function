(in-package "TEMPLATE-FUNCTION")

(defvar *type-name-pairs* nil
  "A list of (type . name) pairs.")

(defun type-name-pair-type (pair)
  (car pair))

(defun type-name-pair-name (pair)
  (cdr pair))

(defun (setf type-name-pair-name) (value pair)
  (setf (cdr pair) value))

(defun order-types-by-decreasing-specificity (types &key key)
  (sort types
        #'(lambda (a b)
            (and (subtypep a b)
                 (not (subtypep b a))))
        :key key))

(defun find-type-name-pair-with-type (type)
  (let* ((matches (remove-if-not #'(lambda (item)
                                     (subtypep type (type-name-pair-type item)))
                                 *type-name-pairs*)))
    (first (order-types-by-decreasing-specificity matches :key #'type-name-pair-type))))

(defun find-type-name-pair-with-name (name)
  (find name *type-name-pairs* :key #'type-name-pair-name :test #'string=))

(defun clear-type-name-pairs ()
  (setf *type-name-pairs* nil))

(defun add-type-name-pair (type name &key implementation-specific (value nil valuep))
  (when (and valuep implementation-specific (not (subtypep (type-of value) type)))
    (return-from add-type-name-pair (values)))

  (let* ((type (introspect-environment:typexpand type))
         (existing-pair (find-type-name-pair-with-type type))
         (type-equal (and (consp existing-pair)
                          (subtypep (type-name-pair-type existing-pair) type))))
    (unless type-equal
      (push (cons type name) *type-name-pairs*)))
  (values))

;;;; Defining type name pairs

(defmacro define-type-name-pairs (&body pairs)
  `(progn
     ,@(mapcar #'(lambda (sexp)
                   (destructuring-bind (type name &rest args &key implementation-specific value) sexp
                     (declare (ignore implementation-specific value))
                     `(add-type-name-pair ',type ,name ,@args)))
               pairs)))
