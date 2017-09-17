;;;; This code provides support for destructuring the argument type
;;;; specification of a requested function type.
;;;;
;;;; Template functions generate specializations according to a user
;;;; supplied argument type specification. The set of allowable
;;;; specifications is a subset of the specifications allowed by the
;;;; function compound type specificer.
;;;;
;;;; The subset permitted is defined by the following syntax:
;;;;
;;;;   arg-typespec ::= (typespec*
;;;;                     [&rest typespec]
;;;;                     [&key (keyword typespec)*])
;;;;
;;;;   typespec -- a type specifier
;;;;   keyword -- a symbol.
;;;;
;;;; This file provides the following operators:
;;;;
;;;;   destructure-argument-specification
;;;;
;;;;     Destructuring an argument type specification according to a
;;;;     function argument lambda list supplied at runtime.
;;;;
;;;;   argument-specification-lambda
;;;;
;;;;     Destructuring an argument type specification according to
;;;;     a function argument lambda list supplied at runtime.
;;;;
;;;;   parse-lambda-list
;;;;
;;;;     Separate an argument specification lambda list in to its
;;;;     constituents.
;;;;
;;;; The definition of a function argument lambda list is provided in
;;;; the section on parse-lambda-list.

(in-package "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION")

;;;; parse-lambda-list
;;;;
;;;; Obtain information about the constitutents in an argument
;;;; specification lambda list (AS-lambda-list).
;;;;
;;;; An AS-lambda-list has the following syntax:
;;;;
;;;;   AS-lambda-list ::= (wholevar reqvars optionalvars othersvar restvar keyvars)
;;;;
;;;;   wholevar ::= [&whole var]
;;;;   reqvars ::= vars*
;;;;   optionalvars ::= [&optional {var | (var [init-form [supplied-p-parameter]])}]
;;;;   othersvar ::= [&others var]
;;;;   restvar ::= [&rest var]
;;;;   keyvars ::= [&key {var | ({var | (keyword var)} [init-form [supplied-p-parameter]])}
;;;;                     [&allow-other-keys]]

;;; Parameter protocol
(defgeneric paramater-var (parameter))
(defgeneric parameter-keyword (parameter))
(defgeneric parameter-init-form (parameter))
(defgeneric parameter-varp (parameter))

;;; Parameters protocol
(defgeneric lambda-list (parameters))
(defgeneric all-parameters (parameters))
(defgeneric whole-parameter (parameters))
(defgeneric required-parameters (parameters))
(defgeneric optional-parameters (parameters))
(defgeneric others-parameter (parameters))
(defgeneric rest-parameter (parameters))
(defgeneric keyword-parameters (parameters))
(defgeneric keyword-parameters-p (parameters))
(defgeneric allow-other-keywords-p (parameters))

(defclass lambda-list-parameters ()
  ((%lambda-list :initarg :lambda-list
                 :reader lambda-list)
   (%all-parameters :initarg :all-parameters
                    :reader all-parameters)
   (%whole-parameter :initarg :whole-parameter
                     :reader whole-parameter)
   (%required-parameters :initarg :required-parameters
                         :reader required-parameters)
   (%optional-parameters :initarg :optional-parameters
                         :reader optional-parameters)
   (%others-parameter :initarg :others-parameter
                      :reader others-parameter)
   (%rest-parameter :initarg :rest-parameter
                    :reader rest-parameter)
   (%keyword-parameters :initarg :keyword-parameters
                        :reader keyword-parameters)
   (%keyword-parameters-p :initarg :keyword-parameters-p
                          :reader keyword-parameters-p)
   (%allow-other-keywords-p :initarg :allow-other-keywords
                            :reader allow-other-keywords-p)))

(defmethod print-object ((object lambda-list-parameters) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (lambda-list object) :stream stream)))

(deftype variable-name ()
  '(and symbol
        (not null)
        (not (member &whole &optional &others &rest &key &allow-other-keys))))

(defun variable-name-p (object)
  (typep object 'variable-name))

(deftype keyword-name ()
  '(and symbol (not null)))

(defun keyword-name-p (object)
  (typep object 'keyword-name))

(defclass parameter ()
  ((%var :initarg :var
         :type variable-name
         :reader parameter-var)))

(defclass whole-parameter (parameter)
  ())

(defclass required-parameter (parameter)
  ())

(defclass optional-parameter (parameter)
  ((%init-form :initarg :init-form
               :reader parameter-init-form)
   (%varp :initarg :varp
          :type (or variable-name null)
          :reader parameter-varp)))

(defclass others-parameter (parameter)
  ())

(defclass rest-parameter (parameter)
  ())

(defclass keyword-parameter (parameter)
  ((%keyword :initarg :keyword
             :reader parameter-keyword)
   (%init-form :initarg :init-form
               :reader parameter-init-form)
   (%varp :initarg :varp
          :type (or variable-name null)
          :reader parameter-varp)))

(defmethod print-object ((object parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (parameter-var object) :stream stream)))

(defmethod print-object ((object optional-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~W ~W ~W"
            (parameter-var object)
            (parameter-init-form object)
            (parameter-varp object))))

(defmethod print-object ((object keyword-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~W ~W ~W ~W"
            (parameter-keyword object)
            (parameter-var object)
            (parameter-init-form object)
            (parameter-varp object))))

(defun make-whole-parameter (var)
  (check-type var variable-name)
  (make-instance 'whole-parameter :var var))

(defun make-required-parameter (var)
  (check-type var variable-name)
  (make-instance 'required-parameter :var var))

(defun make-optional-parameter (var &optional (init-form t) varp)
  (check-type var variable-name)
  (check-type varp (or null variable-name))
  (make-instance 'optional-parameter :var var
                                     :init-form init-form
                                     :varp varp))

(defun make-others-parameter (var)
  (check-type var variable-name)
  (make-instance 'others-parameter :var var))

(defun make-rest-parameter (var)
  (check-type var variable-name)
  (make-instance 'rest-parameter :var var))

(defun make-keyword-parameter (var &optional (init-form t) varp keyword)
  (check-type var variable-name)
  (check-type varp (or null variable-name))
  (check-type keyword (or null keyword-name))
  (make-instance 'keyword-parameter :keyword (or keyword (intern (symbol-name var) "KEYWORD"))
                                    :var var
                                    :init-form init-form
                                    :varp varp))

(defun parameterp (object)
  (typep object 'parameter))

(defun whole-parameter-p (object)
  (typep object 'whole-parameter))

(defun required-parameter-p (object)
  (typep object 'required-parameter))

(defun optional-parameter-p (object)
  (typep object 'optional-parameter))

(defun others-parameter-p (object)
  (typep object 'others-parameter))

(defun rest-parameter-p (object)
  (typep object 'rest-parameter))

(defun keyword-parameter-p (object)
  (typep object 'keyword-parameter))

(define-condition parse-lambda-list-error (error)
  ((lambda-list :initarg :lambda-list
                :reader parse-lambda-list-error-lambda-list)
   (message :initarg :message
            :reader parse-lambda-list-error-message))
  (:report (lambda (condition stream)
             (write-string (parse-lambda-list-error-message condition)
                           stream))))

(defun signal-parse-lambda-list-error (as-lambda-list format-control &rest format-arguments)
  (check-type as-lambda-list list)
  (check-type format-control string)
  (let* ((message (apply #'format nil format-control format-arguments)))
    (error 'parse-lambda-list-error
           :lambda-list as-lambda-list
           :message message)))

(defun %signal-pas-error (as-lambda-list format-control &rest format-arguments)
  (apply #'signal-parse-lambda-list-error
         as-lambda-list
         format-control
         format-arguments))

(defun %signal-pas-invalid-keyword-error (keyword as-lambda-list)
  (%signal-pas-error as-lambda-list
                     "The ~A lambda list keyword appears in an invalid location in the argument argument specification lambda list ~A."
                     keyword as-lambda-list))

(defun %signal-pas-syntax-error (object as-lambda-list)
  (%signal-pas-error as-lambda-list
                     "Invalid object ~A found in the argument specification lambda list ~A."
                     object as-lambda-list))

(defun %signal-others-and-keys-error (as-lambda-list)
  (%signal-pas-error as-lambda-list
                     "Cannot use &others lambda list keyword alongside the &key lambda list keyword in ~A."
                     as-lambda-list))

(defun %signal-others-and-no-rest-error (as-lambda-list)
  (%signal-pas-error as-lambda-list
                     "Cannot use &others lambda list keyword without the &rest lambda list keyword in ~A."
                     as-lambda-list))

(defun %signal-no-others-and-rest-error (as-lambda-list)
  (%signal-pas-error as-lambda-list
                     "Cannot use &rest lambda list keyword without the &others lambda list keyword in ~A."
                     as-lambda-list))

(define-condition duplicate-variable-error (parse-lambda-list-error)
  ((variable :initarg :variable
             :reader duplicate-variable-error-variable)))

(defun signal-duplicate-variable-error (variable lambda-list)
  (check-type variable variable-name)
  (error 'duplicate-variable-error
         :lambda-list lambda-list
         :message (format nil "The variable ~W occurs more than once in the lambda list ~A."
                          variable lambda-list)
         :variable variable))

(defun parse-lambda-list/whole (as-lambda-list list)
  (check-type list list)
  (cond ((null list)
         (values nil nil))
        ((member (first list) '(&allow-other-keys))
         (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
        ((or (member (first list) '(&optional &others &rest &key))
             (variable-name-p (first list)))
         (values nil list))
        ((and (eql '&whole (first list))
              (variable-name-p (second list)))
         (values (make-instance 'whole-parameter :var (second list))
                 (nthcdr 2 list)))
        (t
         (%signal-pas-syntax-error (first list) as-lambda-list))))

(defun parse-lambda-list/required (as-lambda-list list)
  (cond ((null list)
         (values nil nil))
        ((member (first list) '(&allow-other-keys &whole))
         (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
        ((member (first list) '(&optional &others &rest &key))
         (values nil list))
        ((variable-name-p (first list))
         (multiple-value-bind (others next-list) (parse-lambda-list/required as-lambda-list (rest list))
           (values (cons (make-required-parameter (first list))
                         others)
                   next-list)))
        (t
         (%signal-pas-syntax-error (first list) as-lambda-list))))

(defun parse-lambda-list/optional (as-lambda-list list)
  (labels ((process (list)
             (cond ((null list)
                    (values nil nil))
                   ((member (first list) '(&optional &whole &allow-other-keys))
                    (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
                   ((member (first list) '(&others &rest &key))
                    (values nil list))
                   ((variable-name-p (first list))
                    (multiple-value-bind (others next-list) (process (rest list))
                      (values (cons (make-optional-parameter (first list))
                                    others)
                              next-list)))
                   ((let* ((tmp (first list)))
                      (and (listp tmp)
                           (<= 1 (length tmp) 3)
                           (variable-name-p (first tmp))
                           (or (null (third tmp))
                               (variable-name-p (third tmp)))))
                    (multiple-value-bind (others next-list) (process (rest list))
                      (values (cons (apply #'make-optional-parameter (first list))
                                    others)
                              next-list)))
                   (t
                    (%signal-pas-syntax-error (first list) as-lambda-list)))))
    (cond ((eql '&optional (first list))
           (process (rest list)))
          (t
           (values nil list)))))

(defun parse-lambda-list/others (as-lambda-list list)
  (cond ((null list)
         (values nil nil))
        ((member (first list) '(&optional &allow-other-keys &whole))
         (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
        ((member (first list) '(&rest &key))
         (values nil list))
        ((and (eql '&others (first list))
              (variable-name-p (second list)))
         (values (make-others-parameter (second list))
                 (nthcdr 2 list)))
        (t
         (%signal-pas-syntax-error (first list) as-lambda-list))))

(defun parse-lambda-list/rest (as-lambda-list list)
  (cond ((null list)
         (values nil nil))
        ((member (first list) '(&optional &allow-other-keys &whole &others))
         (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
        ((member (first list) '(&key))
         (values nil list))
        ((and (eql '&rest (first list))
              (variable-name-p (second list)))
         (values (make-rest-parameter (second list))
                 (nthcdr 2 list)))
        (t
         (%signal-pas-syntax-error (first list) as-lambda-list))))

(defun parse-lambda-list/keys (as-lambda-list full-list)
  (labels ((process-keyword-list (parameter rest-list keys)
             (assert (and parameter (listp parameter)))
             (destructuring-bind (item &optional (init-form t) varp) parameter
               (when (and varp (not (variable-name-p varp)))
                 (%signal-pas-error as-lambda-list
                                    "Invalid varp name in keyword parameter specification ~A."
                                    varp))
               (let* ((item (cond ((variable-name-p item)
                                   (list (intern (symbol-name item) "KEYWORD")
                                         item))
                                  ((and (listp item)
                                        (keyword-name-p (first item))
                                        (variable-name-p (second item)))
                                   item)
                                  (t
                                   (%signal-pas-error as-lambda-list
                                                      "Invalid keyword parameter specification ~A in ~A."
                                                      parameter
                                                      as-lambda-list)))))
                 (destructuring-bind (keyword var) item
                   (process rest-list
                            (cons (make-keyword-parameter var init-form varp keyword)
                                  keys))))))
           (process (list keys)
             (cond ((null list)
                    (values keys nil nil))
                   ((and (eql '&allow-other-keys (first list))
                         (null (rest list)))
                    (values keys t (rest list)))
                   ((member (first list) '(&optional &allow-other-keys &whole &others &rest &key))
                    (%signal-pas-invalid-keyword-error (first list) as-lambda-list))
                   ((variable-name-p (first list))
                    (process (rest list)
                             (cons (make-keyword-parameter (first list))
                                   keys)))
                   ((listp (first list))
                    (process-keyword-list (first list) (rest list) keys))
                   (t
                    (%signal-pas-syntax-error (first list) as-lambda-list)))))
    (cond ((null full-list)
           (values (list nil nil nil)
                   full-list))
          ((eql '&key (first full-list))
           (multiple-value-bind (keys othersp new-list) (process (rest full-list) nil)
             (values (list t (reverse keys) othersp)
                     new-list)))
          (t
           (%signal-pas-syntax-error (first full-list) as-lambda-list)))))

(defun check-duplicate-variables (parameters lambda-list)
  (let (visited)
    (flet ((check-visited (&rest vars)
             (dolist (var vars)
               (unless (null var)
                 (when (find var visited)
                   (signal-duplicate-variable-error var lambda-list))
                 (push var visited)))))
      (dolist (parameter parameters)
        (etypecase parameter
          ((or keyword-parameter optional-parameter)
           (check-visited (parameter-var parameter)
                          (parameter-varp parameter)))
          (parameter
           (check-visited (parameter-var parameter))))))))

(defun parse-lambda-list (as-lambda-list)
  (check-type as-lambda-list list)
  (labels ((process (list parser others)
             (cond ((and list (null parser))
                    (%signal-pas-syntax-error (first list) as-lambda-list))
                   (parser
                    (multiple-value-bind (result next-list) (funcall parser as-lambda-list list)
                      (cons result (process next-list (first others) (rest others)))))
                   (t
                    nil)))
           (ensure-list (object)
             (when object
               (list object))))
    (destructuring-bind (whole required optional others rest keys-tuple)
        (process as-lambda-list
                 #'parse-lambda-list/whole
                 (list #'parse-lambda-list/required
                       #'parse-lambda-list/optional
                       #'parse-lambda-list/others
                       #'parse-lambda-list/rest
                       #'parse-lambda-list/keys))
      (destructuring-bind (keysp keys other-keys-p) keys-tuple
        (when (and others keysp)
          (%signal-others-and-keys-error as-lambda-list))
        (when (and others (not rest))
          (%signal-others-and-no-rest-error as-lambda-list))
        (when (and (not others) rest (not keysp))
          (%signal-no-others-and-rest-error as-lambda-list))
        (let* ((all-parameters (append (ensure-list whole)
                                       required
                                       optional
                                       (ensure-list others)
                                       (ensure-list rest)
                                       keys)))
          (check-duplicate-variables all-parameters as-lambda-list)
          (make-instance 'lambda-list-parameters
                         :all-parameters all-parameters
                         :lambda-list as-lambda-list
                         :whole-parameter whole
                         :required-parameters required
                         :optional-parameters optional
                         :others-parameter others
                         :rest-parameter rest
                         :keyword-parameters-p keysp
                         :keyword-parameters keys
                         :allow-other-keywords other-keys-p))))))

;;;; argument-specification-lambda
;;;;
;;;; The argument-specification-lambda macro provides syntactic
;;;; support for destructuring argument type specifications which are
;;;; congruent with a argument specification lambda list.

(define-condition argument-specification-lambda-error (error)
  ((as-lambda-list :initarg :as-lambda-list
                   :reader argument-specification-lambda-error-as-lambda-list)
   (arg-spec :initarg :argument-specification
             :reader argument-specification-lambda-error-argument-specification)
   (message :initarg :message
            :reader argument-specification-lambda-error-message))
  (:report (lambda (condition stream)
             (write-string (argument-specification-lambda-error-message condition)
                           stream))))

(defun signal-argument-specification-lambda-error (arg-spec as-lambda-list format-control &rest format-arguments)
  (error 'argument-specification-lambda-error
         :as-lambda-list as-lambda-list
         :argument-specification arg-spec
         :message (apply #'format nil format-control format-arguments)))

(defun signal-too-few-required-values-error (arg-spec as-lambda-list)
  (signal-argument-specification-lambda-error
   arg-spec as-lambda-list
   "Missing required values in ~W for argument specification lambda list ~W."
   arg-spec as-lambda-list))

(defun signal-too-many-required-values-error (arg-spec as-lambda-list)
  (signal-argument-specification-lambda-error
   arg-spec as-lambda-list
   "Too many required values in ~A for argument specification lambda list ~W."
   arg-spec as-lambda-list))

(defun signal-invalid-argument-specification-error (arg-spec as-lambda-list)
  (signal-argument-specification-lambda-error
   arg-spec as-lambda-list
   "Invalid argument specification ~A for argument specification lambda list ~W."
   arg-spec as-lambda-list))

(defun signal-malformed-argument-specification-error (arg-spec as-lambda-list)
  (signal-argument-specification-lambda-error
   arg-spec as-lambda-list
   "Argument specification ~A is invalid."
   arg-spec))

;; arg-spec must contain X required values such that min-count <=
;; X <= max-count.
(defun %asl/check-positional (arg-spec min-count max-count as-lambda-list)
  (let* ((count (loop
                  for item in arg-spec
                  when (member item '(&optional &rest &key))
                    do
                       (signal-invalid-argument-specification-error arg-spec as-lambda-list)
                  count item)))
    (when (< count min-count)
      (signal-too-few-required-values-error arg-spec as-lambda-list))
    (when (> count max-count)
      (signal-too-many-required-values-error arg-spec as-lambda-list))
    arg-spec))

;; arg-spec must contain at least min-count required values. The
;; argument specification can contain other required values prior to
;; an optional &rest specification. The other required values will be
;; assigned to optional arguments or collated for the other
;; argument.
(defun %asl/check-rest-and-others (arg-spec min-count max-count as-lambda-list)
  (loop
    for sub-spec on arg-spec
    for item = (first sub-spec)
    until (eql item '&rest)
    when (member item '(&optional &key &allow-other-keys))
      do (signal-invalid-argument-specification-error arg-spec as-lambda-list)
    count item into positional-count
    collect item into positional
    finally
       (when (< positional-count min-count)
         (signal-too-few-required-values-error arg-spec as-lambda-list))
       (when (and (eql item '&rest)
                  (or (< positional-count max-count)
                      (not (consp (cdr sub-spec)))
                      (not (null (cddr sub-spec)))
                      (not (variable-name-p (second sub-spec)))))
         (signal-invalid-argument-specification-error arg-spec as-lambda-list))
       (return (values positional (second sub-spec)))))

;; arg-spec must contain X required values such that min-count <= X <=
;; max-count and possibly a keyword section.
(defun %asl/check-keys (arg-spec min-count max-count as-lambda-list keywords storage allow-other-keys)
  (loop
    for sub-arg-spec on arg-spec
    for item = (first sub-arg-spec)
    when (member item '(&optional &rest &allow-other-keys))
      do (signal-invalid-argument-specification-error arg-spec as-lambda-list)
    until (eql item '&key)
    count item into positional-count
    finally
       (when (< positional-count min-count)
         (signal-too-few-required-values-error arg-spec as-lambda-list))
       (when (> positional-count max-count)
         (signal-too-many-required-values-error arg-spec as-lambda-list))
       (when (and (eql item '&key)
                  (< positional-count max-count))
         (signal-invalid-argument-specification-error arg-spec as-lambda-list))
       (loop
         for item in (rest sub-arg-spec)
         unless (and (consp item)
                     (consp (cdr item))
                     (null (cddr item))
                     (keyword-name-p (first item)))
           do (signal-malformed-argument-specification-error arg-spec as-lambda-list)
         do
            (let* ((pos (position (first item) keywords)))
              (cond (pos
                     (setf (aref storage pos) (rest item)))
                    ((not allow-other-keys)
                     (signal-invalid-argument-specification-error arg-spec as-lambda-list)))))))

(defmacro argument-specification-lambda (as-lambda-list &body body)
  (let* ((parameters (parse-lambda-list as-lambda-list))
         (whole (whole-parameter parameters))
         (arg-spec (gensym "ARGUMENT-SPECIFICATION"))
         (pos-var (gensym "POSITIONAL"))
         (whole-let (when whole
                      (list `(,(parameter-var whole) ,arg-spec))))
         (required (required-parameters parameters))
         (required-count (length required))
         (required-let (loop
                         for r in (required-parameters parameters)
                         for var = (parameter-var r)
                         collect `(,var (pop ,pos-var))))
         (optional (optional-parameters parameters))
         (optional-count (length optional))
         (optional-let (loop
                         for o in optional
                         for var = (parameter-var o)
                         for init-form = (parameter-init-form o)
                         for varp = (parameter-varp o)
                         for tmp = (gensym "TMP")
                         append `((,tmp (and ,pos-var t))
                                  (,var (if ,tmp
                                            (pop ,pos-var)
                                            ,init-form)))
                         when varp
                           collect `(,varp ,tmp)))
         (positional-count (+ required-count optional-count))
         (positional-let (append required-let optional-let))
         (others (others-parameter parameters))
         (rest (rest-parameter parameters))
         (keywords? (keyword-parameters-p parameters))
         (keywords (keyword-parameters parameters))
         (allow-other-keywords-p (allow-other-keywords-p parameters)))
    (cond ((and (null others) (null rest) (not keywords?))
           `(lambda (,arg-spec)
              (let* (,@whole-let
                     (,pos-var (%asl/check-positional ,arg-spec ,required-count ,positional-count ',as-lambda-list))
                     ,@positional-let)
                ,@body)))
          ((and others rest (not keywords?))
           (let* ((tmp-rest (gensym "REST"))
                  (rest-var (parameter-var rest))
                  (others-var (parameter-var others)))
             `(lambda (,arg-spec)
                (multiple-value-bind (,pos-var ,tmp-rest)
                    (%asl/check-rest-and-others ,arg-spec ,required-count ,positional-count ',as-lambda-list)
                  (let* (,@whole-let
                         ,@positional-let
                         (,others-var ,pos-var)
                         (,rest-var ,tmp-rest))
                    ,@body)))))
          (keywords?
           (let* ((keyword-count (length keywords))
                  (keyword-symbols (mapcar #'parameter-keyword keywords)))
             (alexandria:with-gensyms (storage)
               `(lambda (,arg-spec)
                  (let* ((,storage (make-array ,keyword-count :initial-element 0)))
                    (declare (dynamic-extent ,storage))
                    (%asl/check-keys ,arg-spec
                                     ,required-count ,positional-count
                                     ',as-lambda-list ',keyword-symbols
                                     ,storage ,allow-other-keywords-p)
                    (let* (,@whole-let
                           (,pos-var ,arg-spec)
                           ,@positional-let
                           ,@(loop
                               for keyword in keywords
                               for pos from 0
                               for var = (parameter-var keyword)
                               for init-form = (parameter-init-form keyword)
                               for tmp = (gensym (symbol-name var))
                               for varp = (parameter-varp keyword)
                               append `((,tmp (aref ,storage ,pos))
                                        (,var (if (eql ,tmp 0)
                                                  ,init-form
                                                  (car ,tmp))))
                               if varp
                                 collect `(,varp (not (eql ,tmp 0)))))
                      ,@body))))))
          (t
           (error "Do not know how to process this argument specification lambda list ~A." as-lambda-list)))))

(defmacro named-argument-specification-lambda (name lambda-list &body body)
  (let* ((lambda-form (macroexpand-1 `(argument-specification-lambda ,lambda-list
                                        ,@body))))
    `(flet ((,name ,(second lambda-form)
              ,@(nthcdr 2 lambda-form)))
       (function ,name))))

(defmacro defun/argument-specification (name lambda-list &body body)
  (let* ((lambda-form (macroexpand-1 `(argument-specification-lambda ,lambda-list
                                        ,@body))))
    `(defun ,name ,(second lambda-form)
       ,@(nthcdr 2 lambda-form))))

(defmacro destructuring-argument-specification (lambda-list argument-specification &body body)
  `(funcall (argument-specification-lambda ,lambda-list
              ,@body)
            ,argument-specification))
