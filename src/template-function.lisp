(in-package "TEMPLATE-FUNCTION")

;;;; Instantiation state

;; This dynamic variable will be set to T if a template is being
;; instantiated.
(defvar *instantiating* nil)

(declaim (inline instantiatingp))
(defun instantiatingp ()
  *instantiating*)

;; Template functions encountered during processing. Each item must be
;; an instance of RECORD.
(defvar *records*)

(defstruct record
  template-function
  argument-specification)

(defun note-template-function (template-function argument-specification)
  (push (make-record :template-function template-function
                     :argument-specification argument-specification)
        *records*))

;; Template functions that have been processed i.e. the template has
;; been instantiated and inserted in to the global environment. Each
;; item must be an instance of instantiation.
(defvar *processed*)

(defgeneric instantiation-name (instantiation))
(defgeneric instantiation-argument-specification (instantiation))
(defgeneric instantiation-function-type (instantiation))
(defgeneric instantiation-lambda-form (instantiation))
(defgeneric instantiation-function (instantiation))
(defgeneric instantiation-expand-function (instantiation))

(defclass instantiation ()
  ((%argument-specification :initarg :argument-specification
                            :reader instantiation-argument-specification)
   (%lambda-form :initarg :lambda-form
                 :reader instantiation-lambda-form)
   (%function-type :initarg :function-type
                   :reader instantiation-function-type)
   (%name :initarg :name
          :reader instantiation-name)
   (%function :initarg :function
              :reader instantiation-function)

   ;; This is here to help processing instantiations.
   (%template-function :initarg :template-function
                       :reader instantiation-template-function)))

(defun make-instantiation (&key
                             argument-specification lambda-form function-type
                             name function template-function)
  (make-instance 'instantiation
                 :argument-specification argument-specification
                 :lambda-form lambda-form
                 :function-type function-type
                 :name name
                 :function function
                 :template-function template-function))

(defun processed-template-function-p (template-function argument-specification)
  (find-if #'(lambda (instantiation)
               (and (eql (instantiation-template-function instantiation)
                         template-function)
                    (equalp (instantiation-argument-specification instantiation)
                            argument-specification)))
           *processed*))

(defun processed-record-p (record)
  (processed-template-function-p (record-template-function record)
                                 (record-argument-specification record)))

;;;; Helpers

(defun store-parameters-as-arg-spec-lambda-list (parameters)
  (check-type parameters specialization-store.lambda-lists:parameters)
  (let* ((required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         (keys? (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (keys (specialization-store.lambda-lists:keyword-parameters parameters))
         (others? (specialization-store.lambda-lists:allow-other-keys-p parameters)))
    (append (loop
              for p in (append required optional)
              collect (specialization-store.lambda-lists:parameter-var p))
            (when (and rest (not keys?))
              (list '&others (gensym "OTHERS")
                    '&rest (specialization-store.lambda-lists:parameter-var rest)))
            (when keys?
              (append (list '&key)
                      (loop
                        for p in keys
                        for keyword = (specialization-store.lambda-lists:parameter-keyword p)
                        for var = (specialization-store.lambda-lists:parameter-var p)
                        for init-form = (specialization-store.lambda-lists:parameter-init-form p)
                        for init-type = (specialization-store:determine-form-value-type init-form nil)
                        collect `((,keyword ,var) ,init-type))
                      (when others?
                        '(&allow-other-keys)))))))

(defun store-parameters-as-arg-spec-parameters (parameters)
  (let* ((lambda-list (store-parameters-as-arg-spec-lambda-list parameters)))
    (template-function.argument-specification:parse-lambda-list lambda-list)))

(defun make-specialization-lambda-list-lambda-form (parameters)
  ;; Create a specialization lambda list for a given
  ;; argument-specification.
  (check-type parameters template-function.argument-specification:lambda-list-parameters)
  (let* ((required (template-function.argument-specification:required-parameters parameters))
         (others (template-function.argument-specification:others-parameter parameters))
         (rest (template-function.argument-specification:rest-parameter parameters))
         (keys? (template-function.argument-specification:keyword-parameters-p parameters))
         (keywords (template-function.argument-specification:keyword-parameters parameters))
         (allow-other-keys? (template-function.argument-specification:allow-other-keywords-p parameters))
         (req-vars (mapcar #'template-function.argument-specification:parameter-var required))
         (req-section (loop
                        for var in req-vars
                        collect `(list ',var ,var))))
    (macroexpand-1
     (cond ((and (not others) (not rest) (not keys?))
            `(template-function:argument-specification-lambda (,@req-vars)
               (list ,@req-section)))
           ((and (not others) rest (not keys?))
            (let* ((rest-var (template-function.argument-specification:parameter-var rest)))
              `(template-function:argument-specification-lambda (,@req-vars &rest ,rest-var)
                 (list ,@req-section '&rest (list ',rest-var ,rest-var)))))
           ((and others rest (not keys?))
            (let* ((others-var (template-function.argument-specification:parameter-var others))
                   (rest-var (template-function.argument-specification:parameter-var rest)))
              (alexandria:with-gensyms (o-vars)
                `(template-function:argument-specification-lambda (,@req-vars &others ,others-var &rest ,rest-var)
                   (let* ((,o-vars (alexandria:make-gensym-list (length ,others-var))))
                     (append (list ,@req-section)
                             (mapcar #'(lambda (name type)
                                         (list name type))
                                     ,o-vars ,others-var)
                             (list '&rest (list ',rest-var ,rest-var))))))))
           ((and (not others) (not rest) keys?)
            (let* ((keys-arguments (loop
                                     for p in keywords
                                     for keyword = (template-function.argument-specification:parameter-keyword p)
                                     for var = (template-function.argument-specification:parameter-var p)
                                     for init-type = (template-function.argument-specification:parameter-init-form p)
                                     collect `((,keyword ,var) ',init-type)))
                   (other-keywords (when allow-other-keys?
                                     '(&allow-other-keys)))
                   (keys-section (loop
                                   for p in keywords
                                   for keyword = (template-function.argument-specification:parameter-keyword p)
                                   for var = (template-function.argument-specification:parameter-var p)
                                   collect `(list (list ',keyword ',var) ,var))))
              `(template-function:argument-specification-lambda (,@req-vars &key ,@keys-arguments ,@other-keywords)
                 (append (list ,@req-section)
                         (list '&key ,@keys-section)))))
           (t
            (error "Do not know how to process argument specification parameters ~A." parameters))))))

(defun make-name-lambda-form (prefix parameters)
  ;; Create a lambda form which accepts an argument-specification and
  ;; returns a unique name.
  (check-type prefix symbol)
  (check-type parameters template-function.argument-specification:lambda-list-parameters)
  (let* ((required (template-function.argument-specification:required-parameters parameters))
         (others (template-function.argument-specification:others-parameter parameters))
         (rest (template-function.argument-specification:rest-parameter parameters))
         (keys? (template-function.argument-specification:keyword-parameters-p parameters))
         (keywords (template-function.argument-specification:keyword-parameters parameters))
         (allow-other-keys? (template-function.argument-specification:allow-other-keywords-p parameters))
         (req-vars (mapcar #'template-function.argument-specification:parameter-var required)))
    (macroexpand-1
     (cond ((and (not others) (not rest) (not keys?))
            `(template-function.argument-specification:argument-specification-lambda (,@req-vars)
               (name-for-types ',prefix ,@req-vars)))

           ((and (not others) rest (not keys?))
            (let* ((rest-var (template-function.argument-specification:parameter-var rest)))
              `(template-function.argument-specification:argument-specification-lambda (,@req-vars &rest ,rest-var)
                 (name-for-types ',prefix ,@req-vars ,rest-var))))

           ((and others rest (not keys?))
            (let* ((others-var (template-function.argument-specification:parameter-var others))
                   (rest-var (template-function.argument-specification:parameter-var rest)))
              `(template-function.argument-specification:argument-specification-lambda (,@req-vars &others ,others-var &rest ,rest-var)
                 (apply #'name-for-types ',prefix ,@req-vars (append ,others-var (list ,rest-var))))))

           ((and (not others) (not rest) keys?)
            (let* ((key-arguments (loop
                                    for p in keywords
                                    for keyword = (template-function.argument-specification:parameter-keyword p)
                                    for var = (template-function.argument-specification:parameter-var p)
                                    for init-type = (template-function.argument-specification:parameter-init-form p)
                                    collect `((,keyword ,var) ',init-type)))
                   (other-keywords (when allow-other-keys?
                                     '(&allow-other-keys)))
                   (key-vars (mapcar #'template-function.argument-specification:parameter-var keywords)))
              `(template-function.argument-specification:argument-specification-lambda (,@req-vars &key ,@key-arguments ,@other-keywords)
                 (name-for-types ',prefix ,@req-vars ,@key-vars))))

           (t
            (error "Do not know how to process parameters object ~A." parameters))))))

(defun make-specialization-lambda-list-function (parameters)
  (compile nil (make-specialization-lambda-list-lambda-form parameters)))

(defun make-name-function (prefix parameters)
  (compile nil (make-name-lambda-form prefix parameters)))

(define-condition invalid-function-type-error (error)
  ((%function-type :initarg :function-type
                   :reader invalid-function-type-error-function-type)
   (%store-lambda-list :initarg :store-lambda-list
                       :reader invalid-function-type-error-store-lambda-list))
  (:report (lambda (condition stream)
             (format stream "Invalid function type ~A for store lambda list ~A."
                     (invalid-function-type-error-function-type condition)
                     (invalid-function-type-error-store-lambda-list condition)))))

(defun signal-invalid-function-type-error (function-type store-lambda-list)
  (error 'invalid-function-type-error
         :function-type function-type
         :store-lambda-list store-lambda-list))

(defun argument-types-to-argument-specification (positional-count arguments)
  (loop
    for sub-arguments on arguments
    for index from 0 below positional-count
    collect (first sub-arguments) into positional
    finally
       (return (append positional
                       (when sub-arguments
                         (cons '&key
                               (loop
                                 for (key value) on sub-arguments by #'cddr
                                 collect (list key value))))))))

(defun argument-specification-to-argument-types (argument-specification)
  (loop
    with state of-type (member positional keyword) = 'positional
    for item in argument-specification
    append (cond ((member item '(&rest &optional))
                  (error "Invalid argument specification ~A." argument-specification))
                 ((eql item '&key)
                  (setf state 'keyword)
                  nil)
                 ((eql state 'positional)
                  (list item))
                 ((eql state 'keyword)
                  (assert (listp item))
                  (list (first item) (second item)))
                 (t
                  (error "Invalid state")))))

(defun parse-argument-specification (argspec)
  (loop
    with req = nil
    with rest = nil
    with restp = nil
    with keys = nil
    with keysp = nil
    with subspec = argspec
    for item = (car subspec)
    while subspec
    do
       (cond ((eql '&optional item)
              (error "Invalid argument specification ~A." argspec))
             ((eql '&rest item)
              (assert (not restp) nil "Duplicate rest constituent in argument specification ~A." argspec)
              (assert (cdr subspec) nil "No rest type specified in argument specification ~A." argspec)
              (setf rest (second subspec)
                    restp t)
              (pop subspec))
             ((eql '&key item)
              (assert (not keysp) nil "Duplicate &key found in argument specification ~A." argspec)
              (assert (not restp) nil "Cannot specify &rest and &key simultaneously in argument specification ~A." argspec)
              (setf keysp t)
              (pop subspec))
             (keysp
              (assert (and (listp item) (= 2 (length item)))
                      nil
                      "Invalid keyword constituent in argument specification ~A." argspec)
              (push item keys)
              (pop subspec))
             (t
              (push item req)
              (pop subspec)))
    finally
       (return (list (nreverse req)
                     rest
                     (nreverse keys)))))

(defun argument-specification-equal (argspec-a argspec-b)
  (flet ((keys-equal (a b)
           (destructuring-bind (key-a type-a) a
             (destructuring-bind (key-b type-b) b
               (and (eql key-a key-b)
                    (alexandria:type= type-a type-b))))))
    (destructuring-bind (req-a rest-a keys-a) (parse-argument-specification argspec-a)
      (destructuring-bind (req-b rest-b keys-b) (parse-argument-specification argspec-b)
        (and (= (length req-a) (length req-b))
             (every #'alexandria:type= req-a req-b)
             (alexandria:type= rest-a rest-b)
             (alexandria:set-equal keys-a keys-b :test #'keys-equal))))))

;;;; Object Layer

(defgeneric name (template-function))
(defgeneric lambda-list (template-function))
(defgeneric name-function (template-function))
(defgeneric lambda-form-function (template-function))
(defgeneric function-type-function (template-function))
(defgeneric inlinep (template-function))

(defgeneric compute-name (template-function argument-specification))
(defgeneric compute-lambda-form (template-function argument-specification))
(defgeneric compute-function-type (template-function argument-specification))
(defgeneric compute-specialization-lambda-list (template-function argument-specification))
(defgeneric compute-name* (template-function &rest argument-specification))
(defgeneric compute-lambda-form* (template-function &rest argument-specification))
(defgeneric compute-function-type* (template-function &rest argument-specification))
(defgeneric compute-specialization-lambda-list* (template-function &rest argument-specification))
(defgeneric complete-argument-specification (template-function argument-specification))
(defgeneric complete-argument-specification* (template-function &rest argument-specification))

(defgeneric funcall-template-function (template-function &rest arguments))
(defgeneric apply-template-function (template-function &rest arguments))
(defgeneric expand-template-function (template-function form &optional environment))
(defgeneric compute-form-argument-specification (template-function form &optional environment))
(defgeneric make-template-function-unbound (template-function))

(defgeneric ensure-instantiation (template-function argument-specification))
(defgeneric ensure-instantiation* (template-function &rest argument-specification))

(defgeneric instantiations (template-function))
(defgeneric add-instantiation (template-function instantiation))
(defgeneric remove-instantiation (template-function instantiation))

(defgeneric find-instantiation (template-function argument-specification))
(defgeneric find-instantiation* (template-function &rest argument-specification))

(defgeneric store (template-function))
(defgeneric store-parameters (template-function))
(defgeneric argument-specification-parameters (template-function))
(defgeneric specialization-lambda-list-function (template-function))

(defgeneric define-template-using-object (template-class
                                          &key
                                            name
                                            lambda-list
                                            environment
                                            lambda-form-function
                                            function-type-function
                                            name-function))

(defclass template-function-class (specialization-store.mop:funcallable-standard-class)
  ())

(defmethod specialization-store.mop:validate-superclass ((class template-function-class)
                                                         (superclass specialization-store.mop:funcallable-standard-class))
  t)

(defclass template-function ()
  ((%name :initarg :name
          :reader name)
   (%lambda-list :initarg :lambda-list
                 :reader lambda-list)
   (%lambda-form-function :initarg :lambda-form-function
                          :reader lambda-form-function)
   (%function-type-function :initarg :function-type-function
                            :reader function-type-function)
   (%name-function :initarg :name-function
                   :reader name-function)
   (%specialization-lambda-list-function :initarg :specialization-lambda-list-function
                                         :reader specialization-lambda-list-function)
   (%store :initarg :store
           :reader store)
   (%store-parameters :initarg :store-parameters
                      :reader store-parameters)
   (%argument-specification-parameters :initarg :argument-specification-parameters
                                       :reader argument-specification-parameters)
   (%inlinep :initarg :inline
             :reader inlinep)
   (%instantiations :initarg :instantiations
                    :reader instantiations))
  (:metaclass template-function-class)
  (:default-initargs
   :inline nil
   :instantiations nil))

(defmethod initialize-instance :after ((instance template-function) &key)
  ;; Initialise %store-parameters and %argument-specification-parameters)
  (with-slots (%lambda-list %store-parameters %argument-specification-parameters) instance
    (setf %store-parameters (specialization-store.lambda-lists:parse-store-lambda-list %lambda-list)
          %argument-specification-parameters (store-parameters-as-arg-spec-parameters %store-parameters)))

  ;; Initialise %specialization-lambda-list-function
  (with-slots (%specialization-lambda-list-function %argument-specification-parameters) instance
    (setf %specialization-lambda-list-function
          (make-specialization-lambda-list-function %argument-specification-parameters)))

  ;; Initialise %name-function
  (with-slots (%name-function %argument-specification-parameters %name) instance
    (unless (and (slot-boundp instance '%name-function)
                 %name-function)
      (setf %name-function (make-name-function %name %argument-specification-parameters))))

  ;; Initialise the store
  (with-slots (%store %lambda-list %store-parameters %name) instance
    (unless (and (slot-boundp instance '%store)
                 %store)
      (setf %store (make-instance 'specialization-store:standard-store
                                  :name %name
                                  :lambda-list %lambda-list
                                  :parameters %store-parameters))))

  (with-slots (%store) instance
    (specialization-store.mop:set-funcallable-instance-function instance %store)))

(defmethod reinitialize-instance :before ((instance template-function)
                                          &key
                                            (name nil namep)
                                            (lambda-list nil lambda-list-p))

  ;; Disallow name changing because any instantiations won't have
  ;; their name changed.
  (when namep
    (with-slots (%name) instance
      (unless (eql %name name)
        (error "Cannot change the name of a template function with name ~A to ~A."
               %name name))))

  ;; Ensure the new lambda list is congruent with the old lambda list.
  (when lambda-list-p
    (with-slots (%lambda-list %store-parameters) instance
      (let* ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
        (unless (specialization-store.lambda-lists:congruent-parameters-p new-parameters %store-parameters)
          (error "The new lambda list ~A is not congruent with the existing lambda list ~A."
                 lambda-list %lambda-list))))))

(defmethod reinitialize-instance :after ((instance template-function)
                                         &key
                                           (lambda-list nil lambda-list-p))
  (when lambda-list-p
    (let* ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
      (with-slots (%store-parameters %argument-specification-parameters) instance
        (setf %store-parameters new-parameters
              %argument-specification-parameters (store-parameters-as-arg-spec-parameters %store-parameters)))))

  (with-slots (%store %lambda-list) instance
    (reinitialize-instance %store
                           :lambda-list %lambda-list)))

(defmethod funcall-template-function ((template-function template-function) &rest args)
  (specialization-store:apply-store (store template-function) args))

(defmethod apply-template-function ((template-function template-function) &rest args)
  (apply #'specialization-store:apply-store (store template-function) args))

(defmethod expand-template-function ((template-function template-function) form &optional environment)
  (cond ((instantiatingp)
         ;; This code is invoked when an instantiation of template
         ;; function is calling another template function.
         (let* ((argument-specification (compute-form-argument-specification template-function form environment))
                (function-type (compute-function-type template-function argument-specification))
                (instantiation-name (compute-name template-function argument-specification)))
           (unless (processed-template-function-p template-function argument-specification)
             ;; Insert a dummy function so that the compiler doesn't
             ;; complain about a missing function definition.
             (fmakunbound instantiation-name)
             (setf (fdefinition instantiation-name) (lambda (&rest args)
                                                      (declare (ignore args))
                                                      nil))
             (proclaim `(ftype ,function-type ,instantiation-name))
             (note-template-function template-function argument-specification))
           (cons instantiation-name (specialization-store:compiler-macro-form-arguments form))))
        (t
         (specialization-store:expand-store (store template-function) form environment))))

(defmethod make-template-function-unbound ((template-function template-function))
  (specialization-store:make-store-unbound (store template-function))
  (let* ((name (name template-function)))
    (setf (compiler-macro-function name) nil)
    (fmakunbound name)))

(defmethod compute-name ((template-function template-function) argument-specification)
  (funcall (name-function template-function)
           (complete-argument-specification template-function argument-specification)))

(defmethod compute-lambda-form ((template-function template-function) argument-specification)
  (funcall (lambda-form-function template-function)
           (complete-argument-specification template-function argument-specification)))

(defmethod compute-function-type ((template-function template-function) argument-specification)
  (funcall (function-type-function template-function)
           argument-specification))

(defmethod compute-specialization-lambda-list ((template-function template-function) argument-specification)
  (funcall (specialization-lambda-list-function template-function)
           (complete-argument-specification template-function argument-specification)))

(defmethod compute-name* ((template-function template-function) &rest argument-specification)
  (compute-name template-function argument-specification))

(defmethod compute-lambda-form* ((template-function template-function) &rest argument-specification)
  (compute-lambda-form template-function argument-specification))

(defmethod compute-function-type* ((template-function template-function) &rest argument-specification)
  (compute-function-type template-function argument-specification))

(defmethod compute-specialization-lambda-list* ((template-function template-function) &rest argument-specification)
  (compute-specialization-lambda-list template-function argument-specification))

(defmethod compute-form-argument-specification ((parameters specialization-store.lambda-lists:parameters) form &optional environment)
  (let* ((arguments (specialization-store:compiler-macro-form-arguments form))
         (required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (keys? (specialization-store.lambda-lists:keyword-parameters-p parameters)))
    (cond ((not keys?)
           (loop
             for arg in arguments
             collect (specialization-store:determine-form-value-type arg environment)))
          (t
           (let* ((positional-count (+ (length required) (length optional)))
                  (positional-args (subseq arguments 0 positional-count))
                  (keyword-args (subseq arguments positional-count)))
             (unless (zerop (mod (length keyword-args) 2))
               (error "The length of the keyword arguments in form ~A is not even." form))
             (append (loop
                       for arg in positional-args
                       collect (specialization-store:determine-form-value-type arg environment))
                     (cons '&key
                           (loop
                             for (keyword arg) on keyword-args by #'cddr
                             if (constantp keyword environment)
                               collect (list (introspect-environment:constant-form-value keyword environment)
                                             (specialization-store:determine-form-value-type arg environment))
                             else do
                               (warn "Unable to determine the keyword value ~A in the form ~A." keyword form)))))))))

(defmethod compute-form-argument-specification ((template-function template-function) form &optional environment)
  (complete-argument-specification template-function
                                   (compute-form-argument-specification (store-parameters template-function)
                                                                        form environment)))

(defmethod complete-argument-specification ((template-function template-function) argument-specification)
  (second (compute-function-type template-function argument-specification)))

(defmethod complete-argument-specification* ((template-function template-function) &rest argument-specification)
  (complete-argument-specification template-function argument-specification))

(defmethod add-instantiation ((template-function template-function) (instantiation instantiation))
  (remove-instantiation template-function instantiation)

  (let* ((lambda-form (instantiation-lambda-form instantiation))
         (function-type (instantiation-function-type instantiation))
         (name (instantiation-name instantiation))
         (function (instantiation-function instantiation))
         (argument-specification (instantiation-argument-specification instantiation))
         (expand-function (if (inlinep template-function)
                              (specialization-store:compiler-macro-lambda (&rest args)
                                `(the ,(third function-type) (,lambda-form ,@args)))
                              (specialization-store:compiler-macro-lambda (&rest args)
                                `(the ,(third function-type) (,name ,@args)))))
         (specialization-lambda-list (compute-specialization-lambda-list template-function
                                                                         argument-specification))
         (specialization (make-instance 'specialization-store:standard-specialization
                                        :lambda-list specialization-lambda-list
                                        :value-type (third function-type)
                                        :name name
                                        :function function
                                        :expand-function expand-function)))
    (specialization-store:add-specialization (store template-function) specialization)
    (fmakunbound name)
    (setf (fdefinition name) function)
    (proclaim `(ftype ,function-type ,name)))

  (push instantiation (slot-value template-function '%instantiations))
  (values))

(defmethod remove-instantiation ((template-function template-function) (instantiation instantiation))
  (alexandria:removef (slot-value template-function '%instantiations)
                      (instantiation-argument-specification instantiation)
                      :test #'argument-specification-equal
                      :key #'instantiation-argument-specification)

  (let* ((argument-specification (complete-argument-specification template-function
                                                                  (instantiation-argument-specification instantiation)))
         (specialization-lambda-list (compute-specialization-lambda-list template-function argument-specification))
         (specialization (find specialization-lambda-list
                               (specialization-store:store-specializations (store template-function))
                               :key #'specialization-store:specialization-lambda-list
                               :test #'equalp)))
    (when specialization
      (specialization-store:remove-specialization (store template-function) specialization))))

;;;; Glue Layer (Template Function)

(defun find-template-function (name &optional (errorp t))
  (or (get name 'template-function)
      (when errorp
        (error "Unable to find template function with name ~A." name))))

(defmethod make-template-function-unbound ((name symbol))
  (make-template-function-unbound (find-template-function name)))

(defun ensure-template-function (name lambda-list &rest args
                                 &key
                                   lambda-form-function
                                   function-type-function
                                   name-function
                                   inline)
  (declare (ignore lambda-form-function function-type-function name-function))
  (let* ((existing-template-function (find-template-function name nil))
         (template-function (cond (existing-template-function
                                   (alexandria:remove-from-plistf args :name-function :name :inline)
                                   (apply #'reinitialize-instance
                                          existing-template-function
                                          :lambda-list lambda-list
                                          :inline inline
                                          args))
                                  (t
                                   (apply #'make-instance
                                          'template-function
                                          :name name
                                          :lambda-list lambda-list
                                          :inline inline
                                          args)))))
    (setf (get name 'template-function) template-function
          (fdefinition name) template-function
          (compiler-macro-function name) (lambda (form &optional environment)
                                           (handler-case (expand-template-function template-function form environment)
                                             (error (c)
                                               (princ c *error-output*)
                                               form))))
    template-function))

(defmethod compute-name ((name symbol) argument-specification)
  (compute-name (find-template-function name) argument-specification))

(defmethod compute-name* ((name symbol) &rest argument-specification)
  (compute-name (find-template-function name) argument-specification))

(defmethod compute-function-type ((name symbol) argument-specification)
  (compute-function-type (find-template-function name) argument-specification))

(defmethod compute-function-type* ((name symbol) &rest argument-specification)
  (compute-function-type (find-template-function name) argument-specification))

;;;; Glue Layer (Instantiation requests)

(defun %ensure-instantiations (requests)
  (labels ((process (template-function argument-specification)
             (multiple-value-bind (lambda-form function-type) (compute-lambda-form template-function argument-specification)
               (let* ((function-type (or function-type
                                         (compute-function-type template-function argument-specification)))
                      (name (compute-name template-function argument-specification))
                      (named-lambda-form `(alexandria:named-lambda ,name ,@(rest lambda-form)))
                      (wrapper-fn (compile nil `(lambda ()
                                                  ,named-lambda-form)))
                      (function (funcall wrapper-fn))
                      (instantiation (make-instantiation :template-function template-function
                                                         :argument-specification argument-specification
                                                         :name name
                                                         :lambda-form lambda-form
                                                         :function-type function-type
                                                         :function function)))
                 (add-instantiation template-function instantiation)
                 instantiation))))
    (let* ((*instantiating* t)
           (*records* (loop
                        for (template-function argument-specification) in requests
                        collect (make-record :template-function template-function
                                             :argument-specification argument-specification)))
           (*processed* nil))
      (loop
        for record = (pop *records*)
        until (null record)
        unless (processed-record-p record)
          do
             (push (process (record-template-function record)
                            (record-argument-specification record))
                   *processed*))
      *processed*)))

(defun %ensure-instantiation (template-function argument-specification)
  (%ensure-instantiations (list (list template-function argument-specification))))

(defmethod ensure-instantiation ((template-function template-function) argument-specification)
  (%ensure-instantiation template-function argument-specification))

(defmethod ensure-instantiation ((name symbol) argument-specification)
  (ensure-instantiation (find-template-function name) argument-specification))

(defmethod ensure-instantiation* ((name symbol) &rest argument-specification)
  (ensure-instantiation name argument-specification))

(defmethod ensure-instantiation* ((template-function template-function) &rest argument-specification)
  (%ensure-instantiation template-function argument-specification)
  (values))

(defun %install-instantiation (template-function-name argument-specification lambda-form function-type name function)
  (let* ((template-function (find-template-function template-function-name))
         (instantiation (make-instantiation :argument-specification argument-specification
                                            :lambda-form lambda-form
                                            :function-type function-type
                                            :name name
                                            :function function
                                            :template-function template-function)))
    (add-instantiation template-function instantiation))
  (values))


;;;; Syntax Layer (template function)

(defmethod define-template-using-object ((class template-function-class)
                                         &key
                                           name
                                           lambda-list
                                           environment
                                           lambda-form-function
                                           function-type-function
                                           name-function
                                           inline)
  (macrolet ((check-arg (arg)
               `(unless ,arg
                  (error "~A argument is required." ',arg))))
    (check-arg name)
    (check-arg lambda-list)
    (check-arg lambda-form-function)
    (check-arg function-type-function))
  (labels ((wrap-function (function)
             (if (symbolp function)
                 `(lambda (&rest args)
                    (apply (function ,function) args))
                 function)))
    (destructuring-bind (store-parameters globals) (specialization-store.lambda-lists:parameter-init-forms-as-global-functions
                                                    name
                                                    (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)
                                                    environment)
      (let* ((lambda-list-parameters (store-parameters-as-arg-spec-parameters store-parameters))
             (name-function (or name-function
                                (make-name-lambda-form name lambda-list-parameters)))
             (new-lambda-list (specialization-store.lambda-lists:original-lambda-list store-parameters)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,@globals
           (ensure-template-function ',name ',new-lambda-list
                                     :lambda-form-function ,(wrap-function lambda-form-function)
                                     :function-type-function ,(wrap-function function-type-function)
                                     :name-function ,(wrap-function name-function)
                                     :inline ,inline))))))

(defmacro define-template (name lambda-list &body body &environment env)
  (labels ((body-values (key &optional errorp)
             (let* ((v (find key body :key #'first)))
               (cond (v (values (rest v) t))
                     (errorp (error "Unable to find ~A option." key))
                     (t
                      (values nil nil)))))
           (body-value (key &optional errorp)
             (multiple-value-bind (args present?) (body-values key errorp)
               (values (first args) present?)))
           (ensure-class (class)
             (if (symbolp class)
                 (find-class class)
                 class)))
    (let* ((class (ensure-class (or (body-value :metaclass)
                                    'template-function))))
      (define-template-using-object class
        :name name
        :lambda-list lambda-list
        :lambda-form-function (body-value :lambda-form-function t)
        :function-type-function (body-value :function-type-function t)
        :name-function (body-value :name-function)
        :environment env
        :inline (body-value :inline)))))

;;;; Syntax Layer (Instantiation requests)

(defmacro require-instantiations (&rest pairs-of-name-and-argument-specifications)
  (let* ((pairs (loop
                  for (template-function-name . requests) in pairs-of-name-and-argument-specifications
                  for template-function = (find-template-function template-function-name)
                  append (loop
                           for argument-specification in requests
                           collect (list template-function argument-specification))))
         (instantiations (%ensure-instantiations pairs)))
    `(eval-when (:load-toplevel)
       ;; Generate the instantiations when a fasl is loaded.
       ,@(loop
           for instantiation in instantiations
           for template-function-name = (name (instantiation-template-function instantiation))
           for name = (instantiation-name instantiation)
           for lambda-form = (instantiation-lambda-form instantiation)
           for function-type = (instantiation-function-type instantiation)
           for argument-specification = (instantiation-argument-specification instantiation)
           collect
           `(%install-instantiation ',template-function-name ',argument-specification
                                    ',lambda-form ',function-type ',name
                                    (alexandria:named-lambda ,name ,@(rest lambda-form)))))))

(defmacro require-instantiation (template-function-name argument-specification)
  `(require-instantiations (,template-function-name (,@argument-specification))))
