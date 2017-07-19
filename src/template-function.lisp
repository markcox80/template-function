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

(defstruct instantiation
  template-function
  argument-specification
  name
  lambda-form
  function
  function-type)

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

(defun %complete-argument-specification (store-parameters type-completion-function argument-specification)
  (let* ((required (specialization-store.lambda-lists:required-parameters store-parameters))
         (optional (specialization-store.lambda-lists:optional-parameters store-parameters))
         (positional-count (+ (length required) (length optional)))
         (rest (specialization-store.lambda-lists:rest-parameter store-parameters))
         (keys? (specialization-store.lambda-lists:keyword-parameters-p store-parameters)))
    (labels ((to-argument-specification (completed-forms)
               (append (subseq completed-forms 0 positional-count)
                       (when keys?
                         (cons '&key
                               (loop
                                 for (keyword type) on (nthcdr positional-count completed-forms) by #'cddr
                                 collect (list keyword type))))))
             (to-form-types (argument-specification)
               (let* ((length (length argument-specification)))
                 (append (subseq argument-specification 0 (min positional-count length))
                         (when (>= length positional-count)
                           (let* ((key-section (nthcdr positional-count argument-specification)))
                             (cond ((null key-section)
                                    nil)
                                   ((eql '&key (first key-section))
                                    (loop
                                      for (keyword type) in (rest key-section)
                                      append (list keyword type)))
                                   (t
                                    (error "Invalid argument specification ~A for store parameters ~A."
                                           argument-specification
                                           (specialization-store.lambda-lists:original-lambda-list store-parameters))))))))))
      (cond ((and (not keys?) optional rest)
             (error "Cannot complete argument specification for functions which specify optional parameters and a rest parameter."))
            ((and (not keys?) (null optional))
             ;; Nothing to do
             argument-specification)
            (t
             (let* ((fn (funcall type-completion-function (lambda (&rest args)
                                                             args)))
                    (form-types (to-form-types argument-specification))
                    (completed-form-types (apply fn form-types)))
               (to-argument-specification completed-form-types)))))))

(defun make-argument-specification-completion-function (store-parameters type-completion-function)
  (let* ((optional (specialization-store.lambda-lists:optional-parameters store-parameters))
         (rest (specialization-store.lambda-lists:rest-parameter store-parameters))
         (keys? (specialization-store.lambda-lists:keyword-parameters-p store-parameters)))
    (cond ((and (not keys?) optional rest)
           (error "A user supplied argument specification completion function is required for template functions which specify optional and rest parameters."))
          ((and (not keys?) (null optional))
           (lambda (continuation)
             (lambda (argument-specification)
               (funcall continuation argument-specification))))
          (t
           (lambda (continuation)
             (lambda (argument-specification)
               (funcall continuation
                        (%complete-argument-specification store-parameters
                                                          type-completion-function
                                                          argument-specification))))))))

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

(defun make-value-completion-function (parameters)
  (compile nil (specialization-store.lambda-lists:make-value-completion-lambda-form parameters)))

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

(defun make-type-completion-lambda-form/sans-rest-sans-keywords (store-parameters function-type-function)
  (assert (not (specialization-store.lambda-lists:rest-parameter store-parameters)))
  (assert (not (specialization-store.lambda-lists:keyword-parameters-p store-parameters)))
  (let* ((original-lambda-list (specialization-store.lambda-lists:original-lambda-list store-parameters)))
    `(lambda (continuation)
       (lambda (&rest arguments)
         (let* ((ftype (funcall ,function-type-function arguments))
                (arg-spec (second ftype)))
           (loop
             for arg in arg-spec
             when (or (eql arg '&optional)
                      (eql arg '&rest)
                      (eql arg '&key))
               do (signal-invalid-function-type-error ftype ',original-lambda-list))
           (apply continuation arg-spec))))))

(defun make-type-completion-lambda-form/with-rest-sans-keywords (store-parameters function-type-function)
  (assert (specialization-store.lambda-lists:rest-parameter store-parameters))
  (assert (not (specialization-store.lambda-lists:keyword-parameters-p store-parameters)))
  (let* ((original-lambda-list (specialization-store.lambda-lists:original-lambda-list store-parameters)))
    `(lambda (continuation)
       (lambda (&rest arguments)
         (let* ((ftype (funcall ,function-type-function arguments))
                (arg-spec (second ftype)))
           (loop
             for arg in arg-spec
             when (or (eql arg '&optional)
                      (eql arg '&key))
               do (signal-invalid-function-type-error ftype ',original-lambda-list))
           (apply continuation (loop
                                 for arg in arg-spec
                                 until (eql arg '&rest)
                                 collect arg)))))))

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

(defun make-type-completion-lambda-form/with-keywords (store-parameters function-type-function)
  (assert (specialization-store.lambda-lists:keyword-parameters-p store-parameters))
  (let* ((required (specialization-store.lambda-lists:required-parameters store-parameters))
         (optional (specialization-store.lambda-lists:optional-parameters store-parameters))
         (positional-count (+ (length required) (length optional))))
    `(lambda (continuation)
       (lambda (&rest arguments)
         (let* ((input (argument-types-to-argument-specification ,positional-count
                                                                 arguments))
                (output (funcall ,function-type-function input)))
           (apply continuation (argument-specification-to-argument-types (second output))))))))

(defun make-type-completion-lambda-form (store-parameters function-type-function)
  (let* ((rest (specialization-store.lambda-lists:rest-parameter store-parameters))
         (keywords? (specialization-store.lambda-lists:keyword-parameters-p store-parameters)))
    (cond ((and (not rest) (not keywords?))
           (make-type-completion-lambda-form/sans-rest-sans-keywords store-parameters
                                                                     function-type-function))
          ((and rest (not keywords?))
           (make-type-completion-lambda-form/with-rest-sans-keywords store-parameters
                                                                     function-type-function))
          (t
           (make-type-completion-lambda-form/with-keywords store-parameters
                                                           function-type-function)))))

(defun make-type-completion-function (store-parameters function-type-completion-function)
  (compile nil (make-type-completion-lambda-form store-parameters function-type-completion-function)))

;;;; Object Layer

(defgeneric name (template-function))
(defgeneric lambda-list (template-function))
(defgeneric name-function (template-function))
(defgeneric lambda-form-function (template-function))
(defgeneric function-type-function (template-function))
(defgeneric type-completion-function (template-function))
(defgeneric value-completion-function (template-function))
(defgeneric argument-specification-completion-function (template-function))

(defgeneric compute-name (template-function argument-specification))
(defgeneric compute-lambda-form (template-function argument-specification))
(defgeneric compute-function-type (template-function argument-specification))
(defgeneric compute-specialization-lambda-list (template-function argument-specification))
(defgeneric compute-name* (template-function &rest argument-specification))
(defgeneric compute-lambda-form* (template-function &rest argument-specification))
(defgeneric compute-function-type* (template-function &rest argument-specification))
(defgeneric compute-specialization-lambda-list (template-function argument-specification))
(defgeneric complete-argument-specification (template-function argument-specification))
(defgeneric complete-argument-specification* (template-function &rest argument-specification))

(defgeneric funcall-template-function (template-function &rest arguments))
(defgeneric apply-template-function (template-function &rest arguments))
(defgeneric expand-template-function (template-function form &optional environment))
(defgeneric compute-form-argument-specification (template-function form &optional environment))
(defgeneric make-template-function-unbound (template-function))

(defgeneric ensure-instantiation (template-function argument-specification))
(defgeneric ensure-instantiation* (template-function &rest argument-specification))
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
                                            name-function
                                            value-completion-function
                                            type-completion-function))

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
   (%value-completion-function :initarg :value-completion-function
                               :reader value-completion-function)
   (%type-completion-function :initarg :type-completion-function
                              :reader type-completion-function)
   (%argument-specification-completion-function :initarg :argument-specification-completion-function
                                                :reader argument-specification-completion-function)
   (%specialization-lambda-list-function :initarg :specialization-lambda-list-function
                                         :reader specialization-lambda-list-function)
   (%store :initarg :store
           :reader store)
   (%store-parameters :initarg :store-parameters
                      :reader store-parameters)
   (%argument-specification-parameters :initarg :argument-specification-parameters
                                       :reader argument-specification-parameters))
  (:metaclass template-function-class))

(defmethod initialize-instance :after ((instance template-function) &key)
  ;; Initialise %store-parameters and %argument-specification-parameters)
  (with-slots (%lambda-list %store-parameters %argument-specification-parameters) instance
    (setf %store-parameters (specialization-store.lambda-lists:parse-store-lambda-list %lambda-list)
          %argument-specification-parameters (store-parameters-as-arg-spec-parameters %store-parameters)))

  ;; Initialise %value-completion-function
  (with-slots (%value-completion-function %store-parameters) instance
    (unless (and (slot-boundp instance '%value-completion-function)
                 %value-completion-function)
      (setf %value-completion-function (make-value-completion-function %store-parameters))))

  ;; Initialise %type-completion-function
  (with-slots (%type-completion-function %store-parameters %function-type-function) instance
    (unless (and (slot-boundp instance '%type-completion-function)
                 %type-completion-function)
      (setf %type-completion-function (make-type-completion-function %store-parameters %function-type-function))))

  ;; Initialise %argument-specification-completion-function
  (with-slots (%argument-specification-completion-function %type-completion-function %store-parameters) instance
    (unless (and (slot-boundp instance '%argument-specification-completion-function)
                 %argument-specification-completion-function)
      (setf %argument-specification-completion-function
            (make-argument-specification-completion-function %store-parameters
                                                             %type-completion-function))))

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
  (with-slots (%store %value-completion-function %type-completion-function %lambda-list %store-parameters) instance
    (unless (and (slot-boundp instance '%store)
                 %store)
      (setf %store (make-instance 'specialization-store:standard-store
                                  :type-completion-function (type-completion-function instance)
                                  :value-completion-function (value-completion-function instance)
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
                                           (lambda-list nil lambda-list-p)
                                           type-completion-function
                                           value-completion-function
                                           argument-specification-completion-function)
  (when lambda-list-p
    (let* ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
      (with-slots (%store-parameters %argument-specification-parameters) instance
        (setf %store-parameters new-parameters
              %argument-specification-parameters (store-parameters-as-arg-spec-parameters %store-parameters)))

      (unless value-completion-function
        (setf (slot-value instance '%value-completion-function)
              (make-value-completion-function new-parameters)))

      (unless type-completion-function
        (setf (slot-value instance '%type-completion-function)
              (make-type-completion-function new-parameters
                                             (slot-value instance '%function-type-function))))))

  (when (and (or lambda-list-p type-completion-function)
             (not argument-specification-completion-function))
    (setf (slot-value instance '%argument-specification-completion-function)
          (make-argument-specification-completion-function (store-parameters instance)
                                                           (type-completion-function instance))))

  (with-slots (%store %lambda-list %type-completion-function %value-completion-function) instance
    (reinitialize-instance %store
                           :lambda-list %lambda-list
                           :type-completion-function %type-completion-function
                           :value-completion-function %value-completion-function)))

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
           (complete-argument-specification template-function argument-specification)))

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
  (let* ((fn (funcall (argument-specification-completion-function template-function)
                      #'identity)))
    (funcall fn argument-specification)))

(defmethod complete-argument-specification* ((template-function template-function) &rest argument-specification)
  (complete-argument-specification template-function argument-specification))

;;;; Glue Layer (Template Function)

(defun find-template-function (name &optional (errorp t))
  (or (get name 'template-function)
      (when errorp
        (error "Unable to find template function with name ~A." name))))

(defmethod make-template-function-unbound ((name symbol))
  (make-template-function-unbound (find-template-function name)))

(defun ensure-template-function (name lambda-list &rest args
                                 &key
                                   value-completion-function
                                   type-completion-function
                                   lambda-form-function
                                   function-type-function
                                   name-function)
  (declare (ignore value-completion-function type-completion-function
                   lambda-form-function function-type-function name-function))
  (let* ((existing-template-function (find-template-function name nil))
         (template-function (cond (existing-template-function
                                   (alexandria:remove-from-plistf args :name-function :name)
                                   (apply #'reinitialize-instance
                                          existing-template-function
                                          :lambda-list lambda-list
                                          args))
                                  (t
                                   (apply #'make-instance
                                          'template-function
                                          :name name
                                          :lambda-list lambda-list
                                          args)))))
    (setf (get name 'template-function) template-function
          (fdefinition name) template-function
          (compiler-macro-function name) (lambda (form &optional environment)
                                           (expand-template-function template-function form environment)))
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
                      (wrapper-fn (compile nil `(lambda ()
                                                  (alexandria:named-lambda ,name ,@(rest lambda-form)))))
                      (function (funcall wrapper-fn))
                      (expand-function (specialization-store:compiler-macro-lambda (&rest args)
                                         `(the ,(third function-type) (,name ,@args))))
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
                 (proclaim `(ftype ,function-type ,name))
                 (make-instantiation :template-function template-function
                                     :argument-specification argument-specification
                                     :name name
                                     :lambda-form lambda-form
                                     :function-type function-type
                                     :function function)))))
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

(defun %install-instantiation (template-function-name argument-specification function)
  (let* ((template-function (find-template-function template-function-name))
         (name (compute-name template-function argument-specification))
         (function-type (compute-function-type template-function argument-specification))
         (specialization-lambda-list (compute-specialization-lambda-list template-function argument-specification))
         (expand-function (specialization-store:compiler-macro-lambda (&rest args)
                            `(the ,(third function-type) (,name ,@args))))
         (specialization (make-instance 'specialization-store:standard-specialization
                                        :name name
                                        :lambda-list specialization-lambda-list
                                        :function function
                                        :expand-function expand-function
                                        :value-type (third function-type))))
    (specialization-store:add-specialization (store template-function) specialization)
    (setf (fdefinition name) function)
    (proclaim `(ftype ,function-type ,name)))
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
                                           value-completion-function
                                           type-completion-function)
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
                                                    (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)
                                                    environment)
      (let* ((value-completion-function (or value-completion-function
                                            (specialization-store.lambda-lists:make-value-completion-lambda-form store-parameters)))
             (type-completion-function (or type-completion-function
                                           (specialization-store.lambda-lists:make-type-completion-lambda-form store-parameters environment)))
             (lambda-list-parameters (store-parameters-as-arg-spec-parameters store-parameters))
             (name-function (or name-function
                                (make-name-lambda-form name lambda-list-parameters)))
             (new-lambda-list (specialization-store.lambda-lists:original-lambda-list store-parameters)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,@globals
           (ensure-template-function ',name ',new-lambda-list
                                     :lambda-form-function ,(wrap-function lambda-form-function)
                                     :function-type-function ,(wrap-function function-type-function)
                                     :name-function ,(wrap-function name-function)
                                     :value-completion-function ,(wrap-function value-completion-function)
                                     :type-completion-function ,(wrap-function type-completion-function)))))))

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
        :type-completion-function (body-value :type-completion-function)
        :value-completion-function (body-value :value-completion-function)
        :environment env))))

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
           for argument-specification = (instantiation-argument-specification instantiation)
           collect
           `(%install-instantiation ',template-function-name ',argument-specification
                                    (alexandria:named-lambda ,name ,@(rest lambda-form)))))))

(defmacro require-instantiation ((template-function-name &rest argument-specification))
  `(require-instantiations (,template-function-name ,@argument-specification)))
