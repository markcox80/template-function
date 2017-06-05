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
  argument-types)

(defun note-template-function (template-function argument-types)
  (push (make-record :template-function template-function
                     :argument-types argument-types)
        *records*))

;; Template functions that have been processed i.e. the template has
;; been instantiated and inserted in to the global environment. Each
;; item must be an instance of instantiation.
(defvar *processed*)

(defstruct instantiation
  template-function
  argument-types
  name
  lambda-form
  function
  function-type)

(defun processed-template-function-p (template-function argument-types)
  (find-if #'(lambda (instantiation)
               (and (eql (instantiation-template-function instantiation)
                         template-function)
                    (equalp (instantiation-argument-types instantiation)
                            argument-types)))
           *processed*))

(defun processed-record-p (record)
  (processed-template-function-p (record-template-function record)
                                 (record-argument-types record)))

;;;; Helpers

(defun destructure-parameters (parameters arguments)
  (flet ((insufficient-arguments ()
           (return-from destructure-parameters 'insufficient-arguments))
         (too-many-arguments ()
           (return-from destructure-parameters 'too-many-arguments)))
    (append (loop
              for parameter in (specialization-store.lambda-lists:required-parameters parameters)
              collect
              (if arguments
                  (list parameter (pop arguments))
                  (insufficient-arguments)))
            (loop
              for parameter in (specialization-store.lambda-lists:optional-parameters parameters)
              collect
              (if arguments
                  (list parameter (pop arguments))
                  (list parameter)))
            (cond ((specialization-store.lambda-lists:keyword-parameters-p parameters)
                   (loop
                     with absent = '#:absent
                     for parameter in (specialization-store.lambda-lists:keyword-parameters parameters)
                     for keyword = (specialization-store.lambda-lists:parameter-keyword parameter)
                     for keyword-arg = (getf arguments keyword absent)
                     if (eql keyword-arg absent)
                       collect (list parameter)
                     else
                       collect (list keyword keyword-arg)))
                  ((specialization-store.lambda-lists:rest-parameter-p parameters)
                   (cons (specialization-store.lambda-lists:rest-parameter parameters)
                         arguments))
                  (arguments
                   (too-many-arguments))))))

(defun make-specialization-lambda-list (parameters argument-types)
  (let* ((d (destructure-parameters parameters argument-types)))
    (cond ((eql d 'insufficient-arguments)
           (error "Not enough argument types provided in ~A." argument-types))
          ((eql d 'too-many-arguments)
           (error "Too many argument types provided in ~A." argument-types))
          (t
           (loop
             with rv = nil
             with optional? = (specialization-store.lambda-lists:optional-parameters-p parameters)
             with keyword? = (specialization-store.lambda-lists:keyword-parameters-p parameters)
             for item in d
             for (parameter value) = item
             for value? = (not (null (cdr item)))
             do
                (etypecase parameter
                  (specialization-store.lambda-lists:required-parameter
                   (push (list (specialization-store.lambda-lists:parameter-var parameter)
                               value)
                         rv))
                  (specialization-store.lambda-lists:optional-parameter
                   (when optional?
                     (setf optional? nil)
                     (push '&optional rv))
                   (push (list (specialization-store.lambda-lists:parameter-var parameter)
                               (if value?
                                   value
                                   t))
                         rv))
                  (specialization-store.lambda-lists:keyword-parameter
                   (when keyword?
                     (setf keyword? nil)
                     (push '&key rv))
                   (push (list (list (specialization-store.lambda-lists:parameter-keyword parameter)
                                     (specialization-store.lambda-lists:parameter-var parameter))
                               (if value?
                                   value
                                   t))
                         rv))
                  (specialization-store.lambda-lists:rest-parameter
                   (loop
                     for arg in (rest item)
                     do
                        (push (list (gensym "REST") arg) rv))))
             finally
                (progn
                  (when (specialization-store.lambda-lists:allow-other-keys-p parameters)
                    (push '(&allow-other-keys) rv))
                  (return (nreverse rv))))))))

(defun make-name-lambda-form (prefix parameters)
  ;; Create a lambda form which accepts argument types and returns a
  ;; unique name.
  (let* ((optional? (specialization-store.lambda-lists:optional-parameters-p parameters))
         (keyword? (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         lambda-list vars)
    (loop
      for required in (specialization-store.lambda-lists:required-parameters parameters)
      for var = (specialization-store.lambda-lists:parameter-var required)
      do
         (push var lambda-list)
         (push var vars))
    (when optional?
      (loop
        for optional in (specialization-store.lambda-lists:optional-parameters parameters)
        for var = (specialization-store.lambda-lists:parameter-var optional)
        do
           (push var lambda-list)
           (push var vars)))
    (when (and (not keyword?) rest)
      (push '&rest lambda-list)
      (let* ((var (specialization-store.lambda-lists:parameter-var rest)))
        (push var lambda-list)
        (push var vars)))
    (when (specialization-store.lambda-lists:keyword-parameters-p parameters)
      (push '&key lambda-list)
      (loop
        for keyword in (specialization-store.lambda-lists:keyword-parameters parameters)
        for key = (specialization-store.lambda-lists:parameter-keyword keyword)
        for var = (specialization-store.lambda-lists:parameter-var keyword)
        do
           (push (list (list key var)) lambda-list)
           (push var vars))
      (when (specialization-store.lambda-lists:allow-other-keys-p parameters)
        (push '&allow-other-keys lambda-list)))
    (cond ((or (not (specialization-store.lambda-lists:rest-parameter-p parameters))
               (specialization-store.lambda-lists:keyword-parameters-p parameters))
           `(lambda ,(nreverse lambda-list)
              (name-for-types ',prefix ,@(nreverse vars))))
          ((specialization-store.lambda-lists:rest-parameter-p parameters)
           `(lambda ,(nreverse lambda-list)
              (apply #'name-for-types ',prefix ,@(nreverse vars))))
          (t
           (error "Do not know how to process parameters of this kind.")))))

(defun make-name-function (prefix parameters)
  (compile nil (make-name-lambda-form prefix parameters)))

(defun make-value-completion-function (parameters)
  (compile nil (specialization-store.lambda-lists:make-value-completion-lambda-form parameters)))

(defun make-type-completion-function (parameters)
  (compile nil (specialization-store.lambda-lists:make-type-completion-lambda-form parameters nil)))

;;;; Object Layer

(defgeneric name (template-function))
(defgeneric lambda-list (template-function))
(defgeneric name-function (template-function))
(defgeneric lambda-form-function (template-function))
(defgeneric function-type-function (template-function))
(defgeneric type-completion-function (template-function))
(defgeneric value-completion-function (template-function))
(defgeneric store (template-function))
(defgeneric parameters (template-function))

(defgeneric compute-name (template-function argument-types))
(defgeneric compute-lambda-form (template-function argument-types))
(defgeneric compute-function-type (template-function argument-types))
(defgeneric compute-name* (template-function &rest argument-types))
(defgeneric compute-lambda-form* (template-function &rest argument-types))
(defgeneric compute-function-type* (template-function &rest argument-types))
(defgeneric complete-argument-types (template-function argument-types))
(defgeneric complete-argument-types* (template-function &rest argument-types))

(defgeneric funcall-template-function (template-function &rest arguments))
(defgeneric apply-template-function (template-function &rest arguments))
(defgeneric expand-template-function (template-function form &optional environment))
(defgeneric compute-form-argument-types (template-function form &optional environment))
(defgeneric make-template-function-unbound (template-function))

(defgeneric ensure-instantiation (template-function argument-types))
(defgeneric ensure-instantiation* (template-function &rest argument-types))
(defgeneric find-instantiation (template-function argument-types))
(defgeneric find-instantiation* (template-function &rest argument-types))

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
   (%store :initarg :store
           :reader store)
   (%parameters :initarg :parameters
                :reader parameters))
  (:metaclass template-function-class))

(defmethod initialize-instance :after ((instance template-function) &key)
  ;; Initialise %parameters
  (with-slots (%parameters %lambda-list) instance
    (setf %parameters (specialization-store.lambda-lists:parse-store-lambda-list %lambda-list)))

  ;; Initialise %value-completion-function
  (with-slots (%value-completion-function %parameters) instance
    (unless (and (slot-boundp instance '%value-completion-function)
                 %value-completion-function)
      (setf %value-completion-function (make-value-completion-function %parameters))))

  ;; Initialise %type-completion-function
  (with-slots (%type-completion-function %parameters) instance
    (unless (and (slot-boundp instance '%type-completion-function)
                 %type-completion-function)
      (setf %type-completion-function (make-type-completion-function %parameters))))

  ;; Initialise %name-function
  (with-slots (%name-function %parameters %name) instance
    (unless (and (slot-boundp instance '%name-function)
                 %name-function)
      (setf %name-function (make-name-function %name %parameters))))

  ;; Initialise the store
  (with-slots (%store %value-completion-function %type-completion-function %lambda-list %parameters) instance
    (unless (and (slot-boundp instance '%store)
                 %store)
      (setf %store (make-instance 'specialization-store:standard-store
                                  :type-completion-function (type-completion-function instance)
                                  :value-completion-function (value-completion-function instance)
                                  :lambda-list %lambda-list
                                  :parameters %parameters))))

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
    (with-slots (%lambda-list %parameters) instance
      (let* ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
        (unless (specialization-store.lambda-lists:congruent-parameters-p new-parameters %parameters)
          (error "The new lambda list ~A is not congruent with the existing lambda list ~A."
                 lambda-list %lambda-list))))))

(defmethod reinitialize-instance :after ((instance template-function)
                                         &key
                                           (lambda-list nil lambda-list-p)
                                           type-completion-function
                                           value-completion-function)
  (when lambda-list-p
    (let* ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
      (with-slots (%parameters) instance
        (setf %parameters new-parameters))

      (unless value-completion-function
        (setf (slot-value instance '%value-completion-function) (make-value-completion-function new-parameters)))

      (unless type-completion-function
        (setf (slot-value instance '%type-completion-function) (make-type-completion-function new-parameters)))))

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
         (let* ((argument-types (compute-form-argument-types template-function form environment))
                (function-type (compute-function-type template-function argument-types))
                (instantiation-name (compute-name template-function argument-types)))
           (unless (processed-template-function-p template-function argument-types)
             ;; Insert a dummy function so that the compiler doesn't
             ;; complain about a missing function definition.
             (fmakunbound instantiation-name)
             (setf (fdefinition instantiation-name) (lambda (&rest args)
                                                      (declare (ignore args))
                                                      nil))
             (proclaim `(ftype ,function-type ,instantiation-name))
             (note-template-function template-function argument-types))
           (cons instantiation-name (specialization-store:compiler-macro-form-arguments form))))
        (t
         (specialization-store:expand-store (store template-function) form environment))))

(defmethod make-template-function-unbound ((template-function template-function))
  (specialization-store:make-store-unbound (store template-function))
  (let* ((name (name template-function)))
    (setf (compiler-macro-function name) nil)
    (fmakunbound name)))

(defmethod complete-argument-types ((template-function template-function) argument-types)
  (apply (funcall (type-completion-function template-function)
                  (lambda (&rest args)
                    args))
         argument-types))

(defmethod compute-name ((template-function template-function) argument-types)
  (apply (name-function template-function)
         (complete-argument-types template-function argument-types)))

(defmethod compute-lambda-form ((template-function template-function) argument-types)
  (apply (lambda-form-function template-function)
         (complete-argument-types template-function argument-types)))

(defmethod compute-function-type ((template-function template-function) argument-types)
  (apply (function-type-function template-function)
         (complete-argument-types template-function argument-types)))

(defmethod complete-argument-types* ((template-function template-function) &rest argument-types)
  (complete-argument-types template-function argument-types))

(defmethod compute-name* ((template-function template-function) &rest argument-types)
  (compute-name template-function argument-types))

(defmethod compute-lambda-form* ((template-function template-function) &rest argument-types)
  (compute-lambda-form template-function argument-types))

(defmethod compute-function-type* ((template-function template-function) &rest argument-types)
  (compute-function-type template-function argument-types))

(defmethod compute-form-argument-types ((template-function template-function) form &optional environment)
  (let* ((form-types-fn (slot-value (store template-function) 'specialization-store.standard-store::form-types-function))
         (type-completion-fn (specialization-store:store-type-completion-function (store template-function)))
         (fn (funcall form-types-fn
                      (funcall type-completion-fn
                               (lambda (&rest args)
                                 args)))))
    (funcall fn form environment)))

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

(defmethod compute-name ((name symbol) argument-types)
  (compute-name (find-template-function name) argument-types))

(defmethod compute-name* ((name symbol) &rest argument-types)
  (compute-name (find-template-function name) argument-types))

(defmethod compute-function-type ((name symbol) argument-types)
  (compute-function-type (find-template-function name) argument-types))

(defmethod compute-function-type* ((name symbol) &rest argument-types)
  (compute-function-type (find-template-function name) argument-types))

;;;; Glue Layer (Instantiation requests)

(defun %ensure-instantiations (requests)
  (labels ((process (template-function argument-types)
             (multiple-value-bind (lambda-form function-type) (compute-lambda-form template-function argument-types)
               (let* ((function-type (or function-type
                                         (compute-function-type template-function argument-types)))
                      (name (compute-name template-function argument-types))
                      (wrapper-fn (compile nil `(lambda ()
                                                  (alexandria:named-lambda ,name ,@(rest lambda-form)))))
                      (function (funcall wrapper-fn))
                      (expand-function (specialization-store:compiler-macro-lambda (&rest args)
                                         `(the ,(third function-type) (,name ,@args))))
                      (specialization-lambda-list (make-specialization-lambda-list (parameters template-function)
                                                                                   argument-types))
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
                                     :argument-types argument-types
                                     :name name
                                     :lambda-form lambda-form
                                     :function-type function-type
                                     :function function)))))
    (let* ((*instantiating* t)
           (*records* (loop
                        for (template-function argument-types) in requests
                        collect (make-record :template-function template-function
                                             :argument-types argument-types)))
           (*processed* nil))
      (loop
        for record = (pop *records*)
        until (null record)
        unless (processed-record-p record)
          do
             (push (process (record-template-function record)
                            (record-argument-types record))
                   *processed*))
      *processed*)))

(defun %ensure-instantiation (template-function argument-types)
  (%ensure-instantiations (list (list template-function argument-types))))

(defmethod ensure-instantiation ((template-function template-function) argument-types)
  (%ensure-instantiation template-function argument-types))

(defmethod ensure-instantiation ((name symbol) argument-types)
  (ensure-instantiation (find-template-function name) argument-types))

(defmethod ensure-instantiation* ((name symbol) &rest argument-types)
  (ensure-instantiation name argument-types))

(defmethod ensure-instantiation* ((template-function template-function) &rest argument-types)
  (%ensure-instantiation template-function argument-types)
  (values))

(defun %install-instantiation (template-function-name argument-types function)
  (let* ((template-function (find-template-function template-function-name))
         (name (compute-name template-function argument-types))
         (function-type (compute-function-type template-function argument-types))
         (specialization-lambda-list (make-specialization-lambda-list (parameters template-function)
                                                                      function-type))
         (specialization (make-instance 'specialization-store:standard-specialization
                                        :name name
                                        :lambda-list specialization-lambda-list
                                        :function function
                                        :value-type (third function-type))))
    (specialization-store:add-specialization template-function specialization)
    (setf (fdefinition name) function)
    (proclaim `(function ,function-type ,name))))


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
    (destructuring-bind (parameters globals) (specialization-store.lambda-lists:parameter-init-forms-as-global-functions
                                              (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)
                                              environment)
      (let* ((value-completion-function (or value-completion-function
                                            (specialization-store.lambda-lists:make-value-completion-lambda-form parameters)))
             (type-completion-function (or type-completion-function
                                           (specialization-store.lambda-lists:make-type-completion-lambda-form parameters environment))))
        `(progn
           ,@globals
           (ensure-template-function ',name ',lambda-list
                                     :environment environment
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
    (let* ((class (ensure-class (or (body-value :class)
                                    'template-function-class))))
      (define-template-using-object class
        :name name
        :lambda-list lambda-list
        :lambda-form-function (body-value :lambda-form-function t)
        :name-function (body-value :name-function)
        :type-completion-function (body-value :type-completion-function t)
        :value-completion-function (body-value :value-completion-function t)
        :environment env))))

;;;; Syntax Layer (Instantiation requests)

(defmacro require-instantiations (&rest pairs-of-name-and-argument-types)
  (let* ((pairs (loop
                  for (template-function-name . argument-types) in pairs-of-name-and-argument-types
                  collect (list (find-template-function template-function-name)
                                argument-types)))
         (instantiations (%ensure-instantiations pairs)))
    `(eval-when (:load-toplevel)
       ;; Generate the instantiations when a fasl is loaded.
       ,@(loop
           for instantiation in instantiations
           for template-function-name = (name (instantiation-template-function instantiation))
           for name = (instantiation-name instantiation)
           for lambda-form = (instantiation-lambda-form instantiation)
           for argument-types = (instantiation-argument-types instantiation)
           collect
           `(%install-instantiation ',template-function-name ',argument-types
                                    (alexandria:named-lambda ,name ,@(rest lambda-form)))))))

(defmacro require-instantiation ((template-function-name &rest argument-types))
  `(require-instantiations (,template-function-name ,@argument-types)))
