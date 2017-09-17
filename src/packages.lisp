(defpackage "TEMPLATE-FUNCTION"
  (:use "COMMON-LISP")

  ;; Argument specification
  (:export "&OTHERS"
           "ARGUMENT-SPECIFICATION-LAMBDA"
           "NAMED-ARGUMENT-SPECIFICATION-LAMBDA"
           "DESTRUCTURING-ARGUMENT-SPECIFICATION"
           "DEFUN/ARGUMENT-SPECIFICATION")

  ;; Object Layer
  (:export "TEMPLATE-FUNCTION-CLASS"
           "TEMPLATE-FUNCTION"
           "NAME"
           "LAMBDA-LIST"
           "LAMBDA-FORM"
           "FORM-FUNCTION-TYPE"
           "INLINEP"

           "COMPUTE-NAME"
           "COMPUTE-NAME*"
           "COMPUTE-LAMBDA-FORM"
           "COMPUTE-LAMBDA-FORM*"
           "COMPUTE-FUNCTION-TYPE"
           "COMPUTE-FUNCTION-TYPE*"
           "COMPLETE-ARGUMENT-SPECIFICATION"
           "COMPLETE-ARGUMENT-SPECIFICATION*"

           "FUNCALL-TEMPLATE-FUNCTION"
           "APPLY-TEMPLATE-FUNCTION"
           "EXPAND-TEMPLATE-FUNCTION"
           "COMPUTE-FORM-ARGUMENT-TYPES"

           "INSTANTIATIONS"
           "INSTANTIATION"
           "INSTANTIATION-NAME"
           "INSTANTIATION-ARGUMENT-SPECIFICATION"
           "INSTANTIATION-LAMBDA-FORM"
           "INSTANTIATION-FUNCTION-TYPE"
           "ADD-INSTANTIATION"
           "REMOVE-INSTANTIATION"

           "FIND-INSTANTIATION"
           "FIND-INSTANTIATION*")

  ;; Glue Layer
  (:export "FIND-TEMPLATE-FUNCTION"
           "ENSURE-TEMPLATE-FUNCTION"
           "ENSURE-INSTANTIATION"
           "ENSURE-INSTANTIATION*"

           "MAKE-TEMPLATE-FUNCTION-UNBOUND")

  ;; Syntax Layer
  (:export "DEFINE-TEMPLATE-USING-OBJECT"
           "DEFINE-TEMPLATE"
           "REQUIRE-INSTANTIATION"
           "REQUIRE-INSTANTIATIONS"))

(defpackage "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION"
  (:use "COMMON-LISP")
  (:import-from "TEMPLATE-FUNCTION"
                "&OTHERS"
                "ARGUMENT-SPECIFICATION-LAMBDA"
                "NAMED-ARGUMENT-SPECIFICATION-LAMBDA"
                "DESTRUCTURING-ARGUMENT-SPECIFICATION"
                "DEFUN/ARGUMENT-SPECIFICATION")

  ;; Parameter Protocol
  (:export "PARAMETER"
           "PARAMETERP"
           "PARAMETER-VAR"
           "PARAMETER-KEYWORD"
           "PARAMETER-INIT-FORM"
           "PARAMETER-VARP"

           "WHOLE-PARAMETER"
           "WHOLE-PARAMETER-P"
           "REQUIRED-PARAMETER"
           "REQUIRED-PARAMETER-P"
           "OPTIONAL-PARAMETER"
           "OPTIONAL-PARAMETER-P"
           "OTHERS-PARAMETER"
           "OTHERS-PARAMETER-P"
           "REST-PARAMETER"
           "REST-PARAMETER-P"
           "KEYWORD-PARAMETER"
           "KEYWORD-PARAMETER-P")

  ;; Parameters Protocol
  (:export "LAMBDA-LIST-PARAMETERS"
           "ALL-PARAMETERS"
           "WHOLE-PARAMETER"
           "REQUIRED-PARAMETERS"
           "OPTIONAL-PARAMETERS"
           "OTHERS-PARAMETER"
           "REST-PARAMETER"
           "KEYWORD-PARAMETERS-P"
           "KEYWORD-PARAMETERS"
           "ALLOW-OTHER-KEYWORDS-P")

  (:export "PARSE-LAMBDA-LIST"
           "&OTHERS"
           "PARSE-LAMBDA-LIST-ERROR"
           "PARSE-LAMBDA-LIST-ERROR-LAMBDA-LIST"
           "PARSE-LAMBDA-LIST-ERROR-MESSAGE"
           "DUPLICATE-VARIABLE-ERROR"
           "DUPLICATE-VARIABLE-ERROR-VARIABLE")

  ;; argument-specification-lambda
  (:export "ARGUMENT-SPECIFICATION-LAMBDA"
           "ARGUMENT-SPECIFICATION-LAMBDA-ERROR"
           "NAMED-ARGUMENT-SPECIFICATION-LAMBDA"
           "DEFUN/ARGUMENT-SPECIFICATION"))

(defpackage "TEMPLATE-FUNCTION.NAMES")
