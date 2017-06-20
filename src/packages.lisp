(defpackage "TEMPLATE-FUNCTION"
  (:use "COMMON-LISP")

  ;; Object Layer
  (:export "TEMPLATE-FUNCTION-CLASS"
           "TEMPLATE-FUNCTION"
           "NAME"
           "LAMBDA-LIST"
           "LAMBDA-FORM"
           "FORM-FUNCTION-TYPE"
           "TYPE-COMPLETION-FUNCTION"
           "VALUE-COMPLETION-FUNCTION"

           "COMPUTE-NAME"
           "COMPUTE-NAME*"
           "COMPUTE-LAMBDA-FORM"
           "COMPUTE-LAMBDA-FORM*"
           "COMPUTE-FUNCTION-TYPE"
           "COMPUTE-FUNCTION-TYPE*"
           "COMPLETE-ARGUMENT-TYPES"
           "COMPLETE-ARGUMENT-TYPES*"

           "FUNCALL-TEMPLATE-FUNCTION"
           "APPLY-TEMPLATE-FUNCTION"
           "EXPAND-TEMPLATE-FUNCTION"
           "COMPUTE-FORM-ARGUMENT-TYPES"

           "FIND-INSTANTIATION"
           "FIND-INSTANTIATION*")

  ;; Glue Layer
  (:export "FIND-TEMPLATE-FUNCTION"
           "ENSURE-TEMPLATE-FUNCTION"
           "ENSURE-INSTANTIATION"
           "ENSURE-INSTANTIATION*")

  ;; Syntax Layer
  (:export "DEFINE-TEMPLATE-USING-OBJECT"
           "DEFINE-TEMPLATE"
           "REQUIRE-INSTANTIATION"
           "REQUIRE-INSTANTIATIONS"
           "&OTHERS"))

(defpackage "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION"
  (:use "COMMON-LISP")
  (:import-from "TEMPLATE-FUNCTION"
                "&OTHERS")

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
           "DUPLICATE-VARIABLE-ERROR-VARIABLE"))

(defpackage "TEMPLATE-FUNCTION.NAMES")
