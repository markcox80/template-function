(defsystem "template-function-tests"
  :author "Mark Cox"
  :description "Tests for the TEMPLATE-FUNCTION system."
  :depends-on ("template-function" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "template-function")
                             (:file "sandbox")
                             (:file "asdf")))))
