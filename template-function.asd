(defsystem "template-function"
  :author "Mark Cox"
  :description "A system for generating functions from a template."
  :depends-on ("specialization-store" "introspect-environment" "alexandria")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "name-mangling")
                             (:file "name-mangling-definitions")
                             (:file "template-function"))))
  :in-order-to ((test-op (test-op "template-function-tests"))))
