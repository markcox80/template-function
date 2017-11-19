(defsystem "template-function"
  :author "Mark Cox"
  :description "A system for generating functions from a template."
  :depends-on ("specialization-store" "introspect-environment" "alexandria")
  :license "Simplified BSD License variant"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "name-mangling")
                             (:file "name-mangling-definitions")
                             (:file "argument-specification")
                             (:file "template-function"))))
  :in-order-to ((test-op (test-op "template-function-tests"))))
