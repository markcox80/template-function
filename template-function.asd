(defsystem "template-function"
  :author "Mark Cox"
  :description "A system for generating functions from a template."
  :depends-on ("specialization-store")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages"))))
  :in-order-to ((test-op (test-op "template-function-tests"))))
