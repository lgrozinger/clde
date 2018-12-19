#|
  This file is a part of clde project.
|#

(defsystem "clde-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("clde"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "clde"))))
  :description "Test system for clde"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
