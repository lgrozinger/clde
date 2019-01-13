#|
  This file is a part of clde project.
|#

(defsystem "clde"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:cl-randist)
  :components ((:module "src"
                :components
                ((:file "clde"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "clde-test"))))
