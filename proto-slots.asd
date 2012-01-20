
(asdf:defsystem "proto-slots"
  :description "proto-slots: Prototypal inheritance for the CLOS"
  :version "1.0.0"
  :author "John Croisant <john@croisant.net>"
  :license "X11/MIT"
  :components
  ((:file "package")
   (:file "proto-slots" :depends-on ("package"))
   (:module "strategies"
            :depends-on ("proto-slots")
            :components
            ((:file "simple")
             (:file "unique-merge")
             (:file "hash-merge")))))
