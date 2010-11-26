;;;; specificity.test.asd

(asdf:defsystem #:specificity.test
  :depends-on (#:alexandria)
  :components
  :in-order-to ((test-op (load-op specificity.test)))
  ((:module test
            :serial t
            :components
            ((:file "package")
             (:file "specificity")))))
