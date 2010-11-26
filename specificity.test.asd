;;;; specificity.test.asd

(asdf:defsystem #:specificity.test
  :depends-on (#:specificity)
  :components
  :in-order-to ((test-op (load-op specificity.test)))
  ((:module spec
            :serial t
            :components
            ((:file "package")
             (:file "specificity")))))
