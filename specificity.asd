;;;; specificity.asd

(asdf:defsystem #:specificity
  :depends-on (#:alexandria)
  :components
  :in-order-to ((test-op (load-op specificity.test)))
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "specificity")))))

