;;;; specificity.asd

(asdf:defsystem #:specificity
  :depends-on (#:alexandria)
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "specificity")))))

