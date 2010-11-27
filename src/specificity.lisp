(in-package #:specificity)

(defgeneric spec-description (spec)
  (:documentation "Description for the spec, aka 'docstring'."))
(defgeneric spec-name (spec)
  (:documentation "Name for spec."))
(defgeneric spec-expectations (spec)
  (:documentation "Sequence of expectations."))
(defgeneric specp (maybe-spec))
(defmacro spec (name &body body)
  `(progn ,name ,@body))

(defparameter *specs* (make-hash-table :test 'eq))
(defun find-spec (name)
  (values (gethash name *specs*)))

(defclass spec ()
  ((name :initarg :name :reader spec-name)
   (description :initarg :description :reader spec-description)
   (expectations :initarg :expectations :reader spec-expectations))
  (:default-initargs :expectations nil))

(defun ensure-spec (name &key description expectations)
  (setf (gethash name *specs*) (make-instance 'spec :name name
                                              :description description
                                              :expectations expectations)))

(defun remove-spec (name)
  (remhash name *specs*))

(defun run-spec (name-or-spec)
  ;; TODO
  )

(defgeneric resultp (maybe-result))

(defclass result () ())

(defmethod resultp ((result result)) t)

(defmacro it (&body body)
  (declare (ignore body))
  `(error "Can't call IT outside of SPEC."))

;;;
;;; Examples
;;;
(defun make-example (description example-function)
  (make-instance 'example :description description :function example-function))

(defgeneric examplep (maybe-example)
  (:method ((example t)) nil)
  (:method ((example example)) t))

(defun run-example (example)
  ;; TODO
  )
(defgeneric example-description (example))
(defgeneric example-function (example)
  (:documentation "Compiled function that represents the example's executable body."))

(defclass example ()
  ((description :initarg :description :reader example-description)
   (function :initarg :function :reader example-function)))
