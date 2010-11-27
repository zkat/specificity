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

(defun ensure-spec (name &optional description)
  (setf (gethash name *specs*) (make-instance 'spec :name name :description description)))

(defun remove-spec (name)
  (remhash name *specs*))

(defun run-spec (name-or-spec))

(defmacro it (&body body)
  (declare (ignore body))
  `(error "Can't call IT outside of SPEC."))

(defgeneric expectationp (maybe-expectation))
