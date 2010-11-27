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

(defun find-spec (name))
(defun ensure-spec (name))
(defun remove-spec (name))
(defun run-spec (name-or-spec))

(defmacro it (&body body)
  (declare (ignore body))
  `(error "Can't call IT outside of SPEC."))

(defgeneric expectationp (maybe-expectation))
