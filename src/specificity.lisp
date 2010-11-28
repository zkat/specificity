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

;;;
;;; Examples
;;;
(defmacro it (description &body body)
  `(make-example ,description ,@(when body `((lambda () ,@body)))))

(defgeneric example-description (example))
(defgeneric example-function (example)
  (:documentation "Compiled function that represents the example's executable body."))

(defgeneric examplep (maybe-example)
  (:method ((example t)) nil))

(defvar *results*)
(defun run-example (example)
  (let ((*results* nil))
    (handler-case (funcall (example-function example))
      (error (e) (push (make-failure example e) *results*)))
    *results*))

(defclass example ()
  ((description :initarg :description :reader example-description)
   (function :initarg :function :reader example-function)))

(defmethod examplep ((x example)) t)

(defun make-example (description &optional example-function)
  (make-instance 'example :description description :function example-function))

;;;
;;; Expectations
;;;
(defmacro is (&body body))
(defmacro pending (&body body))
(defmacro finishes (&body body))
(defmacro signals (&body body))

;;;
;;; Results
;;;
(defgeneric resultp (maybe-result)
  (:method ((else t)) nil))
(defgeneric successp (maybe-success))
(defgeneric failurep (maybe-failure))
(defgeneric pendingp (maybe-pending))
(defgeneric pending-explanation (pending))

(defgeneric make-success (example))
(defgeneric make-failure (example reason))
(defgeneric make-pending (example explanation))
