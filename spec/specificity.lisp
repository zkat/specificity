(in-package #:specificity)

(def-spec-group specificity)
(def-spec-group spec :in specificity)

(in-spec-group spec)

(spec spec
  (it "should allow you to define a basic spec with just a name."
    (spec test-spec)
    (is (find-spec 'test-spec))
    (remove-spec 'test-spec))
  (it "should only accept symbols as spec names."
    (finishes (spec test-spec))
    (signals 'error (spec "string-name"))
    (is (find-spec 'test-spec))
    (remove-spec 'test-spec))
  (it "should be able to redefine existing specs."
    (spec test-spec)
    (let ((old-spec (find-spec 'test-spec)))
      (spec test-spec)
      (is (not (eql old-spec (find-spec 'test-spec)))))
    (remove-spec 'test-spec))
  (it "should allow an optional docstring right after the name."
    (spec test-spec "checkme")
    (is (string= "checkme" (description (find-spec 'test-spec))))
    (remove-spec 'test-spec)))

(spec specp
  (it "should return true when passed a spec object."
    (spec test-spec)
    (is (specp (find-spec 'test-spec)))
    (remove-spec 'test-spec)))

(spec ensure-spec
  (it "should define a spec."
    (ensure-spec 'test-spec)
    (is (specp (find-spec 'test-spec)))
    (remove-spec 'test-spec))
  (it "should define a spec with the name given."
    (ensure-spec 'test-spec)
    (is (eq 'test-spec (spec-name (find-spec 'test-spec))))
    (remove-spec 'test-spec)))

(spec find-spec
  (it "should return a spec object for existing specs."
    (spec test-spec)
    (is (specp (find-spec 'test-spec)))
    (remove-spec 'test-spec))
  (it "should return NIL when no such spec exists."
    (is (null (find-spec 'test-spec)))))

(spec remove-spec
  (it "should remove specs."
    (spec test-spec)
    (remove-spec 'test-spec)
    (is (null (find-spec 'test-spec))))
  (it "should error if something other than a symbol is given as the name."
    (spec test-spec)
    (signals 'error (remove-spec 1))
    (signals 'error (remove-spec "test-spec"))
    (signals 'error (remove-spec 1))
    (finishes (remove-spec 'test-spec)))
  (it "should return true when a spec with that name already exists."
    (spec test-spec)
    (is (remove-spec 'test-spec)))
  (it "should return false when there is no spec with that name."
    (is (not (remove-spec 'test-spec)))))

(spec run-spec
  (it "should be a function."
    (is (functionp #'run-spec)))
  (it "should return an object representing the execution results."
    (spec test-spec)
    (is (resultp (run-spec 'test-spec)))))

(spec spec-examples)

(def-spec-group results :in specificity)
(in-spec-group results)

(spec resultp)

(def-spec-group examples :in specificity)
(in-spec-group examples)

;;;
;;; Examples
;;;

(spec it
  (it "should return an example object."
    (is (examplep (it "description"))))
  (it "must require a first argument describing its purpose, erroring if it does not receive one."
    (signals 'error (it))
    (finishes (it "requires a description")))
  (it "should allow examples defined with a null body."
    (finishes (it "Spec goes here.")))
  (it "must use the first argument as the example's description."
    (spec test-spec (it "checkme"))
    (let ((example (elt (spec-examples (find-spec 'test-spec)) 0)))
      (is (string= "checkme" (description example))))
    (remove-spec 'test-spec)))

(spec make-example
  (it "should return an example object."
    (is (examplep (make-example "foo" (lambda () t)))))
  (it "should require a description."
    (signals error (make-example))
    (finishes (make-example "foo")))
  (it "should use its first argument as the example's description."
    (let* ((description "desc")
           (example (make-example "desc")))
      (is (eq description (example-description example)))))
  (it "should accept an optional second argument which it will use as the example's function."
    (is (examplep (make-example "foo")))
    (let* ((function (lambda () t))
           (example (make-example "foo" function)))
      (is (eq function (example-function example)))))
  (it "should create an object that holds the description and function given to it."
    (let ((description "foo")
          (function (lambda () t)))
      (let ((example (make-example description function)))
        (is (eq description (example-description example)))
        (is (eq function (example-function example)))))))

(spec example-description
  (it "should return the example's description."
    (let* ((description "foo")
           (example (make-example description (lambda () t))))
      (is (eq description (example-description example))))))

(spec example-function
  (it "should return the example's function."
    (let* ((function (lambda () t))
           (example (make-example "foo" function)))
      (is (eq function (example-function example))))))

(spec examplep
  (it "should return true when passed an example object."
    (let ((example (make-example "foo" (lambda () t))))
      (is (examplep example)))))

(spec run-example
  (it "should allow you to execute an example object."
    (let ((example (make-example "description" (lambda () t))))
      (finishes (run-example example))))
  (it "should return an object representing the results of executing the example."
    (let ((example (make-example "description" (lambda () t))))
      (is (resultp (run-example example)))))
  (it "should finish even if the body of its function errors."
    (let ((example (make-example "description" (lambda () (error "Something went wrong.")))))
      (finishes (run-example example)))))

;;;
;;; Results
;;;
