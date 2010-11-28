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
  (it "should capture its surrounding lexical environment in its body."
    (let ((value nil))
      (let ((example
             (let ((sentinel 'sentinel))
               (it "desc"
                 (setf value sentinel)))))
        (run-example example)
        (is (= 'sentinel value))))))

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
  (it "should return a sequence of results."
    (let ((example (make-example "description" (lambda () t))))
      (is (every #'resultp (run-example example)))))
  (it "should return multiple results when there are multiple expectations, in the order they were executed."
    (let* ((example (make-example "x" (lambda () (is t) (is nil) (pending "just chillin'"))))
           (results (run-example example)))
      (is (every #'resultp results))
      (is (successp (elt results 0)))
      (is (failurep (elt results 1)))
      (is (pendingp (elt results 2)))))
  (it "should finish even if the body of its function errors."
    (let ((example (make-example "description" (lambda () (error "Something went wrong.")))))
      (finishes (run-example example))))
  (it "should return a pending result in its results when the example has a null function."
    (let* ((example (make-example "description"))
           (result (elt (run-example example) 0)))
      (is (pendingp result)))))

;;;
;;; Expectations
;;;
(spec is
  (it "should add a success to its example's results when its body evaluates to TRUE."
    (let* ((example (make-example "is" (lambda () (is (= 1 1)))))
           (results (run-example example)))
      (is (successp (elt results 0)))))
  (it "should add a failure to its example's results when its body evaluates to FALSE."
    (let* ((example (make-example "is" (lambda () (is (= 1 2)))))
           (results (run-example example)))
      (is (failurep (elt results 0)))))
  (it "should add a failure to its example's results when its body signals an error."
    (let ((example (make-example "is" (lambda () (error "Failwhale"))))
          (results (run-example example)))
      (is (failurep (elt results 0)))))
  (it "should simply return its result object when called outside of an example's scope."))

(spec pending
  (it "should add a pending result to the example's results."
    (let* ((example (make-example "pending" (lambda () (pending))))
           (result (elt (run-example example) 0)))
      (is (pendingp result))))
  (it "should accept an argument to be used as its explanation."
    (let* ((explanation "some reason")
           (example (make-example "pending" (lambda () (pending explanation))))
           (result (elt (run-example example) 0)))
      (is (eq explanation (pending-explanation result))))))

(spec finishes
  (it "should report a success when its body executes completely."
    (let* ((example (make-example "finishes" (lambda () (finishes 1 2 3))))
           (result (elt (run-example example) 0)))
      (is (successp result))))
  (it "should fail when its body executes a non-local exit."))

(spec signals
  (it "should report a success when its body signals an unhandled condition of the declared type."
    (let* ((example (make-example "signals" (lambda () (signals error (error "foo")))))
           (result (elt (run-example example) 0)))
      (is (successp result))))
  (it "should report a failure when its body fails to signal the expected condition."
    (let* ((example (make-example "signals" (lambda () (signals error "Problem, officer?"))))
           (result (elt (run-example example) 0)))
      (is (failurep result))))
  (it "should report a success when its body signals a condition that is a subtype of the declared condition type."
    (let* ((example (make-example "signals" (lambda () (signals error (error 'type-error)))))
           (result (elt (run-example example) 0)))
      (is (successp result))))
  (it "should report a failure when its body signals a condition that is NOT a subtype of the declared condition type."
    (let* ((example (make-example "signals" (lambda () (signals error (warn "foo")))))
           (result (elt (run-example example) 0)))
      (is (failurep result)))))

;;;
;;; Results
;;;
(def-spec-group results :in specificity)
(in-spec-group results)

(spec resultp)
(spec successp)
(spec pendingp)
(spec pending-explanation)
(spec make-success)
(spec make-failure)
(spec make-pending)
