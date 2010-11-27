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
    (is (run-spec 'test-spec))))

(def-spec-group expectations :in specificity)
(in-spec-group expectations)

(spec it
  (it "should be callable from within the SPEC macro."
    (finishes (spec test-spec (it "should be callable from within the SPEC macro.")))
    (remove-spec 'test-spec))
  (it "should signal an error if called outside of the SPEC macro."
    (signalsp 'error (it "should signal an error if called outside of the SPEC macro.")))
  (it "must require a first argument describing its purpose, erroring if it does not receive one."
    (signals 'error (spec test-spec (it)))
    (remove-spec 'test-spec)
    (finishes (spec test-spec (it "requires a description")))
    (remove-spec 'test-spec))
  (it "should allow expectations defined with a null body."
    (finishes (spec test-spec (it "Spec goes here.")))
    (remove-spec 'test-spec))
  (it "must define an expectation for the spec."
    (spec test-spec (it "is expected"))
    (is (expectationp (elt (spec-expectations (find-spec 'test-spec)) 0)))
    (remove-spec 'test-spec))
  (it "must use the first argument as the expectation's description."
    (spec test-spec (it "checkme"))
    (let ((expectation (elt (spec-expectations (find-spec 'test-spec)) 0)))
      (is (string= "checkme" (description expectation))))
    (remove-spec 'test-spec)))

(spec run-expectation)

(spec expectation-lambda
  (it "should return a function."
    (spec test-spec (is "an expectation"))
    (let* ((spec (find-spec 'test-spec))
           (expectation (elt (spec-expectations spec) 0)))
      (is (functionp (expectation-lambda expectation))))
    (remove-spec 'test-spec))
  (it "should capture its definition environment."
    (let ((value nil))
      (let ((x 'sentinel))
        (spec test-spec (is "an expectation" (setf value x))))
      (let ((spec (find-spec 'test-spec))
            (expectation (elt (spec-expectations spec) 0)))
        (funcall (expectation-lambda expectation))
        (is (eq 'sentinel value))))
    (remove-spec 'test-spec)))

(spec expectation-results)

(spec expectationp
  (it "should return true when passed an expectation object."
    (spec test-spec)
    (let ((expectation (elt (spec-expectations (find-spec 'test-spec)) 0)))
      (is (expectationp expectation)))
    (remove-spec 'test-spec)))
