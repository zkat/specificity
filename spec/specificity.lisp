(in-package #:specificity)

(spec spec
  (it "should allow you to define a basic spec with just a name."
    (spec test-spec)
    (is (find-spec 'test-spec))
    (remove-spec 'test-spec))
  (it "should only accept symbols as spec names."
    (finishesp (spec test-spec))
    (signals 'error (spec "string-name"))
    (is (find-spec 'test-spec))
    (remove-spec 'test-spec))
  (it "should be able to redefine existing specs."
    (spec test-spec)
    (let ((old-spec (find-spec 'test-spec)))
      (spec test-spec)
      (is (not (eql old-spec (find-spec 'test-spec)))))))

(spec it
  (it "should be callable from within the SPEC macro."
    (finishesp (spec test-spec (it "should be callable from within the SPEC macro.")))
    (remove-spec 'test-spec))
  (it "should signal an error if called outside of the SPEC macro."
    (signalsp 'error (it "should signal an error if called outside of the SPEC macro.")))
  (it "must require a first argument describing its purpose, erroring if it does not receive one."
    (signalsp 'error (spec test-spec (it)))
    (remove-spec 'test-spec)
    (finishesp (spec test-spec (it "requires a description")))
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

(spec specp
  (it "should return true when passed a spec object."
    (spec test-spec)
    (is (specp (find-spec 'test-spec)))
    (remove-spec 'test-spec)))

(spec remove-spec
  (it "should remove specs, by name."
    (spec test-spec)
    (remove-spec 'test-spec)
    (is (null (find-spec 'test-spec))))
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

