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

