(in-package :cl-user)
(defpackage kaggle-titanic-test
  (:use :cl
        :kaggle-titanic
        :prove))
(in-package :kaggle-titanic-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kaggle-titanic)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
