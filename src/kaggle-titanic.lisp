(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl)
  (:export :main
           :validate)
  (:import-from :kaggle-titanic.data
                :make-my-path)
  (:import-from :kaggle-titanic.learner
                :learn
                :classify-result-id
                :classify-result-result
                :classify-result-certainty
                :classify-result-expected
                :do-classified-result
                :cross-validate)
  (:import-from :anaphora
                :it)
  (:import-from :alexandria 
                :iota))
(in-package :kaggle-titanic)

(defun classify-test-data (store test-path)
  (with-open-file (out (make-my-path "resources/result.csv")
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :overwrite)
    (format out "PassengerId,Survived~%")
    (let ((count 0)
          (sum-first-post-prob 0))
      (do-classified-result store (result test-path)
        (format out "~D,~D~%" (classify-result-id result) (classify-result-result result))
        (incf count)
        (incf sum-first-post-prob (classify-result-certainty result)))
      (format t "~%Average: ~A~%" (/ sum-first-post-prob count)))))

(defun main ()
  (let ((store (learn "resources/train.csv")))
    (classify-test-data store "resources/test.csv"))
  t)

(defun validate ()
  (multiple-value-bind (ave confidence)
      (cross-validate "resources/train.csv")
    (format t "~A (+-~A)~%" ave confidence)))
