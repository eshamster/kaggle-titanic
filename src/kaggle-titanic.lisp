(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl)
  (:export :main
           :cross-validate)
  (:import-from :kaggle-titanic.data
                :make-my-path)
  (:import-from :kaggle-titanic.learner
                :learn
                :classify-result-id
                :classify-result-result
                :classify-result-certainty
                :classify-result-expected
                :do-classified-result)
  (:import-from :anaphora
                :it)
  (:import-from :alexandria 
                :iota))
(in-package :kaggle-titanic)

(defun classify (store test-path)
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
  (let ((store (nbayes:make-learned-store)))
    (learn store "resources/train.csv")
    (classify store "resources/test.csv"))
  t)

(defun cross-validate ()
  (let* ((k-cross 5)
         (use-ratio (/ 1 k-cross))
         (offset-ratio-lst (iota k-cross
                                 :start 0
                                 :step (/ 1 k-cross)))
         (success 0)
         (count 0))
    (dolist (test-offset-ratio offset-ratio-lst)
      (let ((store (nbayes:make-learned-store)))
        (dolist (learn-offset-ratio (remove test-offset-ratio offset-ratio-lst))
          (learn store "resources/train.csv"
                 :offset-ratio learn-offset-ratio
                 :use-ratio use-ratio))
        (do-classified-result store (class-result "resources/train.csv"
                                                  :offset-ratio test-offset-ratio
                                                  :use-ratio use-ratio)
          (let ((result (classify-result-result class-result))
                (expected (classify-result-expected class-result)))
            (when (eq result expected)
              (incf success))
            (incf count)))))
    (let* ((ave (float (/ success count)))
           (confidence (* 1.96 (sqrt (* ave (- 1 ave) (/ 1 count))))))
      (format t "~%~D/~D (~A +- ~A)~%" success count ave confidence))))
