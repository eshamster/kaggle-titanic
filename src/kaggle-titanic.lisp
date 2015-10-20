(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl)
  (:export :main
           :cross-validate)
  (:import-from :kaggle-titanic.data
                :do-converted-line-data)
  (:import-from :anaphora
                :it)
  (:import-from :alexandria 
                :iota))
(in-package :kaggle-titanic)

(defun learn (store learn-path &optional (offset-ratio 0) (use-ratio 1))
  (let ((count 0)
        (sampling-interval 200))
    (do-converted-line-data (line-lst learn-path
                                      :offset-ratio offset-ratio
                                      :use-ratio use-ratio)
      (when (= (mod count sampling-interval) 0)
        (format t "~%Sample: ~D~%" line-lst))
      (nbayes:learn-a-document store (cddr line-lst) (cadr line-lst))
      (incf count))))

(defun classify (store test-path)
  (with-open-file (out (make-my-path "resources/result.csv")
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :overwrite)
    (format out "PassengerId,Survived~%")
    (let ((count 0)
          (sum-first-post-prob 0))
      (do-converted-line-data (line-lst test-path)
        (let ((sorted (nbayes:sort-category-with-post-prob store (cdr line-lst))))
          (format out "~D,~D~%" (car line-lst) (caar sorted))
          (incf count)
          (incf sum-first-post-prob (cdar sorted))))
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
          (learn store "resources/train.csv" learn-offset-ratio use-ratio))
        (do-converted-line-data (line-lst "resources/train.csv"
                                          :offset-ratio test-offset-ratio
                                          :use-ratio use-ratio)
          (let ((result (car (nbayes:sort-category-by-prob store (cddr line-lst))))
                (expected (cadr line-lst)))
            (when (equal result expected)
              (incf success))
            (incf count)))))
    (let* ((ave (float (/ success count)))
           (confidence (* 1.96 (sqrt (* ave (- 1 ave) (/ 1 count))))))
      (format t "~%~D/~D (~A +- ~A)~%" success count ave confidence))))
