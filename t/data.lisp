(in-package :cl-user)
(defpackage kaggle-titanic-test.data
  (:use :cl
        :kaggle-titanic.data
        :kaggle-titanic.csv
        :kaggle-titanic-test.utils
        :prove))
(in-package :kaggle-titanic-test.data)

(plan 5)

(subtest
    "Test extract-miss-or-mrs"
  (is (extract-miss-or-mrs "Mrs. somebody") "Mrs" :test #'equal)
  (is (extract-miss-or-mrs "Miss. somebody") "Miss" :test #'equal)
  (is (extract-miss-or-mrs "rs. somebody") nil)
  (is (extract-miss-or-mrs "abc Mrs. anybody") "Mrs" :test #'equal))

(subtest
    "Test extract-cabin"
  (is (extract-cabin "A12 ee") "A" :test #'equal)
  (is (extract-cabin "a12 ee") nil))

(subtest
    "Test add-name"
  (is (add-name "Name" 1) "Name:1" :test #'equal)
  (is (add-name "Name" 1 "abc" 'test) "Name:1-abc-TEST" :test #'equal)
  (is (add-name "Name" nil) nil)
  (is (add-name "Name" nil 23) "Name:NIL-23" :test #'equal)
  (is (add-name "Name" nil nil nil nil) nil))

(subtest
    "Test round-num"
  (is (round-num "12" 5) 10)
  (is (round-num "16" 4) 16)
  (is (round-num "16.5" 4) 16)
  (is (round-num "6.5" 4 :scale 10) 64))

(defparameter *test-path* "t/test.csv")

(with-open-file (out (make-my-path *test-path*)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
  (format out "ab,cd,ef
a1,a2,a3
b1,b2,b3
c1,c2,c3
d1,d2,d3
e1,e2,e3"))

(subtest
    "Test do-converted-line-data"
  (labels ((convert (head-line line)
             (convert-raw-data-one-line head-line line
               ("ab")
               (("cd" "ef") (add-name "n" cd ef))
               ("not-included" it)))
           (make-test-list (offset-ratio use-ratio)
             (let ((result))
               (do-converted-line-data (line *test-path*
                                             :offset-ratio offset-ratio
                                             :use-ratio use-ratio
                                             :process-one-line #'convert)
                 (setf result (cons line result)))
               (reverse result))))
    (is (make-test-list 0 1)
        '(("a1" "n:a2-a3" nil)
          ("b1" "n:b2-b3" nil)
          ("c1" "n:c2-c3" nil)
          ("d1" "n:d2-d3" nil)
          ("e1" "n:e2-e3" nil))
        :test #'equal)
    (is (make-test-list 0.2 0.4)
        '(("b1" "n:b2-b3" nil)
          ("c1" "n:c2-c3" nil)
          ("d1" "n:d2-d3" nil))
        :test #'equal)))

(finalize)
