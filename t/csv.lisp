(in-package :cl-user)
(defpackage kaggle-titanic-test.csv
  (:use :cl
        :kaggle-titanic.csv
        :kaggle-titanic-test.utils
        :prove))
(in-package :kaggle-titanic-test.csv)

(plan nil)

(subtest
    "Test $:find-target-value"
  (is ($:find-target-value "test" '("abc" "test" "def") '(1 2 3))
      2)
  (is ($:find-target-value "test" '("abc" "def" "ghi") '(1 2 3))
      nil))

(subtest
    "Test $:equal-name"
  (ok (not ($:equal-name 1 1)))
  (ok (not ($:equal-name "test" "test")))
  (ok (not ($:equal-name 'test 'not-test)))
  (ok (not ($:equal-name 'test "test")))
  (ok ($:equal-name 'test 'test))
  (ok ($:equal-name 'test :test)))

(subtest
    "Test $:re-intern-symbols"
  (is ($:re-intern-symbols '(:a :b (:c :d) :e :f) '(b c e))
      '(:a b (c :d) e :f)
      :test #'equal))

(subtest
    "Test convert-raw-data-one-line"
  (is (convert-raw-data-one-line
          '("ab" "cd" "ef" "gh")
          '(1 2 3 4)
        ("ab" it)
        ("ab" (* 2 it))
        (("ab" "ef") (+ ab :ef))
        ("gh")
        ("not-found" it))
      '(1 2 4 4 nil)
      :test #'equal))

(finalize)
