(in-package :cl-user)
(defpackage kaggle-titanic.learner
  (:use :cl)
  (:export :learn
           :classify-result
           :classify-result-id
           :classify-result-result
           :classify-result-certainty
           :classify-result-expected
           :do-classified-result)
  (:import-from :kaggle-titanic.csv
                :convert-raw-data-one-line)
  (:import-from :kaggle-titanic.data
                :do-converted-line-data
                :add-name
                :extract-cabin
                :round-num
                :extract-miss-or-mrs)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :alexandria
                :with-gensyms))
(in-package :kaggle-titanic.learner)

(defun process-line (head-line line)
  (convert-raw-data-one-line head-line line
    ("PassengerId")
    ("Survived")
    ("Pclass" (add-name "class" it))
    ("Name" (extract-miss-or-mrs it))
    ("Sex")
    (("Sex" "Age") (add-name "Sex-Age" sex (round-num age 5)))
    ("Age" (add-name "Age" (round-num it 5)))
    (("SibSp" "Parch") (add-name "Sib-Par" sibsp parch))
    ("Parch" (add-name "Par" it))
    ("SibSp" (add-name "Sib" it))
                                        ; ("Fare" (add-name "Fare" (round-num it 50)))
    ("Ticket" (add-name "Ticket-pre" (ppcre:regex-replace " [0-9]*$" it "")))
    ("Cabin" (add-name "Cabin" (extract-cabin it)))
    ("Cabin" (add-name "Cabin-raw" it))
    ("Embarked" (add-name "Emb" it))))

(defun learn (store learn-path &key (offset-ratio 0) (use-ratio 1))
  (let ((count 0)
        (sampling-interval 200))
    (do-converted-line-data (line-lst learn-path
                                      :offset-ratio offset-ratio
                                      :use-ratio use-ratio
                                      :process-one-line process-line)
      (when (= (mod count sampling-interval) 0)
        (format t "~%Sample: ~D~%" line-lst))
      (nbayes:learn-a-document store
                               (remove-if #'null (cddr line-lst))
                               (cadr line-lst))
      (incf count))))

(defstruct classify-result
  id
  result
  certainty
  expected ; 0, 1 or NIL
  )

(defun classify (store line)
  (let ((result (make-classify-result))
        (raw-result (nbayes:sort-category-with-post-prob store
                                                         (remove-if #'null (cddr line)))))
    (setf (classify-result-id result) (car line))
    (setf (classify-result-expected result) (aif (cadr line)
                                                 (parse-integer it)))
    (setf (classify-result-result result) (parse-integer (caar raw-result)))
    (setf (classify-result-certainty result) (cdar raw-result))
    result))

(defmacro do-classified-result (store (result test-path &key
                                              (offset-ratio 0)
                                              (use-ratio 1))
                                &body body)
  (with-gensyms (line)
    `(do-converted-line-data (,line ,test-path
                                    :offset-ratio ,offset-ratio
                                    :use-ratio ,use-ratio
                                    :process-one-line process-line)
       (let ((,result (classify ,store ,line)))
         ,@body))))
