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

(defstruct classifier
  (store (nbayes:make-learned-store))
  (weight 0)
  process-line)

(defstruct ensembler
  (lst (list (make-classifier :process-line 'process-line :weight 0))))

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

(defun learn (learn-path &key (offset-ratio 0) (use-ratio 1) (store nil))
  (let ((count 0)
        (sampling-interval 200))
    (when (null store) (setf store (make-ensembler)))
    (dolist (classifier (ensembler-lst store))
      (do-converted-line-data (line-lst learn-path
                                        :offset-ratio offset-ratio
                                        :use-ratio use-ratio
                                        :process-one-line (classifier-process-line classifier))
        (when (= (mod count sampling-interval) 0)
          (format t "~%Sample: ~D~%" line-lst))
        (nbayes:learn-a-document (classifier-store classifier)
                                 (remove-if #'null (cddr line-lst))
                                 (cadr line-lst))
        (incf count))))
  store)

(defstruct classify-result
  id
  result
  certainty
  expected ; 0, 1 or NIL
  )

(defgeneric classify (store line))

(defmethod classify ((store classifier) line)
  (let ((result (make-classify-result))
        (raw-result (nbayes:sort-category-with-post-prob (classifier-store store)
                                                         (remove-if #'null (cddr line)))))
    (setf (classify-result-id result) (car line))
    (setf (classify-result-expected result) (aif (cadr line)
                                                 (parse-integer it)))
    (setf (classify-result-result result) (parse-integer (caar raw-result)))
    (setf (classify-result-certainty result) (cdar raw-result))
    result))

(defmethod classify ((store ensembler) line)
  (classify (car (ensembler-lst store)) line))

(defmacro do-classified-result (store (result test-path &key
                                              (offset-ratio 0)
                                              (use-ratio 1))
                                &body body)
  (with-gensyms (line classifier)
    `(let ((,classifier (car (ensembler-lst ,store))))
       (do-converted-line-data (,line ,test-path
                                      :offset-ratio ,offset-ratio
                                      :use-ratio ,use-ratio
                                      :process-one-line (classifier-process-line ,classifier))
         (let ((,result (classify ,store ,line)))
           ,@body)))))
