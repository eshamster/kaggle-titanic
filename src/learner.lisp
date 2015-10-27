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
  (lst (list (make-classifier :process-line 'process-line-sample1 :weight 0.8)
             (make-classifier :process-line 'process-line-sample2 :weight 0.2))))

(defmacro def-simple-converter (&rest target-lst)
  (with-gensyms (header line)
    `(lambda (,header ,line)
       (convert-raw-data-one-line ,header ,line
         ,@(mapcar #'list target-lst)))))

(defun process-line-sample1 (header line)
  (funcall (def-simple-converter "Pclass" "Sex" "Sex-Age" "Age") header line))

(defun process-line-sample2 (header line)
  (funcall (def-simple-converter "Pclass" "Fare" "Cabin") header line))

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
    ("Fare" (add-name "Fare" (round-num it 50)))
    ("Ticket" (add-name "Ticket-pre" (ppcre:regex-replace " [0-9]*$" it "")))
    ("Cabin" (add-name "Cabin" (extract-cabin it)))
    ("Cabin" (add-name "Cabin-raw" it))
    ("Embarked" (add-name "Emb" it))))

(defun learn-a-classifier (classifier category line header)
  (let ((converted-line (funcall (classifier-process-line classifier) header line)))
    (nbayes:learn-a-document (classifier-store classifier)
                             (remove-if #'null converted-line)
                             category)))

(defun learn (learn-path &key (offset-ratio 0) (use-ratio 1) (store nil))
  (let ((count 0)
        (sampling-interval 200))
    (when (null store) (setf store (make-ensembler)))
    (do-converted-line-data ((line-lst header) learn-path
                             :offset-ratio offset-ratio
                             :use-ratio use-ratio
                             :process-one-line #'process-line)
      (when (= (mod count sampling-interval) 0)
        (format t "~%Sample: ~D~%" line-lst))
      (dolist (classifier (ensembler-lst store))
        (learn-a-classifier classifier (cadr line-lst)
                            (cddr line-lst) (cddr header)))
      (incf count)))
  store)

(defstruct classify-result
  id
  result
  certainty
  expected ; 0, 1 or NIL
  )

(defgeneric classify (store line header))

(defmethod classify ((store classifier) line header)
  (let ((result (make-classify-result))
        (raw-result (nbayes:sort-category-with-post-prob
                     (classifier-store store) 
                     (remove-if #'null (funcall (classifier-process-line store)
                                                (cddr header)
                                                (cddr line))))))
    (setf (classify-result-id result) (car line))
    (setf (classify-result-expected result) (aif (cadr line)
                                                 (parse-integer it)))
    (setf (classify-result-result result) (parse-integer (caar raw-result)))
    (setf (classify-result-certainty result) (cdar raw-result))
    result))

(defmethod classify ((store ensembler) line header)
  (let ((result (make-classify-result :certainty 0))
        (sum-weight 0))
    (with-slots (id result certainty expected) result
      (dolist (classifier (ensembler-lst store))
        (let ((mid-result (classify classifier line header)))
          (setf id (classify-result-id mid-result))
          (setf expected (classify-result-expected mid-result))
          (incf certainty
                (* (classifier-weight classifier)
                   (if (eq (classify-result-result mid-result) 1)
                       (classify-result-certainty mid-result)
                       (- 1 (classify-result-certainty mid-result)))))
          (incf sum-weight (classifier-weight classifier))))
      (setf certainty (/ certainty sum-weight))
      (setf result (if (> certainty 0.5) 1 0)))
    result))

(defmacro do-classified-result (store (result test-path &key
                                              (offset-ratio 0)
                                              (use-ratio 1))
                                &body body)
  (with-gensyms (line header classifier)
    `(let ((,classifier (car (ensembler-lst ,store))))
       (do-converted-line-data ((,line ,header) ,test-path
                                :offset-ratio ,offset-ratio
                                :use-ratio ,use-ratio
                                :process-one-line #'process-line)
         (let ((,result (classify ,store ,line ,header)))
           ,@body)))))
