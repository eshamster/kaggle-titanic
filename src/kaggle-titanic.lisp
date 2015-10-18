(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl)
  (:export :main)
  (:import-from :cl-csv
                :read-csv
                :write-csv)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:import-from :anaphora
                :aif
                :it))
(in-package :kaggle-titanic)

(defun make-my-path (rel-path)
  (merge-pathnames
   rel-path
   (directory-namestring
    (asdf:system-source-file
     (asdf:find-system :kaggle-titanic)))))

(defun find-target-value (target head-line data-line)
  (aif (position target head-line :test #'equal)
       (nth it data-line)))

(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    `(list ,@(mapcar (lambda (process)
                       `((lambda (it)
                           ,(cadr process))
                         (find-target-value ,(car process) ,head-line ,data-line)))
                     body))))

(defmacro do-converted-line-data ((value data-path) &body body)
  (with-gensyms (data head-line data-lines line)
    `(labels ((add-name (name value)
                (if (null value)
                    nil
                    (format nil "~A:~A" name value)))
              (round-num (target-str interval &key (scale 1))
                (if (= (length target-str) 0)
                    (return-from round-num nil))
                (* interval (round
                             (/ (* (read-from-string target-str) scale)
                                interval)))))
       (let* ((,data (read-csv (make-my-path ,data-path)))
              (,head-line (car ,data))
              (,data-lines (cdr ,data)))
         (dolist (,line ,data-lines)
           (let ((,value (remove-if #'null
                                    (convert-raw-data-one-line ,head-line ,line
                                      ("PassengerId" it)
                                      ("Survived" it)
                                      ("Pclass" (add-name "class" it))
                                      ("Sex" it)
                                      ("Age" (add-name "Age" (round-num it 5)))
                                      ("SibSp" (add-name "SibNum" it))
                                      ("Parch" (add-name "ParChNum" it))
                                      ("Fare" (add-name "Fare" (round-num it 5)))
                                      ("Embarked" (add-name "Emb" it))))))
             ,@body))))))

(defun learn (store learn-path)
  (do-converted-line-data (line-lst learn-path)
    (nbayes:learn-a-document store (cddr line-lst) (cadr line-lst)))
  (print store))

(defun classify (store test-path)
  (with-open-file (out (make-my-path "resources/result.csv")
                       :direction :output
                       :if-exists :overwrite)
    (format out "PassengerId,Survived~%")
    (do-converted-line-data (line-lst test-path)
      (format out "~D,~D~%"
              (car line-lst)
              (car (nbayes:sort-category-by-prob store (cdr line-lst)))))))

(defun main ()
  (let ((store (nbayes:make-learned-store)))
    (learn store "resources/train.csv")
    (classify store "resources/test.csv"))
  t)
