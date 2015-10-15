(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl)
  (:export :main)
  (:import-from :cl-csv
                :read-csv)
  (:import-from :alexandria
                :once-only)
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

(defun learn (head-line data-lines)
  (labels ((add-name (name value)
             (if (null value)
                 nil
                 (format nil "~A:~A" name value)))
           (round-num (target-str interval &key (scale 1))
             (if (= (length target-str) 0)
                 (return-from round-num nil))
             (* interval (round
                          (/ (* (read-from-string target-str) scale)
                             interval)))))
    (dolist (line data-lines)
      (print
       (convert-raw-data-one-line head-line line
         ("Survived" it)
         ("Pclass" (add-name "class" it))
         ("Sex" it)
         ("Age" (add-name "Age" (round-num it 5)))
         ("SibSp" (add-name "SibNum" it))
         ("Parch" (add-name "ParChNum" it))
         ("Fare" (add-name "Fare" (round-num it 5)))
         ("Embarked" (add-name "Emb" it)))))))

(defun main ()
  (let* ((data (read-csv (make-my-path "resources/train.csv")))
         (head-line (car data))
         (rest-line (subseq data 1 10)))
    (print head-line)
    (print rest-line)
    (format t "~%-------------------~%")
    (learn head-line rest-line))
  t)
