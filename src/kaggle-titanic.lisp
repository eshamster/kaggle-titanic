(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl
        :cl-ppcre)
  (:export :main)
  (:import-from :cl-csv
                :read-csv
                :write-csv)
  (:import-from :alexandria
                :once-only
                :with-gensyms
                :compose)
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
    `(list ,@(mapcar
              (lambda (process)
                (let ((arg-lst (if (listp (car process))
                                   (car process)
                                   '("it")))
                      (find-lst (if (listp (car process))
                                    (car process)
                                    (list (car process)))))
                  `((lambda (,@(mapcar (compose #'intern #'string-upcase) arg-lst))
                      ,@(cdr process))
                    ,@(mapcar
                       (lambda (arg) 
                         `(find-target-value ,arg ,head-line ,data-line))
                       find-lst))))
                     body))))

(defun extract-miss-or-mrs (name)
  (multiple-value-bind (replaced found)
      (ppcre:regex-replace "^.*(Miss|Mrs).*$" name "\\1")
    (if found replaced nil)))

(defun extract-cabin (cabin)
  (if cabin
      (ppcre:scan-to-strings "[A-Z]" cabin)))

(defmacro do-converted-line-data ((value data-path) &body body)
  (with-gensyms (data head-line data-lines line)
    `(labels ((add-name (name value)
                (if (or (null value) (equal value ""))
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
           (let ((,value
                  (remove-if
                   #'null
                   (convert-raw-data-one-line ,head-line ,line
                     ("PassengerId" it)
                     ("Survived" it)
                     ("Pclass" (add-name "class" it))
                     ("Name" (extract-miss-or-mrs it))
                     (("Sex" "Age") (add-name "Sex-Age"
                                              (format nil "~A~A"
                                                      sex
                                                      (round-num age 5))))
                     ("SibSp" (add-name "SibNum" it))
                     ("Parch" (add-name "ParChNum" it))
                     ("Fare" (add-name "Fare" (round-num it 5)))
                     ("Cabin" (add-name "Cabin" (extract-cabin it)))
                     ("Cabin" (add-name "Cabin-raw" it))
                     ("Embarked" (add-name "Emb" it))))))
             ,@body))))))

(defun learn (store learn-path)
  (let ((count 0)
        (sampling-interval 100))
    (do-converted-line-data (line-lst learn-path)
      (when (= (mod count sampling-interval) 0)
        (format t "~%Sample: ~D~%" line-lst))
      (nbayes:learn-a-document store (cddr line-lst) (cadr line-lst))
      (incf count)))
  (print store))

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
