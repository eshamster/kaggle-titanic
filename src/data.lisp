(in-package :cl-user)
(defpackage kaggle-titanic.data
  (:use :cl)
  (:export :do-converted-line-data)
  (:import-from :kaggle-titanic.csv
                :convert-raw-data-one-line
                :it)
  (:import-from :cl-csv
                :read-csv)
  (:import-from :alexandria
                :with-gensyms))
(in-package :kaggle-titanic.data)

(defun make-my-path (rel-path)
  (merge-pathnames
   rel-path
   (directory-namestring
    (asdf:system-source-file
     (asdf:find-system :kaggle-titanic)))))

(defun extract-miss-or-mrs (name)
  (multiple-value-bind (replaced found)
      (ppcre:regex-replace "^.*(Miss|Mrs).*$" name "\\1")
    (if found replaced nil)))

(defun extract-cabin (cabin)
  (if cabin
      (ppcre:scan-to-strings "[A-Z]" cabin)))

(defun add-name (name &rest values)
  (labels ((join-values (rest result needs-hyphen)
             (when (null rest)
               (return-from join-values result))
             (join-values (cdr rest)
                          (format nil "~A~A~A" result (if needs-hyphen "-" "") (car rest))
                          t)))
    (if (every #'(lambda (value)
                   (or (null value) (equal value "")))
               values)
        nil
        (format nil "~A:~A" name (join-values values "" nil)))))

(defun round-num (target-str interval &key (scale 1))
  (if (= (length target-str) 0)
      (return-from round-num nil))
  (* interval (round
               (/ (* (read-from-string target-str) scale)
                  interval))))

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


(defmacro do-converted-line-data ((value data-path &key (offset-ratio 0) (use-ratio 1) (process-line #'process-line)) &body body)
  (with-gensyms (data head-line data-lines line offset max-use count)
    `(let* ((,data (read-csv (make-my-path ,data-path)))
            (,head-line (car ,data))
            (,data-lines (cdr ,data))
            (,offset (round (* (length ,data-lines) (max 0 ,offset-ratio))))
            (,max-use (round (* (length ,data-lines) (min 1 (+ ,offset-ratio ,use-ratio)))))
            (,count -1))
       (dolist (,line ,data-lines)
         (incf ,count)
         (when (and (<= ,offset ,count)
                    (<= ,count ,max-use))
           (let ((,value
                  (remove-if
                   #'null
                   (funcall process-line ,head-line ,line))))
             ,@body))))))

