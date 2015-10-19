(in-package :cl-user)
(defpackage kaggle-titanic
  (:use :cl
        :cl-ppcre)
  (:export :main
           :cross-validate)
  (:import-from :cl-csv
                :read-csv
                :write-csv)
  (:import-from :alexandria
                :once-only
                :with-gensyms
                :compose
                :iota)
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

(defun add-name (name value)
  (if (or (null value) (equal value ""))
      nil
      (format nil "~A:~A" name value)))

(defun round-num (target-str interval &key (scale 1))
  (if (= (length target-str) 0)
      (return-from round-num nil))
  (* interval (round
               (/ (* (read-from-string target-str) scale)
                  interval))))

(defmacro do-converted-line-data ((value data-path &key (offset-ratio 0) (use-ratio 1)) &body body)
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
                   (convert-raw-data-one-line ,head-line ,line
                     ("PassengerId" it)
                     ("Survived" it)
                     ("Pclass" (add-name "class" it))
                     ("Name" (extract-miss-or-mrs it))
                     ("Sex" it)
                     (("Sex" "Age") (add-name "Sex-Age"
                                              (format nil "~A~A"
                                                      sex
                                                      (round-num age 5))))
                     ("Age" (add-name "Age" (round-num it 5)))
                     (("SibSp" "Parch") (add-name "Sib-Par"
                                                  (format nil "~A-~A" sibsp parch)))
                     ("Parch" (add-name "Par" it))
                     ("SibSp" (add-name "Sib" it))
                     ; ("Fare" (add-name "Fare" (round-num it 10)))
                     ("Cabin" (add-name "Cabin" (extract-cabin it)))
                     ("Cabin" (add-name "Cabin-raw" it))
                     ("Embarked" (add-name "Emb" it))))))
             ,@body))))))

(defun learn (store learn-path &optional (offset-ratio 0) (use-ratio 1))
  (let ((count 0)
        (sampling-interval 100))
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
