(in-package :cl-user)
(defpackage kaggle-titanic.csv
  (:use :cl)
  (:export :convert-raw-data-one-line)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :alexandria
                :once-only
                :compose))
(in-package :kaggle-titanic.csv)

(defun find-target-value (target head-line data-line)
  (aif (position target head-line :test #'equal)
       (nth it data-line)))

(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    `(list ,@(mapcar
              (lambda (process)
                (let ((arg-lst (if (listp (car process))
                                   (mapcar (compose #'intern #'string-upcase) (car process))
                                   '(it)))
                      (find-lst (if (listp (car process))
                                    (car process)
                                    (list (car process)))))
                  `((lambda (,@arg-lst)
                      ,@(cdr process))
                    ,@(mapcar
                       (lambda (arg) 
                         `(find-target-value ,arg ,head-line ,data-line))
                       find-lst))))
              body))))
