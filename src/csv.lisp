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

(defun equal-name (l r)
  (labels ((get-name (x)
             (if (symbolp x) (symbol-name x) x)))
    (equal (get-name l) (get-name r))))

(defun re-intern-symbols (body-lst name-lst)
  (let ((replace-lst (mapcar (lambda (name) (cons name name))
                             name-lst)))
    (sublis replace-lst body-lst
            :test #'equal-name)))

(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    `(list ,@(mapcar
              (lambda (process)
                (let ((arg-lst (if (listp (car process))
                                   (mapcar (lambda (str)
                                             (intern (string-upcase str)))
                                           (car process))
                                   '(it)))
                      (find-lst (if (listp (car process))
                                    (car process)
                                    (list (car process)))))
                  `((lambda (,@arg-lst)
                      ,@(re-intern-symbols (cdr process) arg-lst))
                    ,@(mapcar
                       (lambda (arg) 
                         `(find-target-value ,arg ,head-line ,data-line))
                       find-lst))))
              body))))
