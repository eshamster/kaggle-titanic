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

(defun reconstruct-list (lst fn-elem)
  (labels ((rec (result rest)
             (if rest
                 (dolist (elem rest)
                   (setf result
                         (cons (if (listp elem)
                                   (rec nil elem)
                                   (funcall fn-elem elem))
                               result))))
             (reverse result)))
    (rec nil lst)))

(defun rec-intern-by-args (body-lst arg-lst)
  (reconstruct-list body-lst
                    (lambda (elem)
                      (if (symbolp elem)
                          (if (find (symbol-name elem) (mapcar #'symbol-name arg-lst)
                                    :test #'equal)
                              (intern (symbol-name elem))
                              elem)
                          elem))))

(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    `(list ,@(mapcar
              (lambda (process)
                (let ((arg-lst (if (listp (car process))
                                   (mapcar (lambda (str)
                                             (intern (string-upcase str)))
                                           (car process))
                                   '(:it)))
                      (find-lst (if (listp (car process))
                                    (car process)
                                    (list (car process)))))
                  `((lambda (,@arg-lst)
                      ,@(rec-intern-by-args (cdr process) arg-lst))
                    ,@(mapcar
                       (lambda (arg) 
                         `(find-target-value ,arg ,head-line ,data-line))
                       find-lst))))
              body))))
