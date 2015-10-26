(in-package :cl-user)
(defpackage kaggle-titanic.csv
  (:use :cl)
  (:export :convert-raw-data-one-line)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :alexandria
                :once-only
                :with-gensyms
                :compose))
(in-package :kaggle-titanic.csv)

(defun find-target-value (target head-line data-line)
  (aif (position target head-line :test #'equal)
       (nth it data-line)))

(defun equal-name (l r)
  (and (symbolp l)
       (symbolp r)
       (equal (symbol-name l) (symbol-name r))))

(defun re-intern-symbols (body-lst name-lst)
  (let ((replace-lst (mapcar (lambda (name) (cons name name))
                             name-lst)))
    (sublis replace-lst body-lst
            :test #'equal-name)))

(defun add-to-new-header (names new-header-lst)
  (labels ((make-base-name (result name-lst prefix)
             (if (null name-lst)
                 result
                 (make-base-name (format nil "~A~A~A" result prefix (car name-lst))
                                 (cdr name-lst)
                                 "-")))
           (add-without-duplicate (base-name count lst)
             (let ((name (format nil "~A~A"
                                 base-name
                                 (if (> count 1) count ""))))
               (if (find name lst :test #'equal)
                   (add-without-duplicate base-name (1+ count) lst)
                   (setf lst (cons name lst))))))
    (add-without-duplicate (if (listp names) (make-base-name "" names "") names)
                           1
                           new-header-lst)))

(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    (with-gensyms (new-header)
      `(let ((,new-header))
         (values (list ,@(mapcar
                          (lambda (process)
                            (let* ((names (car process))
                                   (arg-lst (if (listp names)
                                                (mapcar (lambda (str)
                                                          (intern (string-upcase str)))
                                                        names)
                                                '(it)))
                                   (find-lst (if (listp names)
                                                 (car process)
                                                 (list names))))
                              `((lambda (,@arg-lst)
                                  (setf ,new-header
                                        (add-to-new-header ',names ,new-header))
                                  ,@(if (cdr process)
                                        (re-intern-symbols (cdr process) arg-lst)
                                        arg-lst))
                                ,@(mapcar
                                   (lambda (arg) 
                                     `(find-target-value ,arg ,head-line ,data-line))
                                   find-lst))))
                          body))
                 (reverse ,new-header))))))
