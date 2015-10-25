(in-package :cl-user)
(defpackage kaggle-titanic-test.utils
  (:use :cl
        :cl-ppcre))
(in-package :kaggle-titanic-test.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-dispatch-macro-character #\$)
  (set-dispatch-macro-character
   #\$ #\:
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (intern (symbol-name (read stream nil))
               (regex-replace "-TEST" (package-name *package*) "")))))

