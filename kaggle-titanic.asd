#|
  This file is a part of kaggle-titanic project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage kaggle-titanic-asd
  (:use :cl :asdf))
(in-package :kaggle-titanic-asd)

(defsystem kaggle-titanic
  :version "0.1"
  :author "eshamster"
  :license ""
  :depends-on (:cl-csv
               :cl-ppcre
               :alexandria
               :anaphora
               :cl-naive-bayes)
  :components ((:module "src"
                :components
                ((:file "csv")
                 (:file "data")
                 (:file "kaggle-titanic"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op kaggle-titanic-test))))
