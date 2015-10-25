#|
  This file is a part of kaggle-titanic project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage kaggle-titanic-test-asd
  (:use :cl :asdf))
(in-package :kaggle-titanic-test-asd)

(defsystem kaggle-titanic-test
  :author "eshamster"
  :license ""
  :depends-on (:kaggle-titanic
               :cl-ppcre
               :prove)
  :components ((:module "t"
                :components
                ((:file "utils")
                 (:test-file "csv")
                 (:test-file "kaggle-titanic"))))
  :description "Test system for kaggle-titanic"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
