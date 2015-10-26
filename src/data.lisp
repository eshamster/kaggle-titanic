(in-package :cl-user)
(defpackage kaggle-titanic.data
  (:use :cl)
  (:export :do-converted-line-data
           :add-name
           :extract-cabin
           :round-num
           :extract-miss-or-mrs
           :make-my-path)
  (:import-from :cl-csv
                :read-csv)
  (:import-from :alexandria
                :with-gensyms))
(in-package :kaggle-titanic.data)

; TODO: move to a proper package
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
  (labels ((join-values (rest result prefix)
             (when (null rest)
               (return-from join-values result))
             (join-values (cdr rest)
                          (format nil "~A~A~A" result prefix (car rest))
                          "-")))
    (if (every #'(lambda (value)
                   (or (null value) (equal value "")))
               values)
        nil
        (format nil "~A:~A" name (join-values values "" "")))))

(defun round-num (target-str interval &key (scale 1))
  (if (= (length target-str) 0)
      (return-from round-num nil))
  (* interval (round
               (/ (* (read-from-string target-str) scale)
                  interval))))

(defmacro do-converted-line-data ((value data-path &key
                                         (offset-ratio 0)
                                         (use-ratio 1)
                                         (process-one-line nil))
                                  &body body)
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
                   ,(if process-one-line
                        `(funcall ,process-one-line ,head-line ,line)
                        line)))
             ,@body))))))
