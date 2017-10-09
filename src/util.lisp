(in-package :cl-user)
(defpackage cjhunt.util
  (:use :cl :anaphora)
  (:export :remhash-if :getjso))
(in-package :cjhunt.util)

(defun remhash-if (predicate old)
  "Copy `old', skipping all entries for which (funcall `predicate' key value)"
  (let ((save (loop for key being the hash-key in old using (hash-value data)
                 unless (funcall predicate key data) collect (cons key data))))
    (declare (dynamic-extent save))
    (aprog1 (make-hash-table :rehash-size (hash-table-rehash-size old)
                             :test (hash-table-test old) :size (length save)
                             :rehash-threshold (hash-table-rehash-threshold old))
      (loop for (key . data) in save do (setf (gethash key it) data)))))

(defun getjso (key &optional map)
  (if map (cdr (assoc (intern (string key) :keyword) map :test #'string=))
      (lambda (map) (getjso key map))))
