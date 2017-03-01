(in-package :cl-user)
(defpackage cjhunt.view
  (:use :cl)
  (:import-from :cjhunt.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :cjhunt.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun pprint-json (*standard-output* json)
  (typecase json
    (rational
     (write-string (decimals:format-decimal-number json :round-magnitude -8)))
    (list (pprint-logical-block (*standard-output* json :prefix "{" :suffix "}")
            (loop for (key . value) = (pop json) while key do
                 (pprint-logical-block (*standard-output* ())
                   (format t "\"~(~A~)\": ~@_" key)
                   (pprint-json *standard-output* value))
                 (when json (format t ", ") (pprint-newline :fill)))))
    (string (format t "~S" json))
    (vector (pprint-logical-block (nil nil :prefix "[" :suffix "]")
              (let ((end (length json)) (i 0))
                (when (plusp end)
                  (loop
                     (pprint-json *standard-output* (aref json i))
                     (if (= (incf i) end) (return nil))
                     (write-char #\Space)
                     (pprint-newline :fill))))))
    (t (format t "~(~A~)" json))))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (let ((*print-right-margin* 72) (*print-pretty* t))
    (with-output-to-string (out) (pprint-json out object))))


;;
;; Execute package definition

(defpackage cjhunt.djula
  (:use :cl)
  (:import-from :cjhunt.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :cjhunt.djula))
