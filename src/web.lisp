(in-package :cl-user)
(defpackage cjhunt.web
  (:use :cl
        :caveman2 :caveman2.exception
        :cjhunt.config
        :cjhunt.view
        :cjhunt.db
        :cjhunt.bitcoin-rpc
        :cjhunt.hunt
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cjhunt.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute "/blockjoins" (&key |id|)
  (flet ((random-sleep (&optional (exp 2)) (sleep (random (exp exp)))))
    (handler-case (progn (random-sleep) (render-json (blockjoins |id|)))
      (error () (random-sleep 1) (error 'caveman-exception :code 404)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
