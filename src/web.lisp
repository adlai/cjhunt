(in-package :cl-user)
(defpackage cjhunt.web
  (:use :cl
        :caveman2
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

(defroute "/block" (&key |id|)
  (render-json (remove :tx (if |id| (getblock |id|) (getblock)) :key #'car)))

(defroute "/blockjoins" (&key (|id| (rpc "getbestblockhash")))
  (render-json (blockjoins |id|)))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
