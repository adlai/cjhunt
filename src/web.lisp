(in-package :cl-user)
(defpackage cjhunt.web
  (:use :cl :anaphora :alexandria
        :caveman2 :caveman2.exception
        :cjhunt.util
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

(defroute ("/block") (&key |id|)
  (handler-case (render-json (blockjoins |id|)) (error () (fail))))

(defroute ("/flush") (&key |symbol| |pass| |package|)
  (aif (and (eq |pass| "CHANGEME")
            (get (find-symbol (string-upcase |symbol|)
                              (find-package
                               (string-upcase (or |package| "hunt"))))
                 'fare-memoization::memoization-info))
       (with-output-to-string (*trace-output*)
         (time (setf (slot-value it 'fare-memoization::table)
                     (remhash-if (lambda (txid data)
                                   (declare (ignore txid))
                                   (= (length (car data)) 0))
                                 (fare-memoization::memoized-table it)))))
       "flushing must be explicitly enabled: s/eq/string=/ and recompile"))

(defroute ("/random") ()
  (redirect (let ((all (hash-table-alist
                        (fare-memoization::memoized-table
                         (get 'hunt::coinjoins-in-block
                              'fare-memoization::memoization-info)))))
              (aif (remove 0 all :key (compose #'length #'cadr))
                   (format () "/block?id=~A"
                           (caar (elt it (mod (length all) (length it)))))
                   (fail 204)))))

;;
;; Shouting drools

(defun fail (&optional (code 404)) (error 'caveman-exception :code code))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
