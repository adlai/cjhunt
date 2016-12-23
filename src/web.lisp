(in-package :cl-user)
(defpackage cjhunt.web
  (:use :cl :anaphora
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

(defroute ("/block(joins)?" :regexp t) (&key |id|)
  (handler-case (render-json (blockjoins |id|))
    (error () (error 'caveman-exception :code 404))))

(defroute ("/flush?") (&key |symbol| |pass| |package|)
  (awhen (and (string= |pass| "CHANGEME")
              (get (find-symbol (string-upcase |symbol|)
                                (find-package
                                 (string-upcase (or |package| "hunt"))))
                   'fare-memoization::memoization-info))
    (let* ((old (fare-memoization::memoized-table it))
           (save (loop for key being the hash-key in old using (hash-value data)
                    when (car data) collect (cons key data))))
      (with-output-to-string (*trace-output*)
        (time (setf (slot-value it 'fare-memoization::table)
                    (aprog1 (make-hash-table
                             :rehash-size (hash-table-rehash-size old)
                             :rehash-threshold (hash-table-rehash-threshold old)
                             :test (hash-table-test old) :size (length save))
                      (loop for (key . data) in save
                         do (setf (gethash key it) data)))))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
