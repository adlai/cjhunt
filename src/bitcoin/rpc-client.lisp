(in-package :cl-user)
(defpackage cjhunt.bitcoin-rpc
  (:nicknames :btc) (:export :rpc :*node*) ; define-rpc exports more
  (:use :cl :json-rpc :cl-json :parse-float :alexandria :cjhunt.config))
(in-package :cjhunt.bitcoin-rpc)

(defun read-auth (&optional (path (config :bitcoin)) &aux user pass)
  (with-open-file (config path)
    (loop for line = (read-line config () ())
       while (and line (not (and user pass))) do
         (macrolet ((scrapes (&body configs)
                      (loop for (key var) in configs for len = (length key)
                         collect `((eql 0 (search ,key line))
                                   (setf ,var (subseq line ,len))) into clauses
                         finally (return `(cond ,@clauses)))))
           (scrapes ("rpcuser=" user) ("rpcpassword=" pass))))
    `(,user ,pass)))

(defclass bitcoind ()
  ((url :type string :initarg :url) (stream :type stream)
   (auth :type (cons string (cons string null)) :initarg :auth)))

(defun-json-rpc bitcoind.rpc :explicit (bitcoind method &rest params)
  (with-slots (url auth stream) bitcoind
    (multiple-value-bind (body status headers uri redundant closep)
        (multiple-value-call #'drakma:http-request
          url :method :post :close () :want-stream t :content
          (encode-json-alist-to-string
           `(("method" . ,method) ("params" . ,(apply 'vector params))))
          :basic-authorization auth :keep-alive t
          (if (and (slot-boundp bitcoind 'stream) (open-stream-p stream))
              (values :stream stream) (values)))
      (declare (ignorable status headers uri redundant))
      (let ((json:*real-handler* (lambda (amount)
                                   (parse-float amount :type 'rational))))
        (json-bind (result error) body
          (if closep (close body) (read-line (setf stream redundant)))
          (if error (error "bitcoind error: ~S" error) result))))))

(defparameter *node*                    ; you may want to edit these
  (make-instance 'bitcoind :url "http://localhost:8332" :auth (read-auth)))

(defun rpc (method &rest params)
  (apply #'bitcoind.rpc *node* method params))