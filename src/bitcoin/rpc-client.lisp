(in-package :cl-user)
(defpackage cjhunt.bitcoin-rpc
  (:nicknames :btc) (:export :rpc :*node*) ; define-rpc exports more
  (:use :cl :split-sequence :json-rpc :cl-json :anaphora
        :parse-float :alexandria :cjhunt.config))
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
  ((url :type string :initarg :url)
   (auth :type (cons string (cons string null)) :initarg :auth)))

(define-condition bitcoin-rpc-error (error)
  ((code :initarg :code :reader bitcoin-rpc-error-code)
   (message :initarg :message :reader bitcoin-rpc-error-message))
  (:report (lambda (condition stream)
             (format stream "Bitcoin RPC Error ~D: ~A"
                     (bitcoin-rpc-error-code condition)
                     (bitcoin-rpc-error-message condition)))))

(defun-json-rpc bitcoind.rpc :explicit (bitcoind method &rest params)
  (with-slots (url auth stream) bitcoind
    (let ((*real-handler* (lambda (in) (parse-float in :type 'rational))))
      (with-open-stream
          (body (drakma:http-request url :method :post :want-stream t :content
                                     (encode-json-alist-to-string
                                      `(("method" . ,method)
                                        ("params" . ,(apply 'vector params))))
                                     :basic-authorization auth))
        (json-bind (result error) body
          (if error (error 'bitcoin-rpc-error :code (cdr (assoc :code error))
                           :message (cdr (assoc :message error)))
              result))))))

(defparameter *node*                    ; you may want to edit these
  (make-instance 'bitcoind :url "http://localhost:8332" :auth (read-auth)))

(defun rpc (method &rest params)
  (apply #'bitcoind.rpc *node* method params))

(defun joinmarket-account-overview (account)
  (let* ((txs (sort (rpc "listtransactions" account 1000 0 t) #'>
                    :key (lambda (tx) (cdr (assoc :confirmations tx)))))
         (addresses (loop with all = (rpc "getaddressesbyaccount" account)
                       with hash = (make-hash-table
                                    :test #'equal :size (length all))
                       for address in all do (setf (gethash address hash) ())
                       finally (return hash)))
         (utxos (make-hash-table
                 :test #'equal :size (hash-table-size addresses))))
    (flet ((utxo (data)                 ; colon lollon
             (acons (cdr (assoc :amount data))
                    (cdr (assoc :txid data))
                    (cdr (assoc :vout data)))))
      ;; we see all coinjoins twice, since `listtransactions' actually returns an
      ;; object for each output to a watched address - usually, cjout and change.
      (dolist (tx txs)
        (let ((utxo (utxo tx)))
          (push utxo (gethash (cdr (assoc :address tx)) addresses))
          (setf (gethash utxo utxos) (cdr (assoc :address tx)))))
      (dolist (tx (delete-duplicates txs :test #'string= :key
                                     (lambda (tx) (cdr (assoc :txid tx))))
               addresses)
        (dolist (in (cdr (assoc :vin (getrawtransaction (cdr (assoc :txid tx))))))
          (let* ((txo (utxo in)) (from (gethash txo utxos)))
            (when from
              (remhash txo utxos)
              (let ((stxo (find txo (gethash from addresses) :test #'equal)))
                (rplacd stxo (cons (car stxo) (cdr stxo)))
                (rplaca stxo :spent)))))))))
