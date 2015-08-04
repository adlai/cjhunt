(defpackage :cjhunt
  (:use :cl :json-rpc .
        #.(ql:quickload '(:cl-json :drakma :parse-float :alexandria :local-time))))
(in-package :cjhunt)

(defclass bitcoind ()
  ((url :type string :initarg :url) (stream :type stream)
   (auth :type (cons string (cons string null)) :initarg :auth)))

(defun-json-rpc bitcoind.rpc :explicit (bitcoind method &rest params)
  (with-slots (url auth stream) bitcoind
    (multiple-value-bind (body status headers uri redundant closep)
        (multiple-value-call #'http-request
          url :method :post :close () :want-stream t :content
          (encode-json-alist-to-string
           `(("method" . ,method) ("params" . ,(apply 'vector params))))
          :basic-authorization auth :keep-alive t
          (if (and (slot-boundp bitcoind 'stream) (open-stream-p stream))
              (values :stream stream) (values)))
      (declare (ignorable status headers uri redundant))
      (json-bind (result error) body
        (if closep (close body) (read-line (setf stream redundant)))
        (if error (error "bitcoind error: ~S" error) result)))))

(defvar *node*                          ; edit these!
  (make-instance 'bitcoind :url "http://localhost:8332" ; or wherever
                 :auth '("rpcuser" "rpcpassword"))) ; from bitcoin.conf

(defun coinjoinp-cdr (id &optional
                           (tx (bitcoind.rpc *node* "getrawtransaction" id 1)))
  (let ((ins (cdr (assoc :vin tx))) (outs (cdr (assoc :vout tx))))
    (cond
      ;; ignore transactiosn which have multisig inputs, since the current
      ;; joinmarket doesn't use multisig (but could have a multisig output)
      ((find-if (lambda (asm) (string= "0 " asm :end2 2))
                ins :key (lambda (in)
                           (cdr (assoc :asm (cdr (assoc :script-sig in))))))
       (warn "~A has multisig inputs" id))
      (t (let* ((outsizes (mapcar #'cdar outs))
                ;; look for repeated outputs with identical size
                (cjouts (loop for out in outs for amt = (cdar out)
                           when (> (count amt outsizes) 1) collect out))
                ;; ignore duplicate input public keys
                (pks (delete-duplicates (mapcar (compose #'cdar #'cdaddr)
                                                ins) :test #'string=)))
           (if (null cjouts) (warn "~A no duplicate output sizes" id)
               (let ((n-pks (length pks)) (n-cjouts (length cjouts)))
                 ;; the number of distinct public keys is an upper bound on the
                 ;; number of distinct parties involved in the transaction
                 (if (< n-pks n-cjouts)
                     (warn "~A inputs ~D < ~D cjouts" id n-pks n-cjouts)
                     (let ((cjout-sizes (mapcar #'cdar cjouts)))
                       (if (apply #'= cjout-sizes)
                           (cons n-cjouts (car cjout-sizes))
                           ;; a nontrivial coinjoin has multiple candidates for
                           ;; the actual output size. ignore them, for now.
                           (warn "~A nontrivial cj" id)))))))))))

;;; we are primarily looking for joinmarket coinjoins
(defun coinjoinp (txid &aux)
  (let ((tx (bitcoind.rpc *node* "getrawtransaction" txid 1)))
    ;; ignore the coinbase transaction
    (if (find :coinbase (cdr (assoc :vin tx)) :key #'caar) (warn "~A coinbase" txid)
        (coinjoinp-cdr txid tx))))

(defun blockjoins (blkid &aux (blk (bitcoind.rpc *node* "getblock" blkid)))
  (values (loop for txid in (cddr (assoc :tx blk)) ; cddr skips coinbase txs
             for cjp = (handler-bind ((warning #'muffle-warning))
                         (coinjoinp-cdr txid)) when cjp collect (cons txid cjp))
          (cdr (assoc :previousblockhash blk))
          (cdr (assoc :nextblockhash blk))))

;;; hardcoded script: searches from the best block backwards, accumulating
;;; the coinjoins in each block, grouped by blockid. collects 10 such blocks.
(let (all-joins (blkid (cdr (assoc :bestblockhash
                                   (bitcoind.rpc *node* "getblockchaininfo")))))
  (time (loop
           (multiple-value-bind (joins previd) (time (blockjoins blkid))
             (print (length (and joins (push (cons blkid joins) all-joins))))
             (if (= (length all-joins) 10)
                 (return (pprint all-joins)) (setf blkid previd))))))
