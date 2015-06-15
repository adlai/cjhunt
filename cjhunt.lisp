(use-package (cons :json-rpc (ql:quickload '(:cl-json :drakma :parse-float :alexandria :local-time))))

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

(defun coinjoinp (txid &aux (tx (bitcoind.rpc "getrawtransaction" txid 1)))
  (let ((ins (cdr (assoc :vin tx))) (outs (cdr (assoc :vout tx))))
    (cond
      ((find :coinbase ins :key #'caar) (warn "~A coinbase" txid))
      ((find-if (lambda (asm) (string= "0 " asm :end2 2))
                ins :key (lambda (in)
                           (cdr (assoc :asm (cdr (assoc :script-sig in))))))
       (warn "~A has multisig inputs" txid))
      (t (let* ((outsizes (mapcar #'cdar outs))
                (cjouts (loop for out in outs for amt = (cdar out)
                           when (> (count amt outsizes) 1) collect out))
                (pks (delete-duplicates (mapcar (compose #'cdar #'cdaddr)
                                                ins) :test #'string=)))
           (if (null cjouts) (warn "~A no duplicate output sizes" txid)
               (let ((n-pks (length pks)) (n-cjouts (length cjouts)))
                 (if (< n-pks n-cjouts)
                     (warn "~A inputs ~D < ~D cjouts" txid n-pks n-cjouts)
                     (let ((cjout-sizes (mapcar #'cdar cjouts)))
                       (if (apply #'= cjout-sizes)
                           (cons n-cjouts (car cjout-sizes))
                           (warn "~A nontrivial cj" txid)))))))))))

(defun blockjoins (blkid &aux (blk (bitcoind.rpc "getblock" blkid)))
  (values (loop for txid in (cdr (assoc :tx blk))
             for cjp = (handler-bind ((warning #'muffle-warning))
                         (coinjoinp txid)) when cjp collect (cons txid cjp))
          (cdr (assoc :previousblockhash blk))
          (cdr (assoc :nextblockhash blk))))

(let (all-joins (blkid (cdr (assoc :bestblockhash
                                   (bitcoind.rpc "getblockchaininfo")))))
  (time (loop
           (multiple-value-bind (joins previd) (time (blockjoins blkid))
             (print (length (and joins (push (cons blkid joins) all-joins))))
             (if (or (null previd) (= (length all-joins) 5000))
                 (return all-joins) (setf blkid previd))))))