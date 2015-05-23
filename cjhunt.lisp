(use-package (cons :json-rpc (ql:quickload '(:drakma :cl-json :parse-float))))

(defun-json-rpc bitcoind.rpc :explicit (method &rest params &aux stream)
  (unwind-protect
       (json-bind (result error)
           (setf stream (http-request
                         "http://host:port" :method :post :content
                         (encode-json-alist-to-string
                          `(("method" . ,method)
                            ("params" . ,(apply 'vector params))))
                         :want-stream t :basic-authorization
                         '("username" "password")))
         (if error (error "bitcoind error: ~S" error) result))
    (when stream (close stream))))

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
  (loop for txid in (cdr (assoc :tx blk))
     for cjp = (handler-bind ((warning #'muffle-warning))
                 (coinjoinp txid)) when cjp collect (cons txid cjp)))