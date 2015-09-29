(in-package :cl-user)
(defpackage cjhunt.hunt
  (:use :cl :alexandria :local-time :cjhunt.bitcoin-rpc)
  (:export :coinjoinp :blockjoins) (:nicknames :cjh))
(in-package :cjhunt.hunt)

(defun coinjoinp-cdr (id &optional (tx (getrawtransaction id)))
  (let ((ins (cdr (assoc :vin tx))) (outs (cdr (assoc :vout tx))))
    (cond
      ;; ignore transactions which have multisig inputs, since the current
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
                       (flet ((report (n size &aux (change (- (length outs) n)))
                                (when (> n 2) ; skip trivial false positives
                                  ;; these are the two types of coinjoin
                                  ;; currently created by joinmarket:
                                  (case (- n change)
                                    (1 `#(((:participants . ,n) (:type . :sweep)
                                           (:size . ,(* (expt 10 8) size)))))
                                    (0 `#(((:participants . ,n) (:type . :send)
                                           (:size . ,(* (expt 10 8) size)))))))))
                         (if (apply #'= cjout-sizes)
                             (report n-cjouts (car cjout-sizes))
                             ;; a nontrivial coinjoin has multiple candidates for
                             ;; the actual output size. use the largest ones.
                             (loop for size in cjout-sizes
                                with most = 0 and best and counts =
                                  (make-hash-table :test 'eql :size n-cjouts)
                                for c = (incf (gethash size counts 0))
                                if (> c most) do (setf most c best size) finally
                                  (return (report most best))))))))))))))

;;; we are primarily looking for joinmarket coinjoins
(defun coinjoinp (txid &aux (tx (getrawtransaction txid)))
  ;; ignore the coinbase transaction
  (if (find :coinbase (cdr (assoc :vin tx)) :key #'caar) (warn "~A coinbase" txid)
      (coinjoinp-cdr txid tx)))

(defun tx-fee (txid &aux (tx (getrawtransaction txid)))
  (let ((ins (mapcar (lambda (in)
                       (let* ((ptx (getrawtransaction
                                    (cdr (assoc :txid in))))
                              (out (nth (cdr (assoc :vout in))
                                        (cdr (assoc :vout ptx)))))
                         (cdr (assoc :value out))))
                     (cdr (assoc :vin tx))))
        (outs (mapcar (lambda (out) (cdr (assoc :value out)))
                      (cdr (assoc :vout tx)))))
    (let ((fee (reduce #'- outs :initial-value (reduce #'+ ins))))
      ;; primary value - bitcoin, secondary - satoshi per byte
      (values fee (/ fee (length (cdr (assoc :hex tx))) 1/2 (expt 10 -8))))))

(defun blockjoins (&optional id &aux (blk (if id (getblock id) (getblock))))
  (let ((cjs (loop for txid in (cddr (assoc :tx blk)) ; cddr skips coinbase txs
                for cjp = (handler-bind ((warning #'muffle-warning))
                            (coinjoinp-cdr txid))
                when cjp collect (cons txid cjp))))
    (setf (cdr (assoc :tx blk)) cjs)    ; dump coinbases
    blk))
