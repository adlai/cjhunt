(in-package :cl-user)
(defpackage cjhunt.hunt
  (:use :cl :anaphora :alexandria :local-time
        :fare-memoization :cjhunt.bitcoin-rpc)
  (:export :coinjoinp :blockjoins) (:nicknames :hunt))
(in-package :cjhunt.hunt)

(define-memo-function coinjoinp-cdr (id &optional (tx (getrawtransaction id)))
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
                                  (let ((type (- n change)))
                                    (when (typep type '(member 0 1))
                                      `((:id . ,id) (:participants . ,n)
                                        (:size . ,(* (expt 10 8) size))
                                        (:type . ,(elt '("send" "sweep") type))))))))
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

(define-memo-function block-fees (id &aux (blk (getblock id)))
  (let ((tx (getrawtransaction (cadr (assoc :tx blk))))) ; cheat: fees = coinbase - 25
    (assert (eq (caaadr (assoc :vin tx)) :coinbase))     ;        when you ass-u-me...
    (- (loop for out in (cdr (assoc :vout tx)) sum (cdr (assoc :value out))) 25)))

(define-memo-function coinjoins-in-block (id &aux (blk (getblock id)))
  (sort (handler-bind ((warning #'muffle-warning)) ; muffle rejection reasons
          (loop for txid in (cddr (assoc :tx blk)) ; cddr skips coinbase txs
             for cjp = (coinjoinp-cdr txid) when cjp collect cjp))
        #'> :key (lambda (data) (cdr (assoc :size data)))))

(defgeneric blockjoins (id)             ; don't memoize getblock, it's volatile!
  (:method ((id string))
    (handler-case (blockjoins (parse-integer id)) ; first, treat it as a height
      (error () (aprog1 (getblock id)   ; next, try treating it as a block hash
                  (let ((tx (member :tx it :key #'car))) ; finally!list surgery
                    (psetf (caar tx) :fee (cdar tx) (block-fees id))
                    (push `(:cj .,(coerce (coinjoins-in-block id) 'vector))
                          (cdr tx)))))))
  (:method ((id null)) (blockjoins (getbestblockhash)))    ; /blockjoins?id
  (:method ((id integer)) (blockjoins (getblockhash id)))) ; ?id=height

(define-memo-function next-sizes (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (out) (* (expt 10 8) (cdr (assoc :value out))))
	  (cdr (assoc :vout tx))))

(define-memo-function prev-sizes (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (in &aux (id (cdr (assoc :txid in))))
	    (nth (cdr (assoc :vout in)) (next-sizes id)))
	  (cdr (assoc :vin tx))))

(defun subset-sums-below (set target &optional acc)
  (when set
    (destructuring-bind (head . tail) set
      (let ((gap (- target head)))
	(sort (if (not (minusp gap))
		  (append
		   (acons gap (cons head acc) ()) ; this one
		   (and (plusp gap)		  ; now those
			(subset-sums-below
			 tail gap (cons head acc)))	  ; with
		   (subset-sums-below tail target acc))	  ; without
		  (subset-sums-below tail target acc))	  ; no doubt!
	      #'< :key #'car)))))

;;; FIXME this is still mostly broken
(define-memo-function credible-subsets (id)
  (aif (coinjoinp-cdr id)
       (labels ((rec (coins targets &optional acc)
		  (cond
		    ((null targets) (and (< (reduce #'+ coins))) acc)
		    ((or  (null targets) (null coins))  ())
		    (t (let ((target (pop targets)))
			 (awhen (subset-sums-below coins target)
			   (dolist (subset it)
			     (awhen (rec (set-difference ; imprecise
					  coins (cdr subset))
					 targets (cons subset acc))
			       (return it)))))))))
	 ;; todo: try skipping each target once (ie, as taker)
	 (rec (prev-sizes id)
	      (let ((size (cdr (assoc :size it))))
		(mapcar (lambda (change) (+ change size))
			(remove size (next-sizes id))))))
       (error "Doesn't even look like a coinjoin")))
