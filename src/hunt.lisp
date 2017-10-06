(in-package :cl-user)
(defpackage cjhunt.hunt
  (:use :cl :anaphora :alexandria :local-time
        :fare-memoization :cjhunt.bitcoin-rpc :cjhunt.bitcoin-parser)
  (:export :coinjoinp :blockjoins) (:nicknames :hunt))
(in-package :cjhunt.hunt)

(defun coinjoinp (tx)
  (let ((ins (elt tx 1)) (outs (elt tx 2)))
    (unless (find 0 ins :key (lambda (in) (elt (caar in) 0)))
      (let ((cjouts (remove 1 outs :key
                            (lambda (out) (count (car out) outs :key #'car))))
            (n-pks (length (remove-duplicates ins :test #'equal :key #'caar))))
        (flet ((swp (a b c d list key)
                 (count-if (lambda (x)
                             (and (= (length x) 23) (= (elt x 0) a)
                                  (= (elt x 1) b)   (= (elt x c) d)))
                           list :key key))
               (report (n size swps)
                 (let ((extra (- (* 2 n) (length outs)))
                       (swp (or (= 5 (length tx))
                                (and (<= n (car swps) (cdr swps))))))
                   (when (and (> n 2) (typep extra '(member 0 1)))
                     `((:id . ,(txid tx)) (:parts . ,n) (:size . ,size)
                       (:type .,(format () "~[send~;sweep~];~:[pkh~;sw~]"
                                        extra swp)))))))
          (let ((n-cjouts (length cjouts)) (sizes (map 'list #'car cjouts))
                (p2wpkhsh (cons (swp 22 0 2 20 ins #'cadr)
                                (swp 169 20 22 135 outs #'cdr))))
            (unless (or (= 0 n-cjouts) (< n-pks n-cjouts))
              (if (apply #'= sizes) (report n-cjouts (car sizes) p2wpkhsh)
                  (loop for size in sizes with most = 0 and best
                     and counts = (make-hash-table :test 'eql :size n-cjouts)
                     for c = (incf (gethash size counts 0))
                     if (> c most) do (setf most c best size) finally
                       (return (report most best p2wpkhsh)))))))))))

(defun tx-fee (txid &aux (tx (getrawtransaction txid)))
  (let ((ins (mapcar (lambda (in)
                       (let* ((ptx (getrawtransaction
                                    (cdr (assoc :|txid| in))))
                              (out (nth (cdr (assoc :|vout| in))
                                        (cdr (assoc :|vout| ptx)))))
                         (cdr (assoc :|value| out))))
                     (cdr (assoc :|vin| tx))))
        (outs (mapcar (lambda (out) (cdr (assoc :|value| out)))
                      (cdr (assoc :|vout| tx)))))
    (let ((fee (reduce #'- outs :initial-value (reduce #'+ ins))))
      ;; primary value - bitcoin, secondary - satoshi per byte
      (values fee (/ fee (length (cdr (assoc :|hex| tx))) 1/2 (expt 10 -8))))))

(define-memo-function block-fees (id &aux (blk (getblock id)))
  (let ((cb (getrawtransaction (cadr (assoc :|tx| blk)))))
    (assert (eq (caaadr (assoc :|vin| cb)) :|coinbase|))
    (- (loop for out in (cdr (assoc :|vout| cb)) sum (cdr (assoc :|value| out)))
       (/ (ash (* 50 (expt 10 8)) (- (floor (cdr (assoc :|height| blk)) 210000)))
	  (expt 10 8)))))

(define-memo-function coinjoins-in-block (id &aux (blk (getblock id +false+)))
  (sort (loop for tx across (parse-txs blk) when (coinjoinp tx)
           collect it into cjs finally (return (coerce cjs 'vector)))
        #'> :key (lambda (data) (cdr (assoc :size data)))))

(defgeneric blockjoins (id)             ; don't memoize getblock, it's volatile!
  (:method ((id string))
    (handler-case (blockjoins (parse-integer id)) ; first, treat it as a height
      (error () (aprog1 (getblock id)   ; next, try treating it as a block hash
                  (let ((tx (member :|tx| it :key #'car))) ; finally!list surgery
                    (psetf (caar tx) :cj (cdar tx) (coinjoins-in-block id))
                    ;; (psetf (caar tx) :fee (cdar tx) (block-fees id))
                    ;; (push `(:cj .,(coerce (coinjoins-in-block id) 'vector))
                    ;;       (cdr tx))
                    )))))
  (:method ((id null))                  ; /block[joins]?id
    (blockjoins (cdr (assoc :|bestblockhash| (getblockchaininfo)))))
  (:method ((id integer)) (blockjoins (getblockhash id)))) ; ?id=height

(define-memo-function next-sizes (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (out) (* (expt 10 8) (cdr (assoc :|value| out))))
	  (cdr (assoc :|vout| tx))))

(define-memo-function prev-sizes (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (in &aux (id (cdr (assoc :|txid| in))))
	    (nth (cdr (assoc :|vout| in)) (next-sizes id)))
	  (cdr (assoc :|vin| tx))))

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
