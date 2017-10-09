(in-package :cl-user)
(defpackage cjhunt.hunt
  (:use :cl :anaphora :alexandria :local-time :cjhunt.util
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
               (report (n size swps &aux (swi (car swps)) (swo (cdr swps)))
                 (let ((swp (or (= 5 (length tx)) (<= n (min swi swo))))
                       (extra (- (* 2 n) (length outs))))
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
  (let ((ins (mapcar (lambda (in &aux (ptx (getrawtransaction
                                            (getjso "txid" in))))
                       (getjso "value" (nth (getjso "vout" in)
                                            (getjso "vout" ptx))))
                     (getjso "vin" tx)))
        (outs (mapcar (lambda (out) (getjso "value" out))
                      (getjso "vout" tx))))
    (let ((fee (reduce #'- outs :initial-value (reduce #'+ ins))))
      ;; primary value - bitcoin, secondary - satoshi per byte
      (values fee (/ fee (length (getjso "hex" tx)) 1/2 (expt 10 -8))))))

(define-memo-function block-fees (id &aux (blk (getblock id)))
  (let ((cb (getrawtransaction (cadr (assoc :|tx| blk)))))
    (assert (eq (caaadr (assoc :|vin| cb)) :|coinbase|))
    (- (loop for out in (getjso "vout" cb) sum (getjso "value" out))
       (/ (ash (* 50 (expt 10 8)) (- (floor (getjso "height" blk) 210000)))
	  (expt 10 8)))))

(define-memo-function coinjoins-in-block (id &aux (blk (getblock id +false+)))
  (sort (loop for tx across (parse-txs blk) when (coinjoinp tx)
           collect it into cjs finally (return (coerce cjs 'vector)))
        #'> :key (lambda (data) (getjso :size data))))

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
    (blockjoins (getjso "bestblockhash" (getblockchaininfo))))
  (:method ((id integer)) (blockjoins (getblockhash id)))) ; ?id=height

(define-memo-function next-sizes (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (out) (* (expt 10 8) (getjso "value" out)))
	  (getjso "vout" tx)))

(define-memo-function prev-coins (id &aux (tx (getrawtransaction id)))
  (mapcar (lambda (in &aux (id (getjso "txid" in)))
            (alet (getjso "vout" in) (acons id it (nth it (next-sizes id)))))
	  (getjso "vin" tx)))

(defun subset-sums-below (set target &optional (key #'identity))
  (labels ((rec (tail target acc)
             (when tail
               (let* ((head (pop tail)) (gap (- target (funcall key head))))
                 (sort (if (not (minusp gap))
                           (append (acons gap (cons head acc) ())
                                   (and (plusp gap)
                                        (rec tail gap (cons head acc)))
                                   (rec tail target acc))
                           (rec tail target acc))
                       #'< :key #'car)))))
    (rec set target ())))

;;; FIXME: assumes all makers profited, and... only works for sweeps
(define-memo-function credible-groupings (tx &aux (id (txid tx)))
  (aif (coinjoinp tx)
       (labels ((rec (coins targets &optional acc)
		  (cond
		    ((null targets) acc) ((null coins) ())
		    (t (let ((target (pop targets)))
			 (awhen (subset-sums-below coins target #'cdr)
			   (dolist (subset it)
			     (awhen (rec (set-difference coins (cdr subset))
					 targets (cons subset acc))
			       (return it)))))))))
         (let* ((prev (prev-coins id))
                (targets (let ((size (getjso :size it)))
                           (mapcar (lambda (change) (+ change size))
                                   (remove size (next-sizes id)))))
                ;; todo: try skipping each target once, rather than this IDIOCY
                (guess-a (rec prev targets))
                (guess-b (rec prev (reverse targets)))
                (guess-c (rec (reverse prev) targets))
                (guess-d (rec (reverse prev) (reverse targets))))
           (car (sort (list guess-a guess-b guess-c guess-d) #'<
                      :key (lambda (sets) (reduce #'+ (mapcar #'car sets)))))))
       (error "Doesn't even look like a [joinmarket] coinjoin")))
