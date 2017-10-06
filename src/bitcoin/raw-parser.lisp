(in-package :cl-user)
(defpackage cjhunt.bitcoin-parser
  (:use :cl :anaphora :alexandria :local-time) (:nicknames :btc.raw)
  (:export :txid :parse-block :parse-header :parse-txs))
(in-package :cjhunt.bitcoin-parser)

(defun flip-bytes (hex &key (reverse t) &aux (length (length hex)))
  (if (not reverse)
      (loop for i below length by 2 do
           (rotatef (aref hex i) (aref hex (1+ i)))
         finally (return hex))
      (loop for i below (/ length 2) by 2 do
           (rotatef (aref hex i) (aref hex (- length i 2)))
           (rotatef (aref hex (1+ i)) (aref hex (- length i 1)))
         finally (return hex))))

(defun parse-header (string &aux (index 0))
  (labels ((next (n) (flip-bytes (subseq string index (incf index (* 2 n)))))
           (hex (n) (parse-integer (next n) :radix 16)))
    (values (hex 4)                            ; version
            (next 32)                          ; previous
            (next 32)                          ; merkleroot
            (unix-to-timestamp (hex 4))        ; timestamp
            (/ #.(ash #xFFFF (- (* 4 #x1d) 3)) ; initial target
               (ash (hex 3) (- (hex 1) 3)))    ; current target
            (hex 4))))                         ; nonce

(defun parse-txs (string &aux (i 160))
  (labels ((b (n) (flip-bytes (subseq string i (incf i (* 2 n)))))
           (x (n) (parse-integer (b n) :radix 16))
           (int () (acase (x 1) (#xFF (x 8)) (#xFE (x 4)) (#xFD (x 2)) (t it)))
           (txt () (alet (int) (ironclad:hex-string-to-byte-array
                                string :start i :end (incf i (* 2 it)))))
           (i () `((,(b 32).,(x 4)).(,(txt).,(x 4)))) (o () `(,(x 8).,(txt)))
           (w (sw n) (and sw `(,(vecof (lambda () (vecof #'txt)) n))))
           (vecof (thunk &optional (count (int)))
             (do* ((i 0 (1+ i)) (acc (make-array count)))
                  ((= i count) acc) (setf (aref acc i) (funcall thunk))))
           (tx (&aux (ver (x 4)) (n (int)) sw)
             (when (= 0 n) (assert (= 1 (x 1))) (setf n (int) sw t))
             (multiple-value-call #'vector ver (vecof #'i n) (vecof #'o)
                                  (alet (w sw n) (apply #'values (x 4) it)))))
    (vecof #'tx)))

(defun parse-block (hex)
  (cons (multiple-value-call #'vector (parse-header hex)) (parse-txs hex)))

(defun txid (tx)
  (labels ((cat (&rest all)
             (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) all))
           (le (n &optional (b 4))
             (ironclad:integer-to-octets n :n-bits (* 8 b) :big-endian ()))
           (xdvi (s) (case s ((1 2) '(2 #xFD)) ((3 4) '(4 #xFE)) (t '(8 #xFF))))
           (sint (n &aux (size (ceiling (integer-length n) 8)))
             (if (> #xFD n) (le n 1)
                 (destructuring-bind (m w) (xdvi size) (cat `#(,w) (le n m)))))
           (txt (s) (cat (sint (length s)) s))
           (vec (vec thunk)
             (reduce #'cat vec :key thunk :initial-value (sint (length vec))))
           (o (o) (cat (le (car o) 8) (txt (cdr o))))
           (i (i) (cat (ironclad:hex-string-to-byte-array (flip-bytes (caar i)))
                       (le (cdar i)) (txt (cadr i)) (le (cddr i))))
           (sha256 (stuff) (ironclad:digest-sequence :sha256 stuff)))
    (ironclad:byte-array-to-hex-string
     (nreverse (sha256 (sha256 (cat (le (elt tx 0)) (vec (elt tx 1) #'i)
                                    (vec (elt tx 2) #'o) (le (elt tx 3)))))))))

;;;
;;; Disk format found in blkABCDE.dat files
;;;

(defun read-le-bytes (stream count)
  (aprog1 (make-array count :element-type '(unsigned-byte 8))
    (do ((i (1- count) (1- i))) ((< i 0))
      (setf (aref it i) (read-byte stream)))))

(defun read-le-int (stream width &aux (sum 0))
  (dotimes (i width sum) (incf sum (ash (read-byte stream) (* i 8)))))

(defun read-header (stream)
  (flet ((int (n) (read-le-int stream n)) (data (n) (read-le-bytes stream n)))
    (assert (equal #xD9B4BEF9 (read-le-int stream 4)))
    (let ((total (int 4)))
      (values (int 4)                            ; version
              (data 32)                          ; previous
              (data 32)                          ; merkleroot
              (unix-to-timestamp (int 4))        ; timestamp
              (data 4)                           ; nbits
              (int 4)                            ; nonce
              (- total 80)))))
