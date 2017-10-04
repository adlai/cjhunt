(in-package :cl-user)
(defpackage cjhunt.bitcoin-parser
  (:use :cl :anaphora :alexandria :local-time) (:nicknames :btc.raw))
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

(defun parse-satoshint (string index)
  (labels ((next (n) (flip-bytes (subseq string index (incf index (* 2 n)))))
           (hex (n) (parse-integer (next n) :radix 16)))
    (values (acase (hex 1) (#xFF (hex 8)) (#xFE (hex 4)) (#xFD (hex 2)) (t it))
            index)))

(defun parse-tx (string index)
  (labels ((next (n) (flip-bytes (subseq string index (incf index (* 2 n)))))
           (hex (n) (parse-integer (next n) :radix 16))
           (cpct () (acase (hex 1)
                      (#xFF (hex 8)) (#xFE (hex 4)) (#xFD (hex 2)) (t it)))
           (script () (alet (cpct)
                        (ironclad:hex-string-to-byte-array
                         string :start index :end (incf index (* 2 it)))))
           (input () `((,(next 32) . ,(hex 4)) . (,(script) . ,(hex 4))))
           (output () `(,(hex 8) . ,(script))))
    (values (hex 4)
            (do* ((count (cpct)) (i 0 (1+ i)) (inputs (make-array count)))
                 ((= i count) inputs) (setf (aref inputs i) (input)))
            (do* ((count (cpct)) (i 0 (1+ i)) (outputs (make-array count)))
                 ((= i count) outputs) (setf (aref outputs i) (output)))
            (hex 4))))

(defun parse-block-txs (string &aux (index 160))
  (declare (optimize debug))
  (labels ((next (n) (flip-bytes (subseq string index (incf index (* 2 n)))))
           (hex (n) (parse-integer (next n) :radix 16))
           (cpct () (acase (hex 1)
                      (#xFF (hex 8)) (#xFE (hex 4)) (#xFD (hex 2)) (t it)))
           (script () (alet (cpct)
                        (ironclad:hex-string-to-byte-array
                         string :start index :end (incf index (* 2 it)))))
           (ipt () `((,(next 32) . ,(hex 4)) . (,(script) . ,(hex 4))))
           (opt () `(,(hex 8) . ,(script))))
    (macrolet ((vecof (body &optional (count '(cpct)))
                 `(do* ((count ,count) (i 0 (1+ i)) (acc (make-array count)))
                       ((= i count) acc) (setf (aref acc i) ,body))))
      (vecof (multiple-value-call #'vector                ;
               (values (hex 4))                           ; technical debt
               (alet (cpct)                               ; in car nate !
                 (when (zerop it) (assert (= 1 (hex 1)))) ;
                 (if (not (zerop it)) (values (vecof (ipt) it) (vecof (opt)))
                     (alet (cpct) (values (vecof (ipt) it) (vecof (opt))
                                          (vecof (vecof (script)) it)))))
               (values (hex 4)))))))

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
