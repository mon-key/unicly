;;; :FILE-CREATED <Timestamp: #{2011-08-17T15:58:07-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-byte-arrays.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

(declaim (inline uuid-byte-array-16-zeroed))
(defun uuid-byte-array-16-zeroed ()
  (declare (optimize (speed 3)))
  (the uuid-byte-array-16
    (make-array (the uuid-bit-vector-16-length 16) :element-type 'uuid-ub8 :initial-element 0)))

(defun uuid-get-namespace-bytes (uuid)
  (declare (type unique-universal-identifier uuid)
           (inline uuid-byte-array-16-zeroed  %unique-universal-identifier-null-p)
           (optimize (speed 3)))
  (when (%unique-universal-identifier-null-p uuid)
    (return-from uuid-get-namespace-bytes (the uuid-byte-array-16 (uuid-byte-array-16-zeroed))))
  (the uuid-byte-array-16
    (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
                                %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
        uuid
      (declare (type uuid-ub32 %uuid_time-low)
               (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
               (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
               (type uuid-ub48 %uuid_node))
      (make-array 16 
                  :element-type 'uuid-ub8
                  :initial-contents (multiple-value-call #'list 
                                      (uuid-disassemble-ub32 %uuid_time-low)
                                      (uuid-disassemble-ub16 %uuid_time-mid)
                                      (uuid-disassemble-ub16 %uuid_time-high-and-version)
                                      %uuid_clock-seq-and-reserved
                                      %uuid_clock-seq-low
                                      (uuid-disassemble-ub48 %uuid_node))))))
;;
;; (progn 
;;   (defparameter *tt--uuid-v4* (make-v4-uuid))
;;   (unwind-protect 
;;        (equalp (uuid-to-byte-array *tt--uuid-v4*)
;;                (uuid-get-namespace-bytes *tt--uuid-v4*))
;;     (unintern '*tt--uuid-v4*)))
;;
;;; ==============================
;;
;; :NOTE UNICLY:UUID-GET-NAMESPACE-BYTES is equivalent to
;; UUID:UUID-TO-BYTE-ARRAY we provide it here for congruence. 
;; :SEE Bottom of file for our variation of the original definition.
;; 
(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'uuid-to-byte-array) 
        (fdefinition 'uuid-get-namespace-bytes)))

;;; ==============================
;; :TODO We can get rid of the macrolet and dispatch with the %uuid-*-request
;; fncns However, `%uuid_time-high-and-version' requires that we first find the
;; uuid-version represented by BYTE-ARRAY in order to properly disassemble b/c
;; not currently comfortable with the thought of accidentally messing up the
;; version bits.
(defun uuid-from-byte-array (byte-array)
  ;; :NOTE We declare this a uuid-byte-array-16 even though SHA-1s are arrays of 20 elts
  ;; IOW if we call this from uuid-digest-uuid-instance we deserve to fail.
  (declare (type uuid-byte-array-16 byte-array)
           (inline %uuid-byte-array-null-p))
  #-sbcl (assert (uuid-byte-array-p byte-array) (byte-array)
                 "Arg BYTE-ARRAY does not satisfy `uuid-byte-array-p'")
  (when (%uuid-byte-array-null-p byte-array)
    (return-from uuid-from-byte-array 
      ;; Remember, there can only be one *uuid-null-uuid*!
      (make-instance 'unique-universal-identifier)))
  (macrolet ((arr-to-bytes (from to w-array)
               "Helper macro used in `uuid-from-byte-array'."
               (declare ((mod 17) from to))
               `(loop 
                   for i from ,from to ,to
                   with res = 0
                   do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,w-array i))
                   finally (return res))))
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low (the uuid-ub32 (arr-to-bytes 0 3 byte-array))
                   :%uuid_time-mid (the uuid-ub16 (arr-to-bytes 4 5 byte-array))
                   :%uuid_time-high-and-version (the uuid-ub16 (arr-to-bytes 6 7 byte-array))
                   :%uuid_clock-seq-and-reserved (the uuid-ub8 (aref byte-array 8))
                   :%uuid_clock-seq-low (the uuid-ub8 (aref byte-array 9))
                   :%uuid_node (the uuid-ub48 (arr-to-bytes 10 15 byte-array)))))

;;; ==============================
;;; :TODO Finish `uuid-byte-array-version'
;; (defun uuid-byte-array-version (uuid-byte-array)
;;  (declare (uuid-byte-array-16 uuid-byte-array))

;; :SOURCE cl-crypto/source/rsa.lisp
#+(or)
(defun num->byte-array (num)
  (let* ((num-bytes (truncate (+ (integer-length num) 7) 8))
	 (num-bits (* num-bytes 8))
	 (out (make-array num-bytes :element-type '(unsigned-byte 8))))
    (dotimes (i num-bytes)
      (setf (aref out i) (ldb (byte 8 (- num-bits (* (1+ i) 8))) num)))
    out))

;; :TODO Refactor this to test earlier for integer-length less than 120.
(defun uuid-number-byte-array (uuid-integer)
  (declare (optimize (speed 0) (debug 3)))
  (when (zerop uuid-integer) 
    (return-from uuid-number-byte-array (uuid-byte-array-16-zeroed)))
  (let* ((octet-count (nth-value 0 (truncate (+ (integer-length uuid-integer) 7) 8)))
         (bit-count   (ash octet-count 3))
         (ba-out      (uuid-byte-array-16-zeroed))
         (chk-byte   '()))
    (dotimes (cnt 16 
              (if (evenp octet-count)
                  ba-out
                  ;; when the top bits of the class unique-universal-identifier
                  ;; are such that the uuid has an integer representation with
                  ;; integer-length less than 120 we need to pad the array. On
                  ;; current system this will happen for 1 in 200 make-v4-uuid's
                  (loop
                     with offset = ba-out
                     with new = (uuid-byte-array-16-zeroed)  
                     for x across offset 
                     for y from 1 below 16
                     do (setf (aref new y) x)
                     finally (return new))))
      (setf chk-byte (- bit-count (ash (1+ cnt) 3)))
      (if (minusp chk-byte)
          (setf (aref ba-out cnt) 
                (ldb (byte 8 0) uuid-integer))
          (setf (aref ba-out cnt) 
                (ldb (byte 8 chk-byte) uuid-integer))))))


;;; ==============================
;;; EOF