;;; :FILE-CREATED <Timestamp: #{2011-04-11T11:51:10-04:00Z}#{11151} - by MON>
;;; :FILE unicly/unicly-types.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

(deftype uuid-unsigned-byte-size (byte-size)
  `(unsigned-byte ,byte-size))

(deftype uuid-ub128 ()
 '(uuid-unsigned-byte-size 128))

(deftype uuid-ub64 ()
 '(uuid-unsigned-byte-size 64))

(deftype uuid-ub48 ()
 '(uuid-unsigned-byte-size 48))

(deftype uuid-ub32 ()
 '(uuid-unsigned-byte-size 32))

(deftype uuid-ub24 ()
  '(uuid-unsigned-byte-size 24))

(deftype uuid-ub16 ()
  '(uuid-unsigned-byte-size 16))

(deftype uuid-ub8 ()
  '(uuid-unsigned-byte-size 8))

;; 128 8 bits
;; 64  7 bits
;; 32  6 bits
;; 16  5 bits
;; 8   nibble

(deftype uuid-unsigned-byte-integer-length (integer)
  `(mod ,integer))

(deftype uuid-ub128-integer-length ()
  '(uuid-unsigned-byte-integer-length 129))

(deftype uuid-ub64-integer-length ()
  '(uuid-unsigned-byte-integer-length 65))

(deftype uuid-ub48-integer-length ()
  '(uuid-unsigned-byte-integer-length 49))

(deftype uuid-ub32-integer-length ()
  '(uuid-unsigned-byte-integer-length 33))

(deftype uuid-ub24-integer-length ()
  '(uuid-unsigned-byte-integer-length 24))

(deftype uuid-ub16-integer-length ()
  '(uuid-unsigned-byte-integer-length 17))

(deftype uuid-ub8-integer-length ()
  '(uuid-unsigned-byte-integer-length 9))

;; 
;; (deftype uuid-fixnum-bit-width ()
;;   #-sbcl '(integer 0 #.(integer-length most-positive-fixnum))
;;   #+sbcl '(integer 0 #.sb-vm:n-positive-fixnum-bits))

;; (deftype uuid-bignum-bit-width ()
;;   #-sbcl '(integer #.(integer-length most-positive-fixnum) *)
;;   ;; :NOTE x86-32 only
;;   ;; #+sbcl '(integer #.sb-vm:n-positive-fixnum-bits #.(- (expt 2 24) 2))
;;   ;; not following range is 1 under sb-bignum::maximum-bignum-length for congruency with the x86-32 above.
;;   ;; I'm assuming this is correct for other procs/architectures...
;;   #+sbcl '(integer #.sb-vm:n-positive-fixnum-bits #.(1- sb-bignum::maximum-bignum-length)))

;; :NOTE (upgraded-array-element-type (type-of (make-array 128 :element-type 'bit :initial-element 0))) => T
(deftype uuid-bit-vector (&optional size)
   (let ((sz (or size '*)))
     `(simple-bit-vector ,sz)))

(deftype uuid-bit-vector-128 ()
  '(uuid-bit-vector 128))

(deftype uuid-bit-vector-48 ()
  '(uuid-bit-vector 48))

(deftype uuid-bit-vector-32 ()
  '(uuid-bit-vector 32))

(deftype uuid-bit-vector-16 ()
  '(uuid-bit-vector 16))

(deftype uuid-bit-vector-8 ()
  '(uuid-bit-vector 8))

(deftype uuid-bit-vector-length (size)
  `(integer ,size ,size))

(deftype uuid-bit-vector-128-length ()
  '(uuid-bit-vector-length 128))

(deftype uuid-bit-vector-48-length ()
  '(uuid-bit-vector-length 48))

(deftype uuid-bit-vector-32-length ()
  '(uuid-bit-vector-length 32))

(deftype uuid-bit-vector-16-length ()
  '(uuid-bit-vector-length 16))

(deftype uuid-bit-vector-8-length ()
  '(uuid-bit-vector-length 8))

(deftype uuid-bit-vector-null ()
  '(and uuid-bit-vector-128
    (satisfies %uuid-bit-vector-null-p)))

(deftype uuid-byte-string ()
  '(simple-array character (16)))

(deftype uuid-byte-array (&optional size)
  (let ((sz (or size '*)))
    `(simple-array uuid-ub8 (,sz))))

;; UUID v3 MD5 returns an array of type: (simple-array (unsigned-byte 8) (16))
(deftype uuid-byte-array-16 ()
  ;; expands to: (simple-array (unsigned-byte 8) (16)))
  '(uuid-byte-array 16))

(deftype uuid-byte-array-null ()
  ;; expands to: (simple-array (unsigned-byte 8) (16)))
  '(and uuid-byte-array-16 (satisfies %uuid-byte-array-null-p)))

;; UUID v5 SHA1 returns an array of type: (simple-array (unsigned-byte 8) (20))
(deftype uuid-byte-array-20 ()
  ;; expands to: (simple-array (unsigned-byte 8) (20))
  '(uuid-byte-array 20))

(deftype uuid-string-32 ()
  '(array character (32)))

(deftype uuid-string-36 ()
  '(array character (36)))

(deftype uuid-hex-string-32 ()
  '(and uuid-string-32 (satisfies string-all-hex-char-p)))

(deftype uuid-hex-string-36 ()
  '(and uuid-string-36 (satisfies uuid-hex-string-36-p)))


;;; ==============================
;;; :UUID-TYPE-PREDICATES
;;; ==============================

;;; :NOTE Following definition is unused on the assumption that it is guaranteed
;;;  that the following always returns (SIMPLE-BIT-VECTOR 0) on all implementations:
;;;  (type-of (make-array 0 :element-type 'bit :initial-element 0))
(defun uuid-verify-bit-vector-simplicity (putative-simple-bit-vector)
  (declare (bit-vector putative-simple-bit-vector)
           (optimize (speed 3)))
  (and (eql (array-element-type putative-simple-bit-vector) 'bit)
       (null (adjustable-array-p putative-simple-bit-vector))
       (null (array-has-fill-pointer-p putative-simple-bit-vector))))

(defun uuid-bit-vector-128-p (maybe-uuid-bit-vector-128)
  (typep maybe-uuid-bit-vector-128 'uuid-bit-vector-128))

(defun uuid-string-32-p (maybe-uuid-string-32)
  (typep maybe-uuid-string-32 'uuid-string-32))

(declaim (inline uuid-string-36-p))
(defun uuid-string-36-p (maybe-uuid-string-36)
  (declare (optimize (speed 3)))
  (typep maybe-uuid-string-36 'uuid-string-36))

(defun uuid-byte-array-p (maybe-uuid-byte-array)
  (typep maybe-uuid-byte-array 'uuid-byte-array))

(defun uuid-byte-array-16-p (maybe-uuid-byte-array-16)
  (typep maybe-uuid-byte-array-16 'uuid-byte-array-16))

(defun uuid-byte-array-20-p (maybe-uuid-byte-array-20)
  (typep maybe-uuid-byte-array-20 'uuid-byte-array-20))

(declaim (inline %uuid-byte-array-null-p))
(defun %uuid-byte-array-null-p (byte-array-maybe-null)
  ;; (%uuid-byte-array-null-p (uuid-byte-array-zeroed))
  ;; (%uuid-byte-array-null-p (make-array 16 :element-type 'uuid-ub8 :initial-element 1))
  (declare (uuid-byte-array-16 byte-array-maybe-null)
           (optimize (speed 3)))
  (loop for x across byte-array-maybe-null always (zerop x)))

;; (uuid-byte-array-null-p (make-array 20 :element-type 'uuid-ub8 :initial-element 1))

(defun uuid-byte-array-null-p (byte-array-maybe-null)
  (typep byte-array-maybe-null 'uuid-byte-array-null))

(defun uuid-byte-string-p (maybe-uuid-byte-string)
  (typep maybe-uuid-byte-string 'uuid-byte-string))

(defun uuid-hex-string-32-p (maybe-uuid-hex-string-32)
  (typep maybe-uuid-hex-string-32 'uuid-hex-string-32))

(declaim (inline uuid-hex-string-36-p))
(defun uuid-hex-string-36-p (maybe-uuid-hex-string-36)
  (declare (inline string-all-hex-char-p
                   uuid-string-36-p)
           (optimize (speed 3)))
  (when (uuid-string-36-p maybe-uuid-hex-string-36)
    (let ((split-uuid 
           (split-sequence:split-sequence-if #'(lambda (x) 
                                                 (declare (character x))
                                                 (char= #\- x))
                                             (the uuid-string-36 maybe-uuid-hex-string-36))))
      (declare (list split-uuid))
      (and (= (length split-uuid) 5)
           (equal (map 'list #'length split-uuid) (list 8 4 4 4 12))
           (loop
              for chk-hex in split-uuid
              always (string-all-hex-char-p (the string chk-hex)))))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
