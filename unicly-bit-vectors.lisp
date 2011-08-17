;;; :FILE-CREATED <Timestamp: #{2011-08-13T19:18:12-04:00Z}#{11326} - by MON>
;;; :FILE unicly/unicly-bit-vectors.lisp
;;; ==============================


;;; ==============================
;; :NOTE Per RFC 4.1.3 bit 48 should always be 0.
;; The UUID as bit field:
;; WEIGHT   INDEX      OCTETS                     BIT-FIELD-PER-OCTET
;;    4  | (0  31)  | 255 255 255 255         | #*11111111 #*11111111 #*11111111 #*11111111  | %uuid_time-low               | uuid-ub32
;;    2  | (32 47)  | 255 255                 | #*11111111 #*11111111                        | %uuid_time-mid               | uuid-ub16
;;    2  | (48 63)  | 255 255                 | #*11111111 #*11111111                        | %uuid_time-high-and-version  | uuid-ub16
;;    1  | (64 71)  | 255                     | #*11111111                                   | %uuid_clock-seq-and-reserved | uuid-ub8
;;    1  | (72 79)  | 255                     | #*11111111                                   | %uuid_clock-seq-low          | uuid-ub8
;;    6  | (80 127) | 255 255 255 255 255 255 | #*11111111 #*11111111 #*11111111 #*11111111 #*11111111 #*11111111 | %uuid_node | uuid-ub48
;;
;; The uuid as bit vector
;; (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba"))
;;   0      7       15      23      31      39      47      55       63     71      79      87      95      103     111     119     127
;;   !      !       !       !       !       !       !       !        !      !       !       !       !       !       !       !       !  
;; #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111  
;;
;; The uuid as binary number
;; #b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
;;  => 340282366920938463463374607431768211455
;; 
;; (integer-length 340282366920938463463374607431768211455) => 128
;;
;; 340,282,366,920,938,463,463,374,607,431,768,211,455 340 undecillion, 282
;; decillion, 366 nonillion, 920 octillion, 938 septillion, 463 sextillion, 463
;; quintillion, 374 quadrillion, 607 trillion, 431 billion, 768 million, 211
;; thousand and 455
;;; ==============================


(in-package #:unicly)
;; *package*

(declaim (inline uuid-bit-vector-128-zeroed
                 uuid-bit-vector-48-zeroed
                 uuid-bit-vector-32-zeroed
                 uuid-bit-vector-16-zeroed
                 uuid-bit-vector-8-zeroed))
;; uuid-bit-vector-<N>-zeroed   
(def-uuid-bit-vector-zeroed  128)
(def-uuid-bit-vector-zeroed   48)
(def-uuid-bit-vector-zeroed   32)
(def-uuid-bit-vector-zeroed   16)
(def-uuid-bit-vector-zeroed    8)

;; (uuid-bit-vector-32-zeroed)

;;; ==============================
;; :NOTE For 1mil comparisons of two uuid-bit-vectors following timing support
;; the current implementation using `sb-int:bit-vector-=':
;; comparison with `cl:equal' completes in 251,812,778 cycles, 0.083328 run-time
;; comparison with `sb-int:bit-vector-=' without declarations completes in in ~191,959,447 cycles, 0.063329 run-time
;; comparison with `sb-int:bit-vector-=' with declartions completes in ~170,661,197, 0.05555199 run-time
;; The alternative definition is not altogether inferior but can't be made surpass SBCL's internal transforms
(defun uuid-bit-vector-eql (uuid-bv-a uuid-bv-b)
  (declare (uuid-bit-vector-128 uuid-bv-a uuid-bv-b)
           (optimize (speed 3)))
  #-sbcl 
  (if (and (= (count 0 uuid-bv-a :test #'=) (count 0 uuid-bv-b :test #'=))
           (= (count 1 uuid-bv-a :test #'=) (count 1 uuid-bv-b :test #'=)))
      (loop 
         for low-idx from 0 below 64
         for top-idx = (logxor low-idx 127)
         always (and (= (sbit uuid-bv-a low-idx) (sbit uuid-bv-b low-idx))
                     (= (sbit uuid-bv-a top-idx) (sbit uuid-bv-b top-idx)))))
  #+sbcl 
  (SB-INT:BIT-VECTOR-= uuid-bv-a uuid-bv-b))

(defun %uuid-bit-vector-null-p (bit-vector-maybe-null)
  (declare (uuid-bit-vector-128 bit-vector-maybe-null)
           (inline uuid-bit-vector-128-zeroed)
           (optimize (speed 3)))
  (the (values (member t nil) &optional)
    (uuid-bit-vector-eql bit-vector-maybe-null (the uuid-bit-vector-128 (uuid-bit-vector-128-zeroed)))))
                                               
(declaim (inline uuid-bit-vector-null-p))
(defun uuid-bit-vector-null-p (bit-vector-maybe-null)
  (declare (optimize (speed 3)))
  (the boolean (typep bit-vector-maybe-null 'uuid-bit-vector-null)))

;; :COURTESY Zach Beane :DATE 2011-04-08
;; :SOURCE (URL `http://paste.lisp.org/+2LKZ/2')
(defun uuid-octet-to-bit-vector-8 (octet)
  (declare (type uuid-ub8 octet)
           (inline uuid-bit-vector-8-zeroed)
           (optimize (speed 3)))
  (let ((bv8 (the uuid-bit-vector-8 (uuid-bit-vector-8-zeroed))))
    (declare (type uuid-bit-vector-8 bv8))
    (dotimes (i 8 bv8)
      (setf (sbit bv8 i) (ldb (byte 1 7) octet))
      (setf octet (logand #xFF (ash octet 1))))))

(declaim (inline uuid-deposit-octet-to-bit-vector))
(defun uuid-deposit-octet-to-bit-vector (octet offset uuid-bv)
  (declare (type uuid-ub8 octet)
           (uuid-bit-vector-128 uuid-bv)
           ((mod 129) offset)
           (optimize (speed 3)))
  (loop 
     for idx upfrom offset below (+ offset 8)
     do (setf (sbit uuid-bv idx) (ldb (byte 1 7) octet))
     (setf octet (logand #xFF (ash octet 1)))
     finally (return uuid-bv)))

(defun uuid-byte-array-to-bit-vector (uuid-byte-array)
  (declare (uuid-byte-array-16 uuid-byte-array)
           (inline uuid-deposit-octet-to-bit-vector
                   %uuid-byte-array-null-p)
           (optimize (speed 3)))
  (let ((uuid-bv128 (uuid-bit-vector-128-zeroed)))
    (declare (uuid-bit-vector-128 uuid-bv128))
    (when (%uuid-byte-array-null-p uuid-byte-array)
      (return-from uuid-byte-array-to-bit-vector uuid-bv128))
    (loop 
       for byte across uuid-byte-array
       for offset upfrom 0 by 8 below 128
       do (uuid-deposit-octet-to-bit-vector byte offset uuid-bv128)
       finally (return  uuid-bv128))))

(declaim (inline %uuid-version-bit-vector-if))
(defun %uuid-version-bit-vector-if (uuid-bit-vector)
  ;; :TEST (signals succesfully)
  ;; (let ((bv-z (uuid-bit-vector-128-zeroed)))
  ;;         (setf (sbit bv-z 48) 1)
  ;;         (%uuid-version-bit-vector-if bv-z))
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (optimize (speed 3)))
  (unless (zerop (sbit uuid-bit-vector 48))
    (error 'uuid-bit-48-error  :uuid-bit-48-error-datum uuid-bit-vector)))

;;; ==============================
;; :NOTE Return value has the integer representation: 267678999922476945140730988764022209929
;; (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ"))
;; (uuid-bit-vector-to-integer (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ")))
(defun uuid-to-bit-vector (uuid)
  (declare (type unique-universal-identifier uuid)
           (inline uuid-disassemble-ub32 
                   uuid-disassemble-ub16
                   uuid-bit-vector-128-zeroed 
                   %unique-universal-identifier-null-p)
           (optimize (speed 3)))
  (when (%unique-universal-identifier-null-p uuid)
    (return-from uuid-to-bit-vector (uuid-bit-vector-128-zeroed)))
  (let ((bv-lst 
         (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
                      %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
             uuid
           (declare (type uuid-ub32 %uuid_time-low)
                    (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
                    (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
                    (type uuid-ub48 %uuid_node))
           (multiple-value-call #'list 
             (uuid-disassemble-ub32 %uuid_time-low)
             (uuid-disassemble-ub16 %uuid_time-mid)
             (uuid-disassemble-ub16 %uuid_time-high-and-version)
             %uuid_clock-seq-and-reserved
             %uuid_clock-seq-low
             (uuid-disassemble-ub48 %uuid_node))))
        (uuid-bv128 (the uuid-bit-vector-128 (uuid-bit-vector-128-zeroed))))
    (declare (list bv-lst) (uuid-bit-vector-128 uuid-bv128))
    (loop 
       for byte in bv-lst
       for offset upfrom 0 by 8 below 128
       do (uuid-deposit-octet-to-bit-vector (the uuid-ub8 byte) offset uuid-bv128)
       finally (return uuid-bv128))))

;;; ==============================
;;  48 49 50 51
;; | 0  0  0  1  | 1  The time-based version specified in this document.
;; | 0  0  1  0  | 2  DCE Security version, with embedded POSIX UIDs.
;; | 0  0  1  1  | 3  The name-based MD5
;; | 0  1  0  0  | 4  The randomly or pseudo-randomly generated version
;; | 0  1  0  1  | 5  The name-based SHA-1
(declaim (inline uuid-version-bit-vector))
(defun uuid-version-bit-vector (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline %uuid-version-bit-vector-if
                   uuid-bit-vector-128-zeroed
                   uuid-bit-vector-null-p)
           (optimize (speed 3)))
  (%uuid-version-bit-vector-if uuid-bit-vector)
  (ecase (the bit (sbit uuid-bit-vector 49))
    (0 (ecase (the bit (sbit uuid-bit-vector 50))
         (1 (ecase (the bit (sbit uuid-bit-vector 51))
              (1  3)
              (0  2)))
         (0 (ecase (the bit (sbit uuid-bit-vector 51))
              (1 1)
              ;; RFC4122 Setion 4.1.7. "Nil UUID" specifically requires a null UUID
              ;; However, it doesn't say anything about the version 0 UUID... 
              ;; Of course it wouldn't given how C centric nature of the entire RFC :)
              ;; So, as a way of flipping the bird to the curly brace inclined we
              ;; choose to return as if by `cl:values': 0, null-uuid
              (0 (values (or (and (uuid-bit-vector-null-p uuid-bit-vector) 0)
                             (error "something wrong with UUID-BIT-VECTOR bit field~% got: ~S" uuid-bit-vector))
                         'null-uuid))))))
    (1 (ecase (the bit (sbit uuid-bit-vector 51))
         (0 4)
         (1 5)))))

(declaim (inline uuid-bit-vector-v3-p))
(defun uuid-bit-vector-v3-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-version-bit-vector)
           (optimize (speed 3)))
  (let ((v3-if (uuid-version-bit-vector uuid-bit-vector)))
    (declare ((integer 0 5) v3-if))
    (= v3-if 3)))

(declaim (inline uuid-bit-vector-v4-p))
(defun uuid-bit-vector-v4-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-version-bit-vector)
           (optimize (speed 3)))
  (let ((v4-if (uuid-version-bit-vector uuid-bit-vector)))
    (declare ((integer 0 5) v4-if))
    (= v4-if 4)))

(declaim (inline uuid-bit-vector-v5-p))
(defun uuid-bit-vector-v5-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-version-bit-vector)
           (optimize (speed 3)))
  (let ((v5-if (uuid-version-bit-vector uuid-bit-vector)))
    (declare ((integer 0 5) v5-if))
    (= v5-if 5)))

;;; ==============================
;; :NOTE Following modeled after Stas's inclued at bottom of file:
(defun uuid-bit-vector-to-integer (bit-vector)
  (declare (uuid-bit-vector bit-vector)
           (optimize (speed 3)))
  (let* ((bv-length (the uuid-ub128-integer-length (length bit-vector)))
         (word-size (ash bv-length -1))
         (repeats   (floor bv-length word-size)) ;; in this case always two?
         (result    0)
         (index     -1))
    (labels ((build-word ()
               (loop 
                  repeat word-size
                  for j = 0 then (logior (sbit bit-vector (incf index))
                                         (ash j 1))
                  finally (return j)))
             (loop-repeats ()
                (loop 
                   repeat repeats ;; (floor bv-length word-size)
                   do (setf result (logior (build-word)
                                           (ash result (1- word-size))))))
             (loop-less-index ()
                (loop 
                   while (< index (1- bv-length))
                   do (setf result (logior (sbit bit-vector (incf index))
                                           (ash result 1)))))
             (loop-and-return ()
                (loop-repeats)
                (loop-less-index)
                result)
             (get-bv-128-int ()
               (declare  (uuid-bit-vector-128          bit-vector)
                         (uuid-bit-vector-128-length   bv-length)
                         ((uuid-bit-vector-length  64) word-size))
               (the uuid-ub128 (loop-and-return)))
             (get-bv-48-int ()
               (declare  (uuid-bit-vector-48           bit-vector)
                         (uuid-bit-vector-48-length    bv-length)
                         ((uuid-bit-vector-length 24)  word-size))
               (the uuid-ub48 (loop-and-return)))
             (get-bv-32-int ()
               (declare  (uuid-bit-vector-32           bit-vector)
                         (uuid-bit-vector-32-length    bv-length)
                         (uuid-bit-vector-16-length    word-size))
               (the uuid-ub32 (loop-and-return)))
             (get-bv-16-int ()
               (declare  (uuid-bit-vector-16           bit-vector)
                         (uuid-bit-vector-16-length    bv-length)
                         (uuid-bit-vector-8-length     word-size))
               (the uuid-ub16 (loop-and-return)))
             (get-bv-8-int ()
               (declare  (uuid-bit-vector-8            bit-vector)
                         (uuid-bit-vector-8-length     bv-length)
                         ((uuid-bit-vector-length 4)   word-size))
               (the uuid-ub8 (loop-and-return))))
      (etypecase bv-length 
        (uuid-bit-vector-128-length (get-bv-128-int))
        ;; `%uuid_node'
        (uuid-bit-vector-48-length  (get-bv-48-int))     
        ;; `%uuid_time-low'
        (uuid-bit-vector-32-length  (get-bv-32-int))     
        ;; `%uuid_time-mid', `%uuid_time-high-and-version'
        (uuid-bit-vector-16-length  (get-bv-16-int))     
        ;; `%uuid_clock-seq-and-reserved', `%uuid_clock-seq-low'
        (uuid-bit-vector-8-length   (get-bv-8-int))))))

;;
;; (defun uuid-bit-vector-get-sub-range (bit-vector offset length)
;;  (declare (uuid-bit-vector bit-vector)
;;           ((uuid-bit-vector-index 128) start length))
;;
;; (defun uuid-bit-vector-node-integer (bv)
;;    (uuid-bit-vector-48-to-integer (uuid-bit-vector-get-sub-range <bv> <from> <to>) 
;;       
;; (defun uuid-bit-vector-clock-seq-and-reserved-integer (bv)
;; (defun uuid-bit-vector-clock-seq-low-integer (bv)
;; (defun uuid-bit-vector-time-low-integer (bv)
;; (defun uuid-bit-vector-time-mid-integer (bv)
;; (defun uuid-bit-vector-time-high-and-version-integer (bv)
;; 

;;; ==============================
;; uuid-bit-vector-~A-integer
;; node
;; clock-seq-and-reserved
;; clock-seq-low
;; time-low
;; time-mid
;; time-high-and-version
;;; ==============================

;;
;; (defun uuid-bit-vector-8-to-integer (bv)
;; (defun uuid-bit-vector-8-to-octet    (bv)
;;
;; `%uuid_time-mid' `%uuid_time-high-and-version' `uuid-ub16'
;; (defun uuid-bit-vector-16-range (bv) 

;;
;; `%uuid_time-low' `uuid-ub32'
;; (defun uuid-bit-vector-32-range (bv) 

;;
;; `%uuid_node' `uuid-ub48'
;; (defun uuid-bit-vector-48-to-integer (bv)

;;
;; (defun uuid-bit-vector-16-to-octets  (bv)
;; (defun uuid-bit-vector-32-to-octets  (bv)
;; (defun uuid-bit-vector-48-to-octets  (bv)
;; (defun uuid-bit-vector-deposit-octet-to-byte-array (bv)
;; (defun uuid-bit-vector-to-uuid-byte-array  (bv)
;;
;; (uuid-bit-vector-to-uuid (bv)

;;; ==============================

;;; ==============================
;; :TODO uuid-from-bit-vector
;;
;; (defun %uuid-from-bit-vector-if (uuid-bit-vector-128)
;;; (declare (uuid-bit-vector-128 uuid-bit-vector-128))
;;   (let ((bv-version-if (uuid-version-bit-vector uuid-bit-vector-128)))
;;  (ecase bv-version-if
;;   (0 (return-from 'uuid-from-bit-vector (make-instance 'unique-universal-identifier-null)))
;; (2  
;; (let ((uuid-bit-vector-128 (uuid-to-bit-vector (make-v4-uuid))))
;;   (error 'uuid-simple-error
;;          :format-control
;;          "Arg UUID-BIT-VECTOR-128 is a uuid version 2.~%~@
;;                    The function `unicly:make-v2-uuid' is unimplemented.~%~@
;;                    got UUID-BIT-VECTOR-128 with subseq [48,63]:~%~T~S~%"
;;          :format-arguments
;;          (list (subseq uuid-bit-vector-128 48 63))))
;;   ((3 4 5) bv-version-if)
;;  

;; (defun uuid-from-bit-vector (uuid-bit-vector-128)
;;; (declare (uuid-bit-vector-128 uuid-bit-vector-128))


;;; ==============================
;; :SOURCE usenet-legend/io.lisp :WAS `bit-vector-octets'
;; :TODO Convert this into `uuid-bit-vector-to-byte-array'
;; (defun bit-vector-octets (bv)
;;   (declare (type simple-bit-vector bv)
;;            (optimize speed))
;;   (let ((octets (make-array (ceiling (length bv) 8)
;;                             :element-type 'octet
;;                             :initial-element 0)))
;;     (loop for bit across bv
;;           for i from 0 below (length bv)
;;           do (multiple-value-bind (j k)
;;                  (floor i 8)
;;                (setf (aref octets j)
;;                      (logior (ash bit k) (aref octets j)))))
;;     (values octets
;;             (length bv))))
;;; ==============================



;;; ==============================
;;; :DEPRECATED
;;; ==============================
;;
;;
;; (defmacro declared-uuid-array-zeroed-of-size (size array-type array-elt-type)
;;   ;; (macroexpand-1 '(declared-uuid-array-zeroed-of-size 16 uuid-byte-array-16 uuid-ub8))
;;   `(the ,array-type
;;      (make-array ,size :element-type ',array-elt-type :initial-element 0)))
;;
;; (defmacro declared-uuid-bit-vector-of-size-zeroed (size bit-vector-type)
;;   ;; (macroexpand-1 '(declared-uuid-bit-vector-of-size-zeroed 16 uuid-bit-vector-16))
;;   ;; `(the ,bit-vector-type (make-array ,size :element-type 'bit :initial-element 0))
;;   `(declared-uuid-array-zeroed-of-size ,size ,bit-vector-type bit))
;;
;; (defmacro def-uuid-bit-vector-zeroed (zeroed-size)
;;   ;; (macroexpand-1 (def-uuid-bit-vector-zeroed 
;;   (let ((interned-bv-zeroed-name (%def-uuid-format-and-intern-symbol "UUID-BIT-VECTOR-~D-ZEROED" zeroed-size)))
;;     `(defun ,interned-bv-zeroed-name ()
;;        (uuid-bit-vector-of-size-zeroed ,zeroed-size))))
;;
;; (defun uuid-bit-vector-of-size-zeroed (size)
;;   (declare (uuid-bit-vector-valid-length size)
;;            (optimize (speed 3)))
;;   (etypecase size
;;     (uuid-bit-vector-128-length
;;      (declared-uuid-bit-vector-of-size-zeroed size uuid-bit-vector-128))
;;     (uuid-bit-vector-48-length  
;;      (declared-uuid-bit-vector-of-size-zeroed size uuid-bit-vector-48))
;;     (uuid-bit-vector-32-length  
;;      (declared-uuid-bit-vector-of-size-zeroed size uuid-bit-vector-32))
;;     (uuid-bit-vector-16-length
;;      (declared-uuid-bit-vector-of-size-zeroed size uuid-bit-vector-16))
;;     (uuid-bit-vector-8-length 
;;      (declared-uuid-bit-vector-of-size-zeroed size uuid-bit-vector-8))))
;;
;; :NOTE For both `uuid-bit-vector-128-zeroed' and `uuid-bit-vector-8-zeroed' we
;; assume that the returned array is always of type: (simple-bit-vector 128)
;; :SEE `uuid-verify-bit-vector-simplicity' in :FILE uuid-types.lisp
;; (declaim (inline uuid-bit-vector-128-zeroed))
;; (defun uuid-bit-vector-128-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;; (the uuid-bit-vector-128 (make-array 128 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-of-size-zeroed 128))
;;
;; (declaim (inline uuid-bit-vector-32-zeroed))
;; (defun uuid-bit-vector-32-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;;(the uuid-bit-vector-32 (make-array 32 :element-type 'bit :initial-element 0)))
;;   (uuid-bit-vector-of-size-zeroed 32))
;;
;; (declaim (inline uuid-bit-vector-16-zeroed))
;; (defun uuid-bit-vector-16-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;; (the uuid-bit-vector-16 (make-array 16 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-of-size-zeroed 16))
;;
;; (declaim (inline uuid-bit-vector-8-zeroed))
;; (defun uuid-bit-vector-8-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;;(the uuid-bit-vector-8 (make-array 8 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-of-size-zeroed 8))
;;; ==============================


;;; ==============================
;;; ==============================
;;; ==============================
;;; ==============================
;; :SOURCE (URL `http://www.lispforum.com/viewtopic.php?f=2&t=1205#p6269')
;; (defun uuid-bit-vector-to-integer (bit-vector)
;;   "Return BIT-VECTOR's representation as a positive integer."
;;    ;; (= (bit-vector-to-integer (uuid- (make-v4-uuid)
;;    ;; 122378404974049034400182615604361091930)
;;   (declare (bit-vector bit-vector)
;;            (optimize (speed 3)))
;;   ;; :NOTE We ought to be able to optimize around the size of expected return
;;   ;; value by taking the length of the bv which should not exceed the
;;   ;; integer-length of final return value.
;;   (flet ((bit-adder (first-bit second-bit)
;;            (+ (ash first-bit 1) second-bit)))
;;     (etypecase bit-vector
;;       (simple-bit-vector 
;;        (locally (declare (simple-bit-vector bit-vector))
;;          (reduce #'bit-adder bit-vector)))
;;       (bit-vector
;;        (reduce #'bit-adder bit-vector)))))
;;; ==============================
;; :PASTE-DATE 2011-08-10
;; :PASTE-TITLE "Annotation number 1: another version"
;; :PASTED-BY 	Xach
;; :PASTE-URL (URL `http://paste.lisp.org/+2NN1/1')
;; (defun uuid-bit-vector-to-integer (bit-vector)
;;   "Return BIT-VECTOR's representation as a positive integer."
;;   (let ((j 0))
;;     (dotimes (i (length bit-vector) j)
;;       (setf j (logior (bit bit-vector i)
;;                       (ash j 1))))))
;;; ==============================
;; :PASTE-DATE 2011-08-10
;; :PASTE-TITLE "Annotation number 2: a faster version"
;; :PASTED-BY stassats
;; :PASTE-URL (URL `http://paste.lisp.org/+2NN1/2')
;; (defun uuid-bit-vector-to-integer (bit-vector) ;; (uuid-bit-vector <SIZE>)
;;   (let* ((bv-length (length bit-vector))       ;; uuid-ub128-integer-length
;;          (word-size 64) ;; (ash bv-length 1)   ;; uuid-ub128-integer-length
;;          (result 0)
;;          (index -1))
;;     (flet ((build-word ()
;;              (loop 
;;                 repeat word-size
;;                 for j = 0 then (logior (bit bit-vector (incf index))
;;                                        (ash j 1))
;;                 finally (return j))))
;;       (loop 
;;          repeat (floor bv-length word-size)
;;          do (setf result (logior (build-word)
;;                                  (ash result (1- word-size)))))
;;       (loop 
;;          while (< index (1- bv-length))
;;          do (setf result (logior (bit bit-vector (incf index))
;;                                  (ash result 1)))))
;;     result))
;;; ==============================
;;; ==============================
;;; ==============================



;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
