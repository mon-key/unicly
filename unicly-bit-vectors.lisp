;;; :FILE-CREATED <Timestamp: #{2011-08-13T19:18:12-04:00Z}#{11326} - by MON>
;;; :FILE unicly/unicly-bit-vectors.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

(defun uuid-bit-vector-zeroed-of-size (size)
  (declare (uuid-bit-vector-valid-length size)
           (optimize (speed 3)))
  (etypecase size
    (uuid-bit-vector-128-length
     (declared-uuid-bit-vector-zeroed-of-size size uuid-bit-vector-128))
    (uuid-bit-vector-48-length  
     (declared-uuid-bit-vector-zeroed-of-size size uuid-bit-vector-48))
    (uuid-bit-vector-32-length  
     (declared-uuid-bit-vector-zeroed-of-size size uuid-bit-vector-32))
    (uuid-bit-vector-16-length
     (declared-uuid-bit-vector-zeroed-of-size size uuid-bit-vector-16))
    (uuid-bit-vector-8-length 
     (declared-uuid-bit-vector-zeroed-of-size size uuid-bit-vector-8))))

(declaim (inline uuid-byte-array-zeroed))
(defun uuid-byte-array-zeroed ()
    (declare (optimize (speed 3)))
    ;; (the uuid-byte-array-16
    ;;   (make-array 16 :element-type 'uuid-ub8 :initial-element 0))
    (declared-uuid-array-zeroed-of-size 16 uuid-byte-array-16 uuid-ub8))

(declaim (inline uuid-bit-vector-zeroed
                 uuid-bit-vector-32-zeroed
                 uuid-bit-vector-16-zeroed
                 uuid-bit-vector-8-zeroed))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;
(def-uuid-bit-vector-zeroed uuid-bit-vector-zeroed    128)
(def-uuid-bit-vector-zeroed uuid-bit-vector-48-zeroed 48)
(def-uuid-bit-vector-zeroed uuid-bit-vector-32-zeroed 32)
(def-uuid-bit-vector-zeroed uuid-bit-vector-16-zeroed 16)
(def-uuid-bit-vector-zeroed uuid-bit-vector-8-zeroed  8)
;; )

;;; ==============================  
;; :NOTE For both `uuid-bit-vector-zeroed' and `uuid-bit-vector-8-zeroed' we
;; assume that the returned array is always of type: (simple-bit-vector 128)
;; :SEE `uuid-verify-bit-vector-simplicity' in :FILE uuid-types.lisp
;; (declaim (inline uuid-bit-vector-zeroed))
;; (defun uuid-bit-vector-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;; (the uuid-bit-vector-128 (make-array 128 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-zeroed-of-size 128))
;;
;; (declaim (inline uuid-bit-vector-32-zeroed))
;; (defun uuid-bit-vector-32-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;;(the uuid-bit-vector-32 (make-array 32 :element-type 'bit :initial-element 0)))
;;   (uuid-bit-vector-zeroed-of-size 32))
;;
;; (declaim (inline uuid-bit-vector-16-zeroed))
;; (defun uuid-bit-vector-16-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;; (the uuid-bit-vector-16 (make-array 16 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-zeroed-of-size 16))
;;
;; (declaim (inline uuid-bit-vector-8-zeroed))
;; (defun uuid-bit-vector-8-zeroed ()
;;   (declare (optimize (speed 3)))
;;   ;;(the uuid-bit-vector-8 (make-array 8 :element-type 'bit :initial-element 0))
;;   (uuid-bit-vector-zeroed-of-size 8))
;;; ==============================

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
  ;; (%uuid-bit-vector-null-p (uuid-bit-vector-zeroed))
  ;; (%uuid-bit-vector-null-p (make-array 128 :element-type 'bit :initial-element 0))
  (declare (uuid-bit-vector-128 bit-vector-maybe-null)
            (inline uuid-bit-vector-zeroed)
           (optimize (speed 3)))
  (uuid-bit-vector-eql bit-vector-maybe-null (the uuid-bit-vector-128 
                                               (uuid-bit-vector-zeroed))))
(declaim (inline uuid-bit-vector-null-p))
(defun uuid-bit-vector-null-p (bit-vector-maybe-null)
  (declare (optimize (speed 3)))
  (typep bit-vector-maybe-null 'uuid-bit-vector-null))

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
;; :NOTE Following modeled after Stas's version above
(defun uuid-bit-vector-to-integer (bit-vector)
  (declare (unicly:uuid-bit-vector bit-vector)
           (optimize (speed 3)))
  (let* ((bv-length (the unicly::uuid-ub128-integer-length (length bit-vector)))
         (word-size (ash bv-length -1))
         (repeats   (floor bv-length word-size)) ;; in this case always two?
         (result 0)
         (index -1))
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
                result))
      (ecase bv-length 
        (128 
         (locally 
             (declare  (uuid-bit-vector-128 bit-vector)
                       (uuid-bit-vector-128-length bv-length)
                       ((uuid-bit-vector-length  64) word-size))
           (the uuid-ub128 (loop-and-return))))
        (48 ;;  `%uuid_node'
         (locally 
             (declare  (unicly::uuid-bit-vector-48 bit-vector)
                       (unicly::uuid-bit-vector-48-length bv-length)
                       ((uuid-bit-vector-length 24) word-size))
           (the uuid-ub48 (loop-and-return))))
        (32 ;; `%uuid_time-low'
         (locally 
             (declare  (unicly::uuid-bit-vector-32 bit-vector)
                       (unicly::uuid-bit-vector-32-length bv-length)
                       (uuid-bit-vector-16-length word-size))
           (the uuid-ub32 (loop-and-return))))

        (16 ;;`%uuid_time-mid', `%uuid_time-high-and-version'
         (locally 
             (declare  (unicly::uuid-bit-vector-16 bit-vector)
                       (unicly::uuid-bit-vector-16-length bv-length)
                       (uuid-bit-vector-8-length word-size))
           (the uuid-ub16 (loop-and-return))))
        (8 ;; `%uuid_clock-seq-and-reserved', `%uuid_clock-seq-low'
         (locally 
             (declare  (unicly::uuid-bit-vector-8 bit-vector)
                       (uuid-bit-vector-8-length bv-length)
                       ((uuid-bit-vector-length 4) word-size))
           (the uuid-ub8 (loop-and-return))))))))

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
;; 
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
;; (defun uuid-bit-vector-16-to-octets (bv)
;; (defun uuid-bit-vector-32-to-octets  (bv)
;; (defun uuid-bit-vector-48-to-octets  (bv)
;; (defun uuid-bit-vector-deposit-octet-to-byte-array (bv)
;; (defun uuid-bit-vector-to-uuid-byte-array  (bv)
;;
;; (uuid-bit-vector-to-uuid (bv)


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
