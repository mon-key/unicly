;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:49:17-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-tests.lisp
;;; ==============================

;; :TODO This entire file needs to be wrapped up with a regression suite.


(in-package #:unicly)
;; *package*


;; `uuid-integer-128-to-byte-array' with v5 uuids

 (let ((alphabet (loop 
                    for x from 97 below 123
                    for y from 65 below 91 
                    nconc (list (code-char x) (code-char y)) into alpha
                    finally (return (make-array 52 :element-type 'character :initial-contents alpha)))))
   (flet ((random-string ()
            (loop
               repeat (random 53)
               for x = (schar alphabet (random 52)) collect x into rand
               finally (return (make-array (length rand) :element-type 'character :initial-contents rand)))))
    
     (loop 
        repeat 1000
        for uuid   = (make-v5-uuid (make-v4-uuid) (random-string))
        for ba     = (uuid-to-byte-array uuid)  
        for int    = (uuid-bit-vector-to-integer (uuid-to-bit-vector uuid))
        for ba-int = (uuid-integer-128-to-byte-array int)
        always (equalp ba-int ba))))

;; `uuid-integer-128-to-byte-array' with v3 uuids

 (let ((alphabet (loop 
                    for x from 97 below 123
                    for y from 65 below 91 
                    nconc (list (code-char x) (code-char y)) into alpha
                    finally (return (make-array 52 :element-type 'character :initial-contents alpha)))))
   (flet ((random-string ()
            (loop
               repeat (random 53)
               for x = (aref alphabet (random 52)) collect x into rand
               finally (return (make-array (length rand) :element-type 'character :initial-contents rand)))))
     (loop 
        repeat 1000
        for uuid   = (make-v3-uuid (make-v4-uuid) (random-string))
        for ba     = (uuid-to-byte-array uuid)  
        for int    = (uuid-bit-vector-to-integer (uuid-to-bit-vector uuid))
        for ba-int = (uuid-integer-128-to-byte-array int)
        always (equalp ba-int ba))))

;; `uuid-integer-128-to-byte-array' with v4 uuids

 (loop 
    repeat 1000
    for uuid   = (make-v4-uuid)
    for ba     = (uuid-to-byte-array uuid)
    for int    = (uuid-bit-vector-to-integer (uuid-to-bit-vector uuid))
    for ba-int = (uuid-integer-128-to-byte-array int)
    always (equalp ba-int ba))

;;; ==============================
;; `uuid-disassemble-ub48'

 (equal (multiple-value-list (uuid-disassemble-ub48 263011693164991)) '(239 53 46 138 249 191))

 (let ((gthr '()))
   (dolist (i (list *uuid-namespace-dns* *uuid-namespace-oid*
                    *uuid-namespace-url* *uuid-namespace-x500*) 
            (equalp gthr
                    '((0 192 79 212 48 200)
                      (0 192 79 212 48 200)
                      (0 192 79 212 48 200)
                      (0 192 79 212 48 200))))
     (let ((v4 (slot-value i '%UUID_NODE)))
       (push (multiple-value-list (uuid-disassemble-ub48 v4))
             gthr))))

;;; ==============================
;; `uuid-disassemble-ub32'

 (equal (multiple-value-list (uuid-disassemble-ub32 1686856094)) '(100 139 97 158))

 (equal (multiple-value-list (uuid-disassemble-ub32 
                              (slot-value (make-v5-uuid *uuid-namespace-dns* "bubba") '%UUID_TIME-MID)))
        '(0 0 54 129))

 (let ((gthr '()))
   (dolist (i (list *uuid-namespace-dns* *uuid-namespace-oid*
                    *uuid-namespace-url* *uuid-namespace-x500*) 
            (equalp gthr
                    '((0 0 157 173) (0 0 157 173)
                      (0 0 157 173) (0 0 157 173))))
     (let ((utm (slot-value i '%uuid_time-mid)))
       (push (multiple-value-list (uuid-disassemble-ub32 utm))
             gthr))))
;;
;;; ==============================
;; `uuid-disassemble-ub16'

 (equal (multiple-value-list (uuid-disassemble-ub16 13953)) '(54 129))

 (equal (multiple-value-list 
         (uuid-disassemble-ub16 (slot-value (make-v5-uuid *uuid-namespace-dns* "bubba") '%UUID_TIME-MID)))
        '(54 129))

 (eq (nth-value 0 (uuid-disassemble-ub16 13953)) 54)

 (let ((gthr '()))
   (dolist (i (list *uuid-namespace-dns* *uuid-namespace-oid*
                    *uuid-namespace-url* *uuid-namespace-x500*) 
             (equalp gthr
                     '((157 173) (157 173)
                       (157 173) (157 173))))
     (let ((utm-16 (slot-value i '%uuid_time-mid)))
       (push (multiple-value-list (uuid-disassemble-ub16 utm-16))     
             gthr))))

;;; ==============================
;; `uuid-get-namespace-bytes'

 (eq (length (uuid-get-namespace-bytes *uuid-namespace-dns*)) 16)

 (equalp 
  (uuid-get-namespace-bytes (make-v5-uuid *uuid-namespace-dns* "bubba"))
  #(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199))


;;; ==============================
;; `uuid-byte-array-to-bit-vector'

 (equal (uuid-byte-array-to-bit-vector (uuid-get-namespace-bytes (make-v5-uuid *uuid-namespace-dns* "bubba")))
        #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111)

 (equal
  (uuid-byte-array-to-bit-vector (uuid-get-namespace-bytes (make-v5-uuid *uuid-namespace-dns* "bubba")))
  (uuid-byte-array-to-bit-vector
   (make-array 16 
               :element-type 'uuid-ub8 
               :initial-contents #(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199))))

;;; ==============================
;; `uuid-to-bit-vector'
 
 (equal (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba"))
        #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111)

;;; ==============================
;; `uuid-bit-vector-eql'

 (let ((bubba-uuid (make-v5-uuid *uuid-namespace-dns* "bubba")))
   (uuid-bit-vector-eql 
    (uuid-to-bit-vector bubba-uuid)
    (uuid-byte-array-to-bit-vector (uuid-get-namespace-bytes bubba-uuid))))

 (uuid-bit-vector-eql 
  (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ"))
  #*11001001011000010001100110100011000000000000000001010011101101001011101000111000101101001001000101110001101010000111110110001001)

;;; ==============================
;; `uuid-digest-uuid-instance' for sha1 and md5 

(eq (length (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")) 20)

(eq (length (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba")) 16)

(equalp (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")
        #(238 161 16 94 54 129 17 23 153 182 123 43 95 225 243 199 6 226 80 144))

(equalp (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba")
        #(94 50 8 56 113 87 80 57 67 131 101 45 150 112 90 125))

;;; ==============================
;; compare `uuid-digest-uuid-instance' with combined output of 
;; `uuid-digest-uuid-string'/`uuid-get-bytes'/`uuid-print-bytes'
;; for both sha1 and md5 

 (equalp 
  (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")
  #(238 161 16 94 54 129 17 23 153 182 123 43 95 225 243 199 6 226 80 144))

 (equalp 
  (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba")
  #(94 50 8 56 113 87 80 57 67 131 101 45 150 112 90 125))

;;; ==============================
;; `uuid-request-integer'

 (eq (uuid-request-integer (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba") 10 6) 135426222453703)

 (eq (%uuid_node-request (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")) 135426222453703)

 (eql (uuid-request-integer (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba") 10 6)
      (%uuid_node-request (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")))

;;; ==============================
;; `*random-state-uuid*'
 (random-state-p  *random-state-uuid*)

;;; ==============================
;; test `uuid-request-integer', `%uuid_time-low-request',
;; `%uuid_time-mid-request', `%uuid_time-high-and-version-request',
;; `%uuid_clock-seq-and-reserved-request', `%uuid_clock-seq-low-request',
;; `%uuid_node-request' with both v3 md5 and sha1 byte arrays as returned from
;; `uuid-digest-uuid-instance'

 (let ((digest-5 (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba"))
       (digest-3 (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba")))
   (equal 
    (list 
     ;; `%uuid_time-low-request'
     (list                              ; v5
      (%uuid_time-low-request digest-5)
      (uuid-request-integer digest-5 0 4)
      ;; (uuid-load-bytes digest-5 :start 0 :end 3)
      )
     (list                              ; v3
      (%uuid_time-low-request digest-3)
      (uuid-request-integer digest-3 0 4)
      ;;(uuid-load-bytes digest-3 :start 0 :end 3)
      )
     ;; `%uuid_time-mid-request'
     (list                              ; v5
      (%uuid_time-mid-request digest-5)
      (uuid-request-integer digest-5 4 2)
      ;;(uuid-load-bytes digest-5 :start 4 :end 5)
      )
     (list                              ; v3   
      (%uuid_time-mid-request digest-3)
      (uuid-request-integer digest-3 4 2)
      ;; (uuid-load-bytes digest-3 :start 4 :end 5)
      )
     ;; `%uuid_time-high-and-version-request'
     (list                              ; v5
      (%uuid_time-high-and-version-request digest-5 5)
      ;;(dpb #b0101 (byte 4 12) (uuid-load-bytes digest-5 :start 6 :end 7))
      ;;(dpb #b0101 (byte 4 12) (uuid-load-bytes digest-5 :start 6 :end 7))
      )
     (list                              ; v3
      (%uuid_time-high-and-version-request digest-3 3)
      ;; (dpb #b0011 (byte 4 12) (uuid-load-bytes digest-3 :start 6 :end 7))
      ;; (dpb #b0011 (byte 4 12) (uuid-load-bytes digest-3 :start 6 :end 7))
      )
     ;; `%uuid_clock-seq-and-reserved-request'
     (list                              ; v5 
      (%uuid_clock-seq-and-reserved-request digest-5)
      (dpb #b0010 (byte 2 6) (aref digest-5 8)))
     (list                              ; v3
      (%uuid_clock-seq-and-reserved-request digest-3)
      (dpb #b0010 (byte 2 6) (aref digest-3 8)))
     ;; `%uuid_clock-seq-low-request'
     (list                              ; v5
      (%uuid_clock-seq-low-request digest-5)
      (aref digest-5 9))
     (list                              ; v3
      (%uuid_clock-seq-low-request digest-3)
      (aref digest-3 9))
     ;; `%uuid_node-request'
     (list                              ; v5 
      (%uuid_node-request digest-5)
      (uuid-request-integer digest-5 10 6)
      ;; (uuid-load-bytes digest-5 :start 10 :end 15)
      )
     (list                              ; v3
      (%uuid_node-request digest-3)
      (uuid-request-integer digest-3 10 6)
      ;; (uuid-load-bytes digest-3 :start 10 :end 15)
      ))
    '((4003532894 4003532894) (1580337208 1580337208) (13953 13953) (29015 29015)
      (20759) (12345) (153 153) (131 131) (182 182) (131 131)
      (135426222453703 135426222453703) (111246471879293 111246471879293)))) 

;;; ==============================
;; Whether return value of `make-v5-uuid' and `format-v5-uuid' 
;; have `cl:equal' bit-vector representation
;; have `cl:equalp' byte-array representation
;; are uuid-eql (essentially a test for `uuid-bit-vector-eql')

 (let ((cmp-A (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (cmp-b (digested-v5-uuid (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba"))))
   (and 
    (equal (uuid-to-bit-vector cmp-a)
           (uuid-to-bit-vector cmp-b))   
    (equalp
     (uuid-get-namespace-bytes cmp-a)
     (uuid-get-namespace-bytes cmp-b))
    (uuid-eql cmp-a cmp-b)))

;;; ==============================
;; Same as above but for return value of `make-v3-uuid' and `format-v3-uuid'

 (let ((cmp-A (make-v3-uuid *uuid-namespace-dns* "bubba"))
       (cmp-b (digested-v3-uuid
               (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba"))))
   (and 
    (equal (uuid-to-bit-vector cmp-a)
           (uuid-to-bit-vector cmp-b))   
    (equalp
     (uuid-get-namespace-bytes  cmp-a)
     (uuid-get-namespace-bytes  cmp-b))
    (uuid-eql cmp-a cmp-b)))

;;; ==============================
;; :NOTE Hopefully following three tests around `%verify-version-3-or-5',
;; `%verify-digest-version', `uuid-digest-uuid-instance'and will catch stupid
;; mistakes like that which occured with `%verify-digest-version' after the a
;; change where we inadverdently moved to testing 
;;  (logbitp <VERSION> 0) instead of (logbitp 1 <VERSION>)
;; This has was corrected with the 2011-09-01.
;;
;; `%verify-version-3-or-5'

 (let ((v3 3) 
       (v5 5) 
       (v-invalid (list -1 0 1 2 4 6)))
   (and (eq v3 (%verify-version-3-or-5 v3))
        (eq v5 (%verify-version-3-or-5 v5))
        (every #'null 
               (mapcar #'(lambda (non-version) 
                           (ignore-errors (%verify-version-3-or-5 non-version)))
                       v-invalid))))

;;; ==============================
;; `%verify-digest-version'

 (let ((v3      3)
       (v5      5)
       (v-invalid (list -1 0 1 2 4 6)))
   (and (eq   (%verify-digest-version v3) :MD5)
        (eq   (%verify-digest-version v5) :SHA1)
        (every #'null 
               (mapcar #'(lambda (non-version) 
                           (ignore-errors (%verify-digest-version non-version)))
                       v-invalid))))

;;; ==============================
;; `uuid-digest-uuid-instance'

 (let ((v3-digest   (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba"))
       (v5-digest   (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba")))
   (and 
    (eq (length v3-digest) 16) 
    (eq (length v5-digest) 20)))

;;; ==============================
;; `format-v3or5-uuid'

 (let ((v5-digest   (uuid-digest-uuid-instance 5 *uuid-namespace-dns* "bubba"))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-digest   (uuid-digest-uuid-instance 3 *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and 
    (eq (length v3-digest) 16) 
    (eq (length v5-digest) 20)
    (uuid-eql (digested-v3or5-uuid v5-digest 5) v5-instance)
    (uuid-eql (digested-v3or5-uuid v3-digest 3) v3-instance))
    (not (uuid-eql v3-instance v5-instance)))

;;; ==============================
;; `make-v4-uuid'

 (let ((v4-instance-a (make-v4-uuid))
       (v4-instance-b (make-v4-uuid)))
   (and (unique-universal-identifier-p v4-instance-b)
        (unique-universal-identifier-p v4-instance-a)
        (not (uuid-eql v4-instance-a v4-instance-b))))

;;; ==============================
;; `uuid-princ-to-string'/`uuid-print-bytes'/`cl:sxhash'

 (let ((v4-instance (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and
    (string= (remove #\- (uuid-princ-to-string v5-instance))
             (uuid-print-bytes nil v5-instance))
    (string= (remove #\- (uuid-princ-to-string v3-instance))
             (uuid-print-bytes nil v3-instance))
    (string= (remove #\- (uuid-princ-to-string v4-instance))
             (uuid-print-bytes nil v4-instance))
    (eq (sxhash (remove #\- (uuid-princ-to-string v5-instance)))
        (sxhash (uuid-print-bytes nil v5-instance)))
    (eq (sxhash (remove #\- (uuid-princ-to-string v3-instance)))
        (sxhash (uuid-print-bytes nil v3-instance)))
    (eq (sxhash (remove #\- (uuid-princ-to-string v4-instance)))
        (sxhash (uuid-print-bytes nil v4-instance)))
    (not (eq (sxhash (uuid-princ-to-string v5-instance))
             (sxhash (uuid-print-bytes nil v5-instance))))
    (not (eq (sxhash (uuid-princ-to-string v3-instance))
             (sxhash (uuid-print-bytes nil v3-instance))))
    (not (eq (sxhash (uuid-princ-to-string v4-instance))
             (sxhash (uuid-print-bytes nil v4-instance))))))

;;; ==============================
;; `uuid-hex-string-32-p'

 (and (uuid-hex-string-32-p (uuid-print-bytes nil (make-v4-uuid)))
      (uuid-hex-string-32-p (remove #\- (uuid-print-bytes nil (make-v4-uuid)))))

;;; ==============================
;; `uuid-hex-string-36-p'

 (and (uuid-hex-string-36-p (uuid-princ-to-string  (make-v4-uuid)))
      (uuid-hex-string-36-p (uuid-princ-to-string  (make-v5-uuid *uuid-namespace-dns* "bubba")))
      (uuid-hex-string-36-p (uuid-princ-to-string  (make-v3-uuid *uuid-namespace-dns* "bubba"))))

;;; ==============================
;; `make-uuid-from-string'

 (let ((v4-instance (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and (uuid-eql v5-instance (make-uuid-from-string (uuid-princ-to-string v5-instance)))
        (uuid-eql v3-instance (make-uuid-from-string (uuid-princ-to-string v3-instance)))
        (uuid-eql v4-instance (make-uuid-from-string (uuid-princ-to-string v4-instance)))))

;;; ==============================
;; `%verify-slot-boundp-and-type' gather condition values with handled conditions
;; `uuid-slot-unbound-error' and `uuid-slot-type-error'. Uses `uuid-copy-uuid' to ensure test returns
;; nth-value 0 is t for a passed test
;; nth-value 1 is plist where the property indicators are the symbols
;; UUID-SLOT-UNBOUND-ERROR and UUID-SLOT-TYPE-ERROR the values of which are
;; retrievable with `getf' e.g.:
;;  (getf (nth-value 1 <VALUES>) 'uuid-slot-type-error)
;;  (getf (nth-value 1 <VALUES>) 'uuid-slot-unbound-error)

 (let* ((v4-instance     (make-v4-uuid))
        (v5-instance     (make-v5-uuid *uuid-namespace-dns* "bubba"))
        (v3-instance     (make-v3-uuid *uuid-namespace-dns* "bubba"))
        (objs            (list v4-instance v5-instance v3-instance))
        (caught-unbound  '())
        (caught-bad-type '()))
   (loop 
      for x in objs 
      for y in (list '%uuid_time-low '%uuid_time-mid '%uuid_time-high-and-version) 
      for y2 = (uuid-copy-uuid x)
      do (slot-makunbound x y)
      (handler-case  (%verify-slot-boundp-and-type x)
        (uuid-slot-unbound-error (te)
          (push (list :uuid-slot-unbound-name   (uuid-slot-unbound-name   te)
                      :uuid-slot-unbound-object (uuid-slot-unbound-object te)
                      :uuid-slot-bound-object  y2)
                caught-unbound)
          ;; setf the unbound-slot back to value of slot in the copy uuid
          (setf (slot-value x y) (slot-value y2 y))))
      finally (setf caught-unbound (nreverse caught-unbound)))
   (loop
      for o in objs
      for o2 = (uuid-copy-uuid o)
      for p in (list '%uuid_clock-seq-and-reserved '%uuid_clock-seq-low '%uuid_node)  
      for q in (list "%uuid_clock-seq-and-reserved" "%uuid_clock-seq-low" "%uuid_node")
      do (setf (slot-value o p) q)
      (handler-case (%verify-slot-boundp-and-type o) ;verify)
        (uuid-slot-type-error (te)
          (push (list :type-datum (type-error-datum te)
                      :type-expect (type-error-expected-type te))
                caught-bad-type)
          ;; setf the unbound-slot back to value of slot in the copy uuid otherwise test won't return!
          (setf (slot-value o p) (slot-value o2 p)))))

               (values 
                (and (eq (length caught-unbound) 3)
                     (eq (length caught-bad-type) 3))
                `(uuid-slot-unbound-error ,caught-unbound uuid-slot-type-error  ,caught-bad-type)))

;;; ==============================
;; `uuid-copy-uuid'/`cl:equalp'/`cl:equal'/`eql'/`cl:eq'
 (let ((v4-instance (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and 
    ;; v5
    (not (equalp (uuid-copy-uuid v5-instance) v5-instance))
    (not (equal  (uuid-copy-uuid v5-instance) v5-instance))
    (not (eql    (uuid-copy-uuid v5-instance) v5-instance))
    (not (eq     (uuid-copy-uuid v5-instance) v5-instance))
    ;; v3
    (not (equalp (uuid-copy-uuid v3-instance) v3-instance))
    (not (equal  (uuid-copy-uuid v3-instance) v3-instance))
    (not (eql    (uuid-copy-uuid v3-instance) v3-instance))
    (not (eq     (uuid-copy-uuid v3-instance) v3-instance))
    ;; v4
    (not (equalp (uuid-copy-uuid v4-instance) v4-instance))
    (not (equal  (uuid-copy-uuid v4-instance) v4-instance))
    (not (eql    (uuid-copy-uuid v4-instance) v4-instance))
    (not (eq     (uuid-copy-uuid v4-instance) v4-instance))))

;;; ==============================
;; `uuid-eql'/`uuid-copy-uuid'

 (let ((v4-instance-a (make-v4-uuid))
       (v4-instance-b (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and
    (uuid-eql (uuid-copy-uuid v4-instance-a) v4-instance-a)
    (uuid-eql (uuid-copy-uuid v4-instance-b) v4-instance-b)
    (uuid-eql (uuid-copy-uuid v3-instance) v3-instance)
    (uuid-eql (uuid-copy-uuid v5-instance) v5-instance)
    (not (uuid-eql v4-instance-a v4-instance-b))
    (not (uuid-eql v3-instance v5-instance))))

;;; ==============================
;; `uuid-to-bit-vector'/`uuid-bit-vector-128'

 (and (typep (uuid-to-bit-vector (make-v4-uuid))  'uuid-bit-vector-128)
      (typep (uuid-to-bit-vector (make-v3-uuid *uuid-namespace-dns* "bubba")) 'uuid-bit-vector-128)
      (typep (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")) 'uuid-bit-vector-128))

;; `uuid-to-byte-array'/`uuid-get-namespace-bytes'/`uuid-byte-array-16'
 (and ;;(typep (uuid-to-byte-array (make-v4-uuid)) 'uuid-byte-array-16)
  (typep (uuid-get-namespace-bytes (make-v4-uuid)) 'uuid-byte-array-16)
  ;; (typep (uuid-to-byte-array (make-v3-uuid *uuid-namespace-dns* "bubba")) 'uuid-byte-array-16)
  (typep (uuid-get-namespace-bytes (make-v3-uuid *uuid-namespace-dns* "bubba")) 'uuid-byte-array-16)
  ;;(typep (uuid-to-byte-array (make-v5-uuid *uuid-namespace-dns* "bubba")) 'uuid-byte-array-16)
  (typep (uuid-get-namespace-bytes (make-v5-uuid *uuid-namespace-dns* "bubba")) 'uuid-byte-array-16))

;;; ==============================  
;; `unique-universal-identifier-p'

 (let ((v4-instance (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v5-uuid *uuid-namespace-dns* "bubba")))
   (and 
    (unique-universal-identifier-p v5-instance)
    (unique-universal-identifier-p v4-instance)
    (unique-universal-identifier-p v3-instance)))

;;; ==============================
;; `sxhash-uuid'/`uuid-copy-uuid'

 (let ((v4-instance (make-v4-uuid))
       (v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (and (eq (sxhash-uuid v5-instance)
            (sxhash-uuid (uuid-copy-uuid v5-instance)))
        (eq (sxhash-uuid v3-instance)
            (sxhash-uuid (uuid-copy-uuid v3-instance)))
        (eq (sxhash-uuid v4-instance)
            (sxhash-uuid (uuid-copy-uuid v4-instance)))))

;;; ==============================
;; `make-hash-table-uuid'
 (let ((ht (make-hash-table-uuid)))
   (eq (hash-table-test ht) 'uuid-eql))

;; (defparameter *tt--unicly-ht* (make-hash-table-uuid))
 (let* ((ht (make-hash-table-uuid))
        (names (list "C¹=ḱ¶¥Ch9»2YÚḷùìíûḆṇqÑ§`ṓṎÍṂídBUXḁõx"
                     "Lm&rḼḅÍṆḼS.XḗÈṓḲVṁḥḃ¿ðḿÝḲbúṉ(CḾḯTwḋḩ"
                     "ã£y|vḦṇḜ´^ḯÚḩḘḫXḺÆḦíÞṐ;KçḫṀìæÕḶ$ḓNaṁ"
                     ".@ṅ6ïÂṎṋÔḞṛöUÐṆñn§ÔÔḫ5|ÐḱḠḃṃÆ\"YØæPḂ/"
                     "ḚÄ¸ḻ{ṊRWh7äéḟ)ḶÚOvḆḴ|ÇÞæA¤J»Æ*yṎªt}Ý"
                     "_ṆQýë¡Ḱḡ×ḛḿ¯HJṅV×Ï=ÐṄØṘḲXë#õØþqöÅaḇḺ"
                     "^ãḄöþḈ°âḲRḶ2òªḓḧù.ûḅ4ÆḒhṐqḧ÷ṃṒYEḙú>9"
                     "{ḹ]¥ḺḶåḈuÈpïn=cîṋṔḄDg<õ,ṈḓṆ»ÌÒk$ṛyý¸"
                     "Ḳµî,Àc))ḺüRḝ(ṉḻÛàḈ§;Ḥ-Ñ²yṖṗüḄṕõRª7Òþ"))
        (ids-v5 (loop for n in names collect (make-v5-uuid *uuid-namespace-dns* n)))
        (ids-v3 (loop for n in names collect (make-v3-uuid *uuid-namespace-dns* n)))
        ;; (ids-v4 (loop repeat 10 collect (make-v4-uuid)))
        )
   (dolist (i (list ids-v5 ids-v3))
     (dolist (n i)
       (setf (gethash n ht) (uuid-to-bit-vector n))))
   (loop 
      for chk-key in `(,@ids-v3 ,@ids-v5)
      for chk-bv = (uuid-to-bit-vector chk-key)
      for get-key = (gethash chk-key ht)
      when (uuid-bit-vector-eql get-key chk-bv)
      collect (cons chk-key get-key) into rtn 
      finally (return (values (eq (length rtn) 18) rtn))))

;;; ==============================
;; `make-null-uuid'

 (and
  (equal (uuid-to-bit-vector (make-null-uuid))
         (uuid-bit-vector-128-zeroed))
  (equalp (uuid-get-namespace-bytes (make-null-uuid))
          (make-array 16 :element-type 'uuid-ub8 :initial-element 0))
  (string=  (uuid-princ-to-string (make-null-uuid)) "00000000-0000-0000-0000-000000000000")
  (string=  (uuid-print-bytes nil (make-null-uuid)) "00000000000000000000000000000000" ))

;;; ==============================
;; `uuid-as-urn-string'

 (and 
  (string= (uuid-as-urn-string nil (make-null-uuid)) "urn:uuid:00000000-0000-0000-0000-000000000000")
  (not (string= (uuid-as-urn-string nil (make-null-uuid)) "URN:UUID:00000000-0000-0000-0000-000000000000")))

;;; ==============================
;; (uuid-bit-vector-v5-p (uuid-to-bit-vector (make-v4-uuid)))
;; (uuid-version-bit-vector (uuid-to-bit-vector (make-v3-uuid *uuid-namespace-dns* "bubba")))
;; (uuid-version-bit-vector (uuid-to-bit-vector (make-null-uuid)))
;; (uuid-bit-vector-v3-p (uuid-to-bit-vector (make-v3-uuid *uuid-namespace-dns* "bubba")))
;;
;; `uuid-version-bit-vector'/`make-null-uuid'
 (and (eq (uuid-version-bit-vector (uuid-to-bit-vector (make-null-uuid))) 0)
      (eq (nth-value 1 (uuid-version-bit-vector (uuid-to-bit-vector (make-null-uuid)))) 'null-uuid))

;;; ==============================
;; `uuid-version-bit-vector'/`uuid-bit-vector-v3-p'/`uuid-bit-vector-v4-p'/`uuid-bit-vector-v5-p'

 (let ((v3 (make-v3-uuid *uuid-namespace-dns* "bubba"))
       (v5 (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (v4 (make-v4-uuid)))
   (setf v3 (uuid-to-bit-vector v3))
   (setf v4 (uuid-to-bit-vector v4))
   (setf v5 (uuid-to-bit-vector v5))
   (loop 
      with bvs = (list v3 v4 v5)
      for version in bvs collect (uuid-version-bit-vector version) into get-version
      finally (return
                (loop 
                   for chk-preds in get-version 
                   for chk-pred-version in bvs
                   collect (case chk-preds
                             (3 (uuid-bit-vector-v3-p chk-pred-version))
                             (4 (uuid-bit-vector-v4-p chk-pred-version))
                             (5 (uuid-bit-vector-v5-p chk-pred-version))) into rslt
                   finally (return (values (and (equal get-version (list 3 4 5)) 
                                                (equal rslt (list t t t)))
                                           (list get-version rslt)))))))

;;; ==============================
;; `uuid-octet-to-bit-vector-8'
;; v4
;; (let ((v4 (make-v4-uuid)))
;;   (setf v4 (uuid-get-namespace-bytes v4))
;;   (setf v4 (uuid-request-integer v4 6 2))
;;   (setf v4 (list v4 (multiple-value-list (uuid-disassemble-ub16 v4))))
;;   (setf v4 `(,@v4 (,(uuid-octet-to-bit-vector-8 (caadr v4))
;;                     ,(uuid-octet-to-bit-vector-8 (cadadr v4))))))
;;

;; v5
 (let ((v5 (make-v5-uuid *uuid-namespace-dns* "bubba")))
   (setf v5 (uuid-get-namespace-bytes v5))
   (setf v5 (uuid-request-integer v5 6 2))
   (setf v5 (list v5 (multiple-value-list (uuid-disassemble-ub16 v5))))
   (setf v5 `(,@v5 (,(uuid-octet-to-bit-vector-8 (caadr v5))
                     ,(uuid-octet-to-bit-vector-8 (cadadr v5)))))
   (values (equal v5 '(20759 (81 23) (#*01010001 #*00010111)))
           v5))

;; v3
 (let ((v3 (make-v3-uuid *uuid-namespace-dns* "bubba")))
   (setf v3 (uuid-get-namespace-bytes v3))
   (setf v3 (uuid-request-integer v3 6 2))
   (setf v3 (list v3 (multiple-value-list (uuid-disassemble-ub16 v3))))
   (setf v3 `(,@v3 (,(uuid-octet-to-bit-vector-8 (caadr v3))
                     ,(uuid-octet-to-bit-vector-8 (cadadr v3)))))
   (values (equal v3 '(12345 (48 57) (#*00110000 #*00111001)))
           v3))

;;; ==============================
;; `uuid-version-uuid'
 (and (eq (uuid-version-uuid (make-v3-uuid *uuid-namespace-dns* "bubbb")) 3)
      (eq (uuid-version-uuid (make-v4-uuid)) 4)
      (eq (uuid-version-uuid (make-v5-uuid *uuid-namespace-dns* "bubbb")) 5))

;;; ==============================

;; `uuid-serialize-byte-array-bytes', `uuid-deserialize-byte-array-bytes' 
(let* ((file (make-pathname :directory '(:absolute "tmp")
                            :name "temp-bytes"))
       (w-uuid (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (w-uuid-ba (uuid-to-byte-array w-uuid)))
  (unwind-protect   
       (progn
         (with-open-file (s file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'uuid-ub8)
           (uuid-serialize-byte-array-bytes w-uuid-ba s))
    
         ;; :NOTE following is basis for deserializing a uuid from file, e.g.: 
         ;;  (defun deserialize-uuid (file) (...) )
         (with-open-file (s file 
                            :direction :input
                            :if-does-not-exist :error
                            :element-type 'uuid-ub8)
           (let ((ba-read (uuid-deserialize-byte-array-bytes s)))
             (values (equalp ba-read w-uuid-ba)
                     ba-read 
                     w-uuid))))
    (delete-file file)))

;; `uuid-serialize-bit-vector-bits', `uuid-deserialize-bit-vector-bits'
(let* ((file (make-pathname :directory '(:absolute "tmp")
                            :name "temp-bytes"))
       (w-uuid (make-v5-uuid *uuid-namespace-dns* "bubba"))
       (w-uuid-bv (uuid-to-bit-vector w-uuid)))
  (unwind-protect   
       (progn
         (with-open-file (s file
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :direction :output
                            :element-type 'uuid-ub8)
           (uuid-serialize-bit-vector-bits w-uuid-bv s))
         (let ((bv-read ;; (uuid-read-bit-vector-bits file)
                (with-open-file (s file 
                                   :element-type 'uuid-ub8 
                                   :direction :input 
                                   :if-does-not-exist :error)
                  (uuid-deserialize-bit-vector-bits s))))
           (values (uuid-bit-vector-eql bv-read w-uuid-bv) w-uuid-bv w-uuid)))
    (delete-file file)))

;; `uuid-to-bit-vector'
(equal 
 (list 
  (= (uuid-version-uuid (uuid-from-bit-vector (uuid-to-bit-vector (make-v4-uuid)))) 4)
  (= (uuid-version-uuid (uuid-from-bit-vector (uuid-to-bit-vector (make-v3-uuid *uuid-namespace-dns* "bubba")))) 3)
  (= (uuid-version-uuid (uuid-from-bit-vector (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")))) 5)
  (unique-universal-identifier-null-p 
   (uuid-from-bit-vector (uuid-bit-vector-128-zeroed)))
  (and (nth-value 1
                  (ignore-errors
                    (let ((fake-uuid-bv (uuid-bit-vector-128-zeroed)))
                      (setf (sbit fake-uuid-bv 51) 1) ;; v1 
                      (uuid-from-bit-vector fake-uuid-bv))))
       t)
  (and (nth-value 1 (ignore-errors
                      (let ((fake-uuid-bv (uuid-bit-vector-128-zeroed)))
                        (setf (sbit fake-uuid-bv 50) 1) ;; v2
                        (uuid-from-bit-vector fake-uuid-bv))))
       t))
 (list T T T T T T))

;;; ==============================
;; :NOTE Following requires `make-random-string':
;; :SEE :FILE unicly-timings.lisp
;;
;; (loop 
;;    for rnd in (loop 
;;                  repeat 500 
;;                  collect (make-v5-uuid *uuid-namespace-dns* (make-random-string 32)))
;;    for ba-dbc = (uuid-get-namespace-bytes rnd)
;;    for orig   =  (uuid-to-byte-array rnd)
;;    always (equalp ba-dbc  orig))
;;
;;; ==============================


;;; ==============================
;;; UNICLY/ELISP/UUID
;;; ==============================


;;; ==============================
;; :NOTE On #{2011-09-07T19:33:39-04:00Z}#{11363} the following bit-vector tests
;; were known to have passed. If at some point in the future they don't then one
;; of us has a regression.
;;
;; Testing the version of UUIDs from the uuid library:
;;
;; (and 
;;  (= (uuid-version-bit-vector
;;      (uuid-to-bit-vector 
;;       (make-uuid-from-string (format nil "~A" (uuid:make-v1-uuid)))))
;;     1)
;;  (= (uuid-version-bit-vector
;;      (uuid-to-bit-vector 
;;       (make-uuid-from-string (string-downcase (format nil "~A" (uuid:make-v5-uuid uuid:+namespace-dns+ "bubba"))))))
;;     5)
;;  (= (uuid-version-bit-vector
;;      (uuid-to-bit-vector 
;;       (make-uuid-from-string (string-downcase (format nil "~A" (uuid:make-v3-uuid uuid:+namespace-dns+ "bubba"))))))
;;     3)
;;  (= 
;;   (uuid-version-bit-vector 
;;    (uuid-to-bit-vector 
;;     (make-uuid-from-string (string-downcase (format nil "~A" (uuid:make-v4-uuid)))))) 
;;   4))
;;
;;
;; :NOTE On #{2011-09-07T19:33:39-04:00Z}#{11363} the following
;; uuid-version-uuid tests were known to have passed. If at some point in the
;; future they don't then one of us has a regression.
;; Note also that we don't currently test for v1 UUIDS!
;;
;; (and 
;;  (= (unicly:uuid-version-uuid
;;      (unicly:uuid-from-byte-array
;;       (uuid:uuid-to-byte-array 
;;        (uuid:make-v5-uuid uuid:+namespace-dns+ "bubba"))))
;;     5)
;;  (= (unicly:uuid-version-uuid
;;      (unicly:uuid-from-byte-array
;;       (uuid:uuid-to-byte-array 
;;        (uuid:make-v3-uuid uuid:+namespace-dns+ "bubba"))))
;;     3)
;;
;;  (= (unicly:uuid-version-uuid 
;;      (unicly:uuid-from-byte-array 
;;       (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
;;     4))
;;
;; :NOTE Also that we don't currently test for v1 UUIDS!
;;
;; (null 
;;  (unicly:uuid-version-uuid 
;;   (unicly:uuid-from-byte-array 
;;    (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))))
;;
;;; ==============================

;;; ==============================
;; Compare output of unicly:make-v5-uuid uuid:make-v5-uuid and Elisp uuid.el
;; 
;; (make-v5-uuid *uuid-namespace-dns* "bubba") 
;; => eea1105e-3681-5117-99b6-7b2b5fe1f3c7
;;
;; (make-v5-uuid *uuid-namespace-dns* "ésiaλ")
;; => bb7cb880-7d2a-5db6-86ff-afadc974a7b3
;;
;; (uuid:make-v5-uuid uuid:+namespace-dns+ "bubba")
;; => EEA1105E-3681-5117-99B6-7B2B5FE1F3C7
;;
;; (uuid:make-v5-uuid uuid:+namespace-dns+ "ésiaλ")
;; => BB7CB880-7D2A-5DB6-86FF-AFADC974A7B3
;;
;; :NOTE For comparing with result from `uuid-5' of Emacs lisp uuid.el
;;
;; (insert (uuid-5 "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "bubba"))
;;  => eea1105e-3681-5117-99b6-7b2b5fe1f3c7
;; 
;; (insert (uuid-5 "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "ésiaλ"))
;; => bb7cb880-7d2a-5db6-86ff-afadc974a7b3
;;
;;; ==============================
;; :TEST v3
;;
;; (make-v3-uuid *uuid-namespace-dns* "bubba")
;; => 5e320838-7157-3039-8383-652d96705a7d
;;
;; (make-v3-uuid *uuid-namespace-dns* "ésiaλ")
;; => 6ae2d886-f18b-30c8-82a3-47f5b6909f7b
;;
;; (uuid:make-v3-uuid uuid:+namespace-dns+ "bubba")
;; => 5E320838-7157-3039-8383-652D96705A7D
;;
;; (uuid:make-v3-uuid uuid:+namespace-dns+ "ésiaλ")
;; => 6AE2D886-F18B-30C8-82A3-47F5B6909F7B
;;
;; :NOTE For comparing with result from `uuid-3' of Emacs lisp uuid.el
;; (insert (uuid-3 "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "bubba"))
;; => 5e320838-7157-3039-8383-652d96705a7d
;;
;; (insert (uuid-3 "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "ésiaλ"))
;; => 6ae2d886-f18b-30c8-82a3-47f5b6909f7b
;;
;;; ==============================


;;; ==============================
;;; :DEPRECATED-FUN
;;; ==============================

;;; ==============================
;; `uuid-number-to-byte-array'
;;
;; (equalp (multiple-value-list (uuid-number-to-byte-array 825973027016))
;;         '(#(0 192 79 212 48 200) 6))
;;
;; (let ((v5-instance (make-v5-uuid *uuid-namespace-dns* "bubba"))
;;       (v3-instance (make-v3-uuid *uuid-namespace-dns* "bubba")))
;;   (loop 
;;      for obj in (list v3-instance v5-instance)
;;      collect (with-slots ((utl %uuid_time-low)
;;                           (utm %uuid_time-mid utm)
;;                           (uthav %uuid_time-high-and-version uthav)
;;                           (ucsr %uuid_clock-seq-and-reserved)
;;                           (ucsl %uuid_clock-seq-low)
;;                           (un %uuid_node))
;;                  obj
;;                (loop 
;;                   for slot in (list utl utm uthav ucsr ucsl un)
;;                   collect (multiple-value-list (uuid-number-to-byte-array slot)))) into slot-as-ba
;;      finally (return (equalp
;;                       slot-as-ba
;;                       '(( ;; v3 e.g. (uuid-get-namespace-bytes  (make-v3-uuid *uuid-namespace-dns* "bubba"))
;;                          (#(94 50 8 56) 4) (#(113 87) 2) (#(48 57) 2) 
;;                          (#(131) 1) (#(131) 1) (#(101 45 150 112 90 125) 6))
;;                         ( ;; v5 e.g. (uuid-get-namespace-bytes  (make-v5-uuid *uuid-namespace-dns* "bubba"))
;;                          (#(238 161 16 94) 4) (#(54 129) 2) (#(81 23) 2)
;;                          (#(153) 1) (#(182) 1)(#(123 43 95 225 243 199) 6)))))))
;;; ==============================

;;; ==============================
;; `uuid-get-namespace-bytes'/`uuid-to-byte-array'
;;
;; (let ((gthr '()))
;;   (dolist (i (list *uuid-namespace-dns* *uuid-namespace-oid*
;;                    *uuid-namespace-url* *uuid-namespace-x500*)
;;            (cons (loop 
;;                     :for (get-nb . to-ba) in gthr
;;                     :always (equalp get-nb to-ba))
;;                  (equalp '((#(107 167 184 20 157 173 17 209 128 180 0 192 79 212 48 200)
;;                             . #(107 167 184 20 157 173 17 209 128 180 0 192 79 212 48 200))
;;                            (#(107 167 184 17 157 173 17 209 128 180 0 192 79 212 48 200)
;;                             . #(107 167 184 17 157 173 17 209 128 180 0 192 79 212 48 200))
;;                            (#(107 167 184 18 157 173 17 209 128 180 0 192 79 212 48 200)
;;                             . #(107 167 184 18 157 173 17 209 128 180 0 192 79 212 48 200))
;;                            (#(107 167 184 16 157 173 17 209 128 180 0 192 79 212 48 200)
;;                             . #(107 167 184 16 157 173 17 209 128 180 0 192 79 212 48 200)))
;;                          gthr)))
;;     (push (cons (uuid-get-namespace-bytes i)
;;                 (uuid-to-byte-array i))
;;             gthr)))
;;; ==============================

;;; ==============================
;; compare `uuid-get-namespace-bytes' with combined output of 
;; `ironclad:ascii-string-to-byte-array'/`uuid-get-bytes'/`uuid-print-bytes'
;; using v4-uuid as NAMESPACE arg to make-v5-uuid with NAME arg "bubba"
;;
;; (let* ((new-nmspc (make-v4-uuid))
;;        (rtn (list (uuid-get-namespace-bytes (make-v5-uuid new-nmspc "bubba"))
;;                   (ironclad:ascii-string-to-byte-array
;;                    (uuid-get-bytes (uuid-print-bytes nil (make-v5-uuid new-nmspc "bubba")))))))
;;   (values (equalp (car rtn) (cadr rtn)) rtn))
;;
;;
;; (equalp (uuid-get-namespace-bytes (make-v5-uuid *uuid-namespace-dns* "bubba"))
;;         (ironclad:ascii-string-to-byte-array 
;;          (uuid-get-bytes (uuid-print-bytes nil (make-v5-uuid *uuid-namespace-dns* "bubba")))))
;;; ==============================
;;
;;
;; (defparameter *tt--array*
;;   (make-array 16 :element-type 'uuid-ub8 :initial-contents '(;; 004 163 243 087 
;;                                                              255 255 255 255
;;                                                              131 056 113 209
;;                                                              192 078 ;; 199 013 186 219 156 252)))
;;                                                              255 255 255 255 255 255)))
;;
;; (eql (uuid-get-bytes-for-integer 825973027016) 6)
;; (eql (uuid-get-bytes-for-integer #xFFFFFFFFFFFF) 6)
;; (eql (uuid-get-bytes-for-integer #x1FFFFFFF) 4)
;; (eql (uuid-get-bytes-for-integer #xFFFFFFFF) 4)
;; (eql (uuid-get-bytes-for-integer #xFFFFFF) 4)
;; (eql (uuid-get-bytes-for-integer #x1FFFF) 4)
;; fails successfully:
;; (uuid-get-bytes-for-integer 281474976710656)
;; 
;; (let ((rnd-trip-17 #x1FFFF)
;;       (rslt nil))
;;   (setf rslt
;;         (multiple-value-bind (arr len) (uuid-number-to-byte-array rnd-trip-17)
;;           (uuid-request-integer  arr 0 len)))
;;   (setf rslt `(,(eql rnd-trip-17 rslt) ,rslt)))
;;


;;; ==============================

;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF

