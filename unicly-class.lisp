;;; :FILE-CREATED <Timestamp: #{2011-04-11T11:58:12-04:00Z}#{11151} - by MON>
;;; :FILE unicly/unicly-class.lisp
;;; ==============================



(in-package #:unicly)
;; *package*

(defgeneric uuid-print-bit-vector (stream uuid)
  (:documentation 
   #.(format nil 
 "Print the bit-vector representation of UUID in a format suitable to its class to STREAM.~%~@
UUID an object representing an instance of `unique-universal-identifier' class or subclass.~%~@
STREAM is an output-stream.~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%►►►")))

(defgeneric uuid-print-byte-array (stream uuid)
  (:documentation 
   #.(format nil 
 "Print the byte-array representation of UUID in a format suitable to its class to STREAM.~%~@
UUID an object representing an instance of `unique-universal-identifier' class or subclass.~%~@
STREAM is an output-stream.~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%►►►")))

(defgeneric uuid-print-bytes (stream uuid)
  (:documentation 
   #.(format nil 
             "Print the bytes of UUID in a format suitable to its class to STREAM.~%~@
UUID an object representing an instance of `unique-universal-identifier' class or subclass.~%~@
STREAM is an output-stream.~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%►►►")))

(defgeneric uuid-print-bytes-to-string (uuid &optional string)
  (:documentation 
   #.(format nil
 "Print the bytes of UUID in a format suitable to its class to a string.~%~@
When optional arg STRING is non-nil print bytes to STRING.
STRING should satisfy `string-with-fill-pointer-p'.~%~@
:SEE-ALSO `uuid-print-bytes'.~%►►►")))

(defgeneric uuid-princ-to-string (uuid &key)
  (:documentation 
   #.(format nil
             "Return string representation of UUID-INSTANCE as if by `cl:princ-to-string'.~%~@
:SEE-ALSO `uuid-print-bytes-to-string', `uuid-print-bytes'.~%►►►")))


;;; ==============================
(defclass unique-universal-identifier ()
  ((%uuid_time-low
      :initarg :%uuid_time-low
      :type uuid-ub32
      :initform 0
      ;; :accessor %uuid_time-low
      :documentation
      #.(format nil "An integer of type `uuid-ub32'.~%~@
The low field of a UUID record's timestamp.
Referenced as the `time_low` field in RFC4122.~%~@
Occurs in a UUID `uuid-byte-array-16' representation as a 4 octet value at
offset 0 through index 3.~%~@
A digested UUID's byte-array octets are accessible with `%uuid_time-low-request'.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 0 through index 31.~%~@
Occurs as the 0 element in a UUID string delimited by #\\-.~%~@
Its string representation is 8 hexadecimal characters.~%"))
     (%uuid_time-mid
      :initarg :%uuid_time-mid
      :type uuid-ub16
      :initform 0 
      ;; :accessor %uuid_time-mid
      :documentation
      #.(format nil 
                "An integer of type `uuid-ub16'.~%~@
The middle field of a UUID record's timestamp. 
Referenced as the `time_mid` field in RFC4122.~%~@
Occurs in a UUID `uuid-byte-array-16' representation as a 2 octet value at offset
4 through index 5.~%~@
A digested UUID's byte-array octets are accessible with `%uuid_time-mid-request'.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 32 through index 47.~%~@
Occurs as the 1 element in a UUID string delimited by #\\-.~%~@
Its string representation is 4 hexadecimal characters.~%"))
     (%uuid_time-high-and-version
      :initarg :%uuid_time-high-and-version
      :type uuid-ub16
      :initform 0
      ;; :accessor %uuid_time-high
      :documentation
      #.(format nil 
"An integer of type `uuid-ub16'.~%~@
The high field of a UUID record's bit integer timestamp multiplexed with its version.
Referenced as the `time_high_and_version` field in RFC4122.~%~@
Occurs in a UUID `uuid-byte-array-16' representation as a 2 octet value at offset
6 through index 7.~%~@
A digested UUID's byte-array octets are accessible with `%uuid_time-high-and-version-request'.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 48 through index 63.~%~
:NOTE Per RFC4122 Section 4.1.3, bit 48 should always be 0.~%~@
Occurs as the 2 element in a UUID string delimited by #\\-.~%
Its string representation is 4 hexadecimal characters.~%"))
     (%uuid_clock-seq-and-reserved
      :initarg :%uuid_clock-seq-and-reserved
      :type uuid-ub8
      :initform 0
      ;; :accessor %uuid_clock-seq-var
      :documentation
      #.(format nil 
"An integer of type `uuid-ub8'.~%~@
The high field of a UUID record's bit integer clock sequence multiplexed with its variant.
Referenced as the `clock_seq_hi_and_reserved` field of RFC4122.~%~@
Occurs in a UUID uuid-byte-array-16 representation as an octet value at offset 8.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 64 through index 71.~%~@
A digested UUIDs byte-array octets are accessible with `%uuid_clock-seq-and-reserved-request'.~%~@
Occurs in the upper portion of the 3 element in a UUID string delimited by #\\-.~%~@
Its string representation is 2 hexadecimal characters.~%~@
:NOTE As referenced in RFC4122 Section 4.1.1, the bits of this integer carry the
UUID's variant layout/type with the top 3 most significant bits formatted as follows:~%
 Msb0  Msb1  Msb2
    1     0     x  ; where X indicates a \"don't care\" value~%~@
:NOTE UUID v3, v4, v5 instances of this class will have the relevenat portions
of their bit fields returned in the above configuration.~%"))
     (%uuid_clock-seq-low
      :initarg :%uuid_clock-seq-low
      :type uuid-ub8
      :initform 0
      ;; :accessor %uuid_clock-seq-low
      :documentation
      #.(format nil 
                "An integer of type `uuid-ub8'.~%~@
The low field of the UUID record's bit integer clock sequence. 
Referenced as the `clock_seq_low` field of RFC4122.~%~@
Occurs in a UUID `uuid-byte-array-16' representation as an octet value at offset 9.~%~@
A digested UUIDs byte-array octets are accessible with `%uuid_clock-seq-low-request'.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 72 through index 79.~%~@
Occurs in the lower portion of the 3 element in a UUID string delimited by #\\-.~%~@
Its string representation is 2 hexadecimal characters.~%"))
     (%uuid_node
      :initarg :%uuid_node
      :type uuid-ub48
      :initform 0
      ;; :accessor %uuid_node
      :documentation 
      #.(format nil 
"An integer of type `uuid-ub48'.~%~@
The \"spatially unique\" identifier portion of a UUID record's bit integer.
Referenced as the `node` field of RFC4122.~%~@
Occurs in a UUID uuid-byte-array-16 representation as a 6 octet value at offset 10 through index 15.
A digested UUID's byte-array octets are accessible with `%uuid_node-request'.
:NOTE a digest v5 UUID is initally an object of type `uuid-byte-array-20' with offest 16-19 truncated.~%~@
Occurs in a UUID `uuid-bit-vector-128' representation from offset 79 through 127.~%~@
Occurs as the final or 4 element in a UUID string delimited by #\\-.~%~@
Its string representation is 12 hexadecimal characters.~%")))
  (:documentation
  #.(format nil "Representation of an UUID.~%~@
                    The string representation of A UUID has the format:~%~%~4T
                                         clock-seq-and-reserved~4T
                               time-mid  | clock-seq-low~4T
                               |         | |~4T
                      6ba7b810-9dad-11d1-80b4-00c04fd430c8~4T
                      |             |         |~4T
                      ` time-low    |         ` node~4T
                                    ` time-high-and-version~%~
Each field is treated as an integer and has its value printed as a zero-filled~%~
hexadecimal digit string with the most significant digit first.~%~%~4T~
0                   1                   2                   3    ~%~5T~
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 ~%~4T~
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+~%~4T~
|                        %uuid_time-low                         |~%~4T~
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+~%~4T~
|       %uuid_time-mid          |  %uuid_time-high-and-version  |~%~4T~
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+~%~4T~
|clk-seq-hi-res | clock-seq-low |         %uuid_node (0-1)      |~%~4T~
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+~%~4T~
|                         %uuid_node (2-5)                      |~%~4T~
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+~%~@
Following table enumates the slot/type/value correspondence for instances of this class:~%
  SLOT                          TYPE        BYTE-ARRAY                      SIZE
 `%uuid_time-low'               `uuid-ub32' #(<BYTE> <BYTE> <BYTE> <BYTE>), 4
 `%uuid_time-mid'               `uuid-ub16' #(<BYTE> <BYTE>), 2
 `%uuid_time-high-and-version'  `uuid-ub16' #(<BYTE> <BYTE>), 2
 `%uuid_clock-seq-and-reserved' `uuid-ub8'  #(<BYTE>), 1
 `%uuid_clock-seq-low'          `uuid-ub8'  #(<BYTE>), 1  
 `%uuid_node'                   `uuid-ub48' #(<BYTE> <BYTE> <BYTE> <BYTE> <BYTE> <BYTE>), 6~%~@
While RFC4122 specifically indicates that certain bits of a UUID shall not be
set and/or are reserved for future use, were the full range of its 128 bit
integer representation used it could have an upper bounds with the decimal
representation: 340282366920938463463374607431768211455 e.g.:~%
 \(integer-length 340282366920938463463374607431768211455\)~% => 128~%~@
In long-form this number is:~%
 340,282,366,920,938,463,463,374,607,431,768,211,455 340 undecillion, 282~%~
 decillion, 366 nonillion, 920 octillion, 938 septillion, 463 sextillion, 463~%~
 quintillion, 374 quadrillion, 607 trillion, 431 billion, 768 million, 211~%~
 thousand and 455~%~@
:SEE-ALSO `<XREF>'.►►►~%~%")))


;;; ==============================
;;
;; ,---- RFC4122 Section 3. Subsection "Rules for Lexical Equivalence":
;; | Consider each field of the UUID to be an unsigned integer as shown
;; | in the table in section Section 4.1.2.  Then, to compare a pair of
;; | UUIDs, arithmetically compare the corresponding fields from each
;; | UUID in order of significance and according to their data type.
;; | Two UUIDs are equal if and only if all the corresponding fields
;; | are equal.
;; | 
;; | As an implementation note, equality comparison can be performed on
;; | many systems by doing the appropriate byte-order canonicalization,
;; | and then treating the two UUIDs as 128-bit unsigned integers.
;; | 
;; | UUIDs, as defined in this document, can also be ordered
;; | lexicographically.  For a pair of UUIDs, the first one follows the
;; | second if the most significant field in which the UUIDs differ is
;; | greater for the first UUID.  The second precedes the first if the
;; | most significant field in which the UUIDs differ is greater for
;; | the second UUID.
;; `----

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
  (if (and (= (count 0 uuid-bv-a) (count 0 uuid-bv-b))
           (= (count 1 uuid-bv-a) (count 1 uuid-bv-b)))
      (loop 
         for low-idx from 0 below 64
         for top-idx = (logxor low-idx 127)
         always (and (= (sbit uuid-bv-a low-idx) (sbit uuid-bv-b low-idx))
                     (= (sbit uuid-bv-a top-idx) (sbit uuid-bv-b top-idx)))))
  #+sbcl 
  (SB-INT:BIT-VECTOR-= uuid-bv-a uuid-bv-b))

;;; ==============================
;; :TODO Find the best test for this and incorporate with generic fun `uuid-eql'
;; (defun uuid-byte-array-eql (uuid-ba-a uuid-ba-b)
;;   (declare (uuid-byte-array-16 uuid-ba-a uuid-ba-b)
;;            (optimize (speed 3)))
;;   (equalp uuid-ba-a uuid-ba-b))
;;; ==============================

;;; ==============================
;; :SOURCE kyoto-persistence/uuid.lisp :AUTHOR KevinRaison@chatsubo.net ???
;; :SEE (URL `git://github.com/kraison/kyoto-persistence.git') 
;; :NOTE The original tested `equalp' on the byte-arrray representation of
;; UUID-A and UUID-B e.g.:
;;  (equalp (uuid-get-namespace-bytes uuid-a) (uuid-get-namespace-bytes uuid-b))
;; By converting both ARGS to their bit-vector representation we can test `equal'.
(defgeneric uuid-eql (uuid-a uuid-b)
  (:method ((uuid-a unique-universal-identifier) (uuid-b unique-universal-identifier))
    (uuid-bit-vector-eql (uuid-to-bit-vector uuid-a) (uuid-to-bit-vector uuid-b)))
  (:method ((uuid-a unique-universal-identifier) uuid-b)
    nil)
  (:method (uuid-a (uuid-b unique-universal-identifier))
    nil)
  (:method ((uuid-a t) (uuid-b t))
    nil)
  (:documentation 
   #.(format nil 
             "Whether object UUID-A is eql UUID-B.~%~@
:EXAMPLE~%
 \(uuid-eql \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
           \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)~%
 \(uuid-eql \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
           \(make-v5-uuid *uuid-namespace-dns* \"bubbA\"\)\)~%
 \(uuid-eql \"bubb\" \"bobby\"\)~%~@
:SEE-ALSO `unique-universal-identifier-p'.~%►►►")))

;;; ==============================
;;; :NOTE using generic fun instead.
;; (defun unique-universal-identifier-p (maybe-uuid-instance)
;;   (typep maybe-uuid-instance 'unique-universal-identifier))
(defgeneric unique-universal-identifier-p (object)
  (:method ((object unique-universal-identifier)) t)
  (:method (object) nil)
  (:documentation
   #.(format nil
             "Whether MAYBE-UUID-INSTANCE is a unique-universal-identifier.~%~@
Return T if argument is a class instance of `unique-universal-identifier' or
one of its subclasses.~%~@
:EXAMPLE~%
 \(unique-universal-identifier-p *uuid-namespace-dns*\)~%
 \(unique-universal-identifier-p t\)~%~@
:SEE-ALSO `uuid-eql'.~%►►►")))

;;; ==============================
;; We can speed up make-v5-uuid/make-v3-uuid if an objects is routinely used as
;; namespace arg to and UUID-DIGEST-UUID-INSTANCE is given access to a cached
;; slot value of that objects byte-array representation using as yet an as yet
;; unspecified slot(s) in the class UNICLY:UNIQUE-UNIVERSAL-IDENTIFIER.
;; We should also populate the UUIDs bit-vector representation while were at it :)
;;
;; (defmethod initialize-instance :after ((unique-universal-identifier unique-universal-identifier))
;;   ;; Populate UUID bit-vector/byte-array cache after instance initialization.
;;   (setf (slot-value unique-universal-identifier '%uuid_byte-array)
;;         (uuid-get-namespace-bytes unique-universal-identifier))
;;   (setf (slot-value unique-universal-identifier '%uuid_bit-vector) 
;;         (uuid-to-bit-vector unique-universal-identifier)))
;;
;;; ==============================


;;; ==============================
;;; :UUID-PRINTERS
;;; ==============================

(defun %verify-slot-boundp-and-type (verify-uuid)
  (declare (unique-universal-identifier verify-uuid)
           (optimize speed))
  (with-slots ((utl   %uuid_time-low)
               (utm   %uuid_time-mid)
               (uthav %uuid_time-high-and-version)  
               (ucsar %uuid_clock-seq-and-reserved)
               (ucsl  %uuid_clock-seq-low)
               (un    %uuid_node))
      verify-uuid
    (loop
       for (chk-bnd . chk-type) in '((%uuid_time-low . uuid-ub32)
                                     (%uuid_time-mid . uuid-ub16)
                                     (%uuid_time-high-and-version  . uuid-ub16)
                                     (%uuid_clock-seq-and-reserved . uuid-ub8)
                                     (%uuid_clock-seq-low . uuid-ub8)
                                     (%uuid_node . uuid-ub48))
       unless (slot-boundp verify-uuid chk-bnd)
       do (error 'simple-error
                 :format-control "Arg VERIFY-UUID has unbound slot: ~S"
                 :format-arguments (list chk-bnd))
       unless (typep (slot-value verify-uuid chk-bnd) chk-type)
       do (error (make-condition 'type-error
                                 :datum (slot-value verify-uuid chk-bnd)
                                 :expected-type chk-type))
       finally (return t))))

;; :NOTE Following is likely a violation of the spec as we are printing ID
;; without consideration to a complete conformant implementation of `cl:print-object'
;; Specifically w/r/t to the printer control variables: 
;;  `*print-readably*', `*print-escape*', `*print-pretty*', `*print-level'
;;  `*print-base*', `*print-radix*',  `*print-case*', `*print-array*'
;; 
;; ,----
;; | Methods on 'print-object' are responsible for implementing their part
;; | of the semantics of the printer control variables, 
;; |   {... discussion of printer variables elided ...}
;; | If these rules are not obeyed, the results are undefined.
;; |
;; `---- :SEE (info "(ansicl)print-object")
;;
;; According to the spec *print-level* and *print-length* do not need to be
;; implemented here b/c:
;; 
;; ,----
;; | '*print-level*' and '*print-length*' affect the printing of an any
;; |  object printed with a list-like syntax.  They do not affect the
;; |  printing of symbols, strings, and bit vectors.
;; `---- :SEE (info "(ansicl)*print-level*")
;;
;; *print-case* is relevant to symbols and we explicitly downcase the chars on output
;; *print-length* *print-level* not relevant strings/bit-vectors
;; *print-array* not applicable for string-output
;; 
;; *print-readably* ??? This isnt particulularly readable:
;;  (let ((*print-readably* t))
;;    (print-object (make-v4-uuid) t))
;; 
(defmethod print-object ((id unique-universal-identifier) stream)
  ;;  "Print UNIQUE-UNIVERSAL-IDENTIFIER ID to to STREAM in string representation.
  ;;  :EXAMPLE (print-object (make-v4-uuid) nil)"  
  (declare (type STREAM-OR-BOOLEAN-OR-STRING-WITH-FILL-POINTER stream))
  ;; (%verify-slot-boundp-and-type uuid)
  (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
               %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
      id
    (declare (type uuid-ub32 %uuid_time-low)
             (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
             (type uuid-ub8 %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
             (type uuid-ub48 %uuid_node))
    ;; :NOTE RFC4122 Section 3. "Namespace Registration Template"
    ;; ,----
    ;; | The hexadecimal values "a" through "f" are output as
    ;; | lower case characters and are case insensitive on input.
    ;; `----
    ;; IOW, case is significant on output.
    (format stream "~(~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X~)" 
            %uuid_time-low %uuid_time-mid %uuid_time-high-and-version 
            %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)))
  
(defmethod uuid-print-bytes (stream (uuid unique-universal-identifier))
  #.(format nil
"Print the raw bytes of UUID in hexadecimal format to STREAM.~%~@
UUID is an instance of `unique-universal-identifier' class.~%~@
STREAM is an output-stream.~%~@
Output of return value has the format:~%
 6ba7b8109dad11d180b400c04fd430c8~%~@
:EXAMPLE~%
 \(uuid-print-bytes nil \(make-v4-uuid\)\)~%
 \(uuid-print-bytes t \(make-v4-uuid\)\)~%~@
:NOTE Per RFC4122 Section 3. \"Namespace Registration Template\" 
 ,----
 | The hexadecimal values \"a\" through \"f\" are output as
 | lower case characters and are case insensitive on input.
 `----~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%►►►")
  (declare (type STREAM-OR-BOOLEAN-OR-STRING-WITH-FILL-POINTER stream)
           (optimize (speed 3)))
  ;; Should we (declare (ignore uuid)) this?
  ;; (%verify-slot-boundp-and-type uuid)
  (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
               %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
      uuid
    (declare (type uuid-ub32 %uuid_time-low)
             (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
             (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
             (type uuid-ub48 %uuid_node))
    ;; :NOTE RFC4122 Section 3. "Namespace Registration Template"
    ;; Case is significant on output.
    (format stream "~(~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X~)" 
            %uuid_time-low %uuid_time-mid %uuid_time-high-and-version
            %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)))

(defmethod uuid-print-bytes-to-string ((uuid unique-universal-identifier) &optional string)
  #.(format nil
            "Print the bytes of UUID to a string or STRING.~%~@
When optional arg STRING is non-nil print bytes to STRING. 
STRING should satisfy `string-with-fill-pointer-p'.~%~@
Default method speciaclized on instances of class `unique-universal-identifier'.
:EXAMPLE~%
 \(uuid-print-bytes-to-string \(make-v4-uuid\)\)~%
 \(let \(\(fp-string \(make-array 11 
                              :element-type 'base-char 
                              :fill-pointer 11 
                              :initial-contents \"UUID-BYTES:\"\)\)\)
   \(format fp-string \"~~%\"\)
   \(uuid-print-bytes-to-string \(make-v4-uuid\) fp-string\)\)~%~@
:NOTE Per RFC4122 Section 3. \"Namespace Registration Template\" 
 ,----
 | The hexadecimal values \"a\" through \"f\" are output as
 | lower case characters and are case insensitive on input.
 `----~%~@
:SEE-ALSO `uuid-print-bytes'.~%►►►")
  (declare ((or null STRING-WITH-FILL-POINTER) string))
  #-sbcl (assert (STRING-WITH-FILL-POINTER-P "string")
                 () 
                 "arg STRING not of type `string-with-fill-pointer'")
  (let ((fp-strm (if (null string) 
                     (make-array 32 :element-type 'base-char :fill-pointer 0)
                     string)))
    (with-output-to-string (os fp-strm)
      (uuid-print-bytes os uuid))
    fp-strm))

(defmethod uuid-princ-to-string ((uuid unique-universal-identifier) &key)
  (princ-to-string uuid))

(defmethod uuid-print-bit-vector (stream (uuid simple-bit-vector))
  #.(format nil
            "Print the bit-vector reprsentation of UUID to STREAM.~%~@
UUID should be an object of type `uuid-bit-vector-128', sigal an error if not.~%~@
:EXAMPLE~%
 \(uuid-print-bit-vector nil (uuid-bit-vector-zeroed))~%
 \(find-method #'uuid-print-bit-vector nil '\(t simple-bit-vector\)\)~%~@
:SEE-ALSO `uuid-to-bit-vector'.~%►►►~%")
  (declare (uuid-bit-vector-128 uuid))
  #-sbcl (etypecase bv2 (uuid-bit-vector-128 t))
  (with-standard-io-syntax (write uuid :stream stream)))

(defmethod uuid-print-bit-vector (stream (uuid unique-universal-identifier))
  #.(format nil
            "Print the `uuid-bit-vector-128' representation UUID to STREAM.~%~@
UUID is an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(uuid-print-bit-vector nil \(make-v4-uuid\)\)~%
 \(find-method #'uuid-print-bit-vector nil '\(t unique-universal-identifier\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►~%")
  (let ((id-to-bv (uuid-to-bit-vector uuid)))
    (declare (uuid-bit-vector-128 uuid))
    ;; (with-standard-io-syntax (write id-to-bv :stream stream))))
    (uuid-print-bit-vector stream id-to-bv)))

(defun uuid-copy-uuid (uuid-instance)
  (declare (unique-universal-identifier uuid-instance)
           (optimize (speed 3)))
  #-sbcl (assert (unique-universal-identifier-p uuid-instance)
                 (uuid-instance)
                 "Arg UUID-INSTANCE does not satisfy `unique-universal-identifier-p'")
  (%verify-slot-boundp-and-type uuid-instance)
  (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
               %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
      uuid-instance
    (declare (type uuid-ub32 %uuid_time-low)
             (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
             (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
             (type uuid-ub48 %uuid_node))
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low                %uuid_time-low
                   :%uuid_time-mid                %uuid_time-mid
                   :%uuid_time-high-and-version   %uuid_time-high-and-version
                   :%uuid_clock-seq-and-reserved  %uuid_clock-seq-and-reserved
                   :%uuid_clock-seq-low           %uuid_clock-seq-low
                   :%uuid_node                    %uuid_node)))

(defmacro uuid-string-parse-integer (str start end type)
  ;; (macroexpand-1 '(uuid-string-parse-integer "6ba7b810-9dad-11d1-80b4-00c04fd430c8" 0 8 uuid-ub32))
  `(the ,type (parse-integer ,str :start ,start :end ,end :radix 16)))

;;; ==============================
;; :TODO This should also check for a uuid-hex-string-32 and return a symbol
;; naming the appropriate dispatch function for the correct offsets.
(declaim (inline make-uuid-from-string-if))
(defun make-uuid-from-string-if (uuid-hex-string-36-if)
  (declare 
   (inline uuid-hex-string-36-p)
   (optimize (speed 3)))
  (or (and (uuid-hex-string-36-p uuid-hex-string-36-if)
           (the uuid-string-36 uuid-hex-string-36-if))
      #-mon (error "Arg UUID-HEX-STRING-36-IF not `uuid-hex-string-36-p'~% ~
             got: ~S~% ~
             type-of: ~S~%" uuid-hex-string-36-if (type-of uuid-hex-string-36-if))
      #+mon (MON:SIMPLE-ERROR-MON :w-sym  'make-uuid-from-string-if
                                  :w-type 'function
                                  :w-spec "Arg UUID-HEX-STRING-36-IF not `uuid-hex-string-36-p'"
                                  :w-got uuid-hex-string-36-if
                                  :w-type-of t
                                  :signal-or-only nil)))

(defun sxhash-uuid (uuid)
  (declare (unique-universal-identifier uuid)
           (optimize (speed 3)))
  (sxhash (the uuid-bit-vector-128 (uuid-to-bit-vector uuid))))

#+sbcl 
(sb-ext:define-hash-table-test uuid-eql sxhash-uuid)

#-sbcl
(defun make-hash-table-uuid (&key synchronized) 
  (declare (ignore synchronized))
  (make-hash-table :test 'equal))

#+sbcl
(defun make-hash-table-uuid (&key synchronized) ;; &allow-other-keys ??
  (make-hash-table :test 'uuid-eql :synchronized synchronized))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
