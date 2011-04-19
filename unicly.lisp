;;; :FILE-CREATED <Timestamp: #{2011-03-03T18:39:55-05:00Z}#{11094} - by MON>
;;; :FILE  unicly/unicly.lisp
;;; ==============================

;;; ==============================
;;
;; ,---- RFC4122 Section 2. "Motivation":
;; | UUIDs are of a fixed size (128 bits) which is reasonably small
;; | compared to other alternatives.  This lends itself well to sorting,
;; | ordering, and hashing of all sorts, storing in databases, simple
;; | allocation, and ease of programming in general.
;; | 
;; | Since UUIDs are unique and persistent, they make excellent Uniform
;; | Resource Names.  The unique ability to generate a new UUID without a
;; | registration process allows for UUIDs to be one of the URNs with the
;; | lowest minting cost.
;; `----
;; 
;; ,---- RFC4122 Section 3. "Namespace Registration Template":
;; | Identifier persistence considerations:
;; | UUIDs are inherently very difficult to resolve in a global sense.
;; | This, coupled with the fact that UUIDs are temporally unique
;; | within their spatial context, ensures that UUIDs will remain as
;; |  persistent as possible.
;; `----
;;
;; ,----  RFC4122 section 4.3. (Prologue):
;; | The version 3 or 5 UUID is meant for generating UUIDs from "names"
;; | that are drawn from, and unique within, some "name space".  The
;; | concept of name and name space should be broadly construed, and not
;; | limited to textual names.  For example, some name spaces are the
;; | domain name system, URLs, ISO Object IDs (OIDs), X.500 Distinguished
;; | Names (DNs), and reserved words in a programming language.  The
;; | mechanisms or conventions used for allocating names and ensuring
;; | their uniqueness within their name spaces are beyond the scope of
;; | this specification.
;; | 
;; | The requirements for these types of UUIDs are as follows:
;; | 
;; | o  The UUIDs generated at different times from the same name in the
;; |    same namespace MUST be equal.
;; | 
;; | o  The UUIDs generated from two different names in the same namespace
;; |    should be different (with very high probability).
;; | 
;; | o  The UUIDs generated from the same name in two different namespaces
;; |    should be different with (very high probability).
;; | 
;; | o  If two UUIDs that were generated from names are equal, then they
;; |    were generated from the same name in the same namespace (with very
;; |    high probability).
;; `----
;;
;;; ==============================


(in-package #:unicly)
;; *package*


;;; ==============================
;;; :PASTE-AUTHOR nyef -- Alistair Bridgewater
;;; :PASTE-TITLE Informing loop of integer size -- how to do it idiomatically?
;;; :PASTE 120426 :PASTE-URL (URL `http://paste.lisp.org/+2KX6/1')
(defun uuid-request-integer (array offset length &key little-endian sign-extend)
  (let ((value (loop
                  for i from 0 below length
                  for octet = (aref array (+ offset
                                             (if little-endian
                                                 i
                                                 (- length i 1))))
                  sum (ash octet (* i 8)))))
    (if (and sign-extend
             (logbitp (1- (* length 8)) value))
        (logior (lognot (1- (ash 1 (1- (* length 8))))) value)
        value)))
;;
(define-compiler-macro uuid-request-integer (&whole form array offset length &key little-endian sign-extend)
  ;; :NOTE the 4 is an (unsigned-byte 32) which isn't a fixnum on x86-32
  (if (and (member length '(1 2 4)) 
           (member little-endian '(t nil))
           (member sign-extend '(t nil)))
      `(let* (,@(loop
                   for i from 0 below length
                   for var in '(byte-0 byte-1 byte-2 byte-3)
                   collect `(,var (aref ,array (+ ,offset
                                                  ,(if little-endian
                                                       i
                                                       (- length i 1))))))
              (value ,(elt '(#1=byte-0
                             #2=(dpb byte-1 (byte 8 8) #1#)
                             #3=(dpb byte-2 (byte 8 16) #2#)
                             (dpb byte-3 (byte 8 24) #3#))
                           (1- length))))
         ,(if sign-extend
              `(if (logbitp ,(1- (* length 8)) value)
                   (logior ,(lognot (1- (ash 1 (1- (* length 8))))) value)
                   value)
              'value))
      form))

(declaim (inline uuid-disassemble-ub48))
(defun uuid-disassemble-ub48 (u48)
  (declare (uuid-ub48 u48)
           (optimize (speed 3)))
  (let ((b1 nil) (b2 nil) (b3 nil) (b4 nil) (b5 nil) (b6 nil))
    ;; :NOTE The setf/the junk may be ugly, but its certainly faster.
    (setf b1 (ldb (byte 8 40)  u48))
    (setf b2 (ldb (byte 8 32) u48))
    (setf u48 (mask-field (byte 32 0) (the uuid-ub48 u48)))
    (setf b3 (ldb (byte 8 24) (the uuid-ub32 u48)))
    (setf u48 (mask-field (byte 24 0) (the uuid-ub32 u48)))
    (setf b4 (ldb (byte 8 16) (the uuid-ub24 u48)))
    (setf b5 (ldb (byte 8  8) (the uuid-ub24 u48)))
    (setf b6 (ldb (byte 8  0) (the uuid-ub24 u48)))
    (locally (declare (uuid-ub8 b1 b2 b3 b4 b5 b6))
            (values b1 b2 b3 b4 b5 b6))))

;;; ==============================
;; :SOURCE Zach Beane's usenet-legend/io.lisp :WAS `disassemble-u32'
(declaim (inline uuid-disassemble-ub32))
(defun uuid-disassemble-ub32 (u32)
  (declare (type uuid-ub32 u32)
           (optimize (speed 3)))
  (let ((b1 (ldb (byte 8 24) u32))
        (b2 (ldb (byte 8 16) u32))
        (b3 (ldb (byte 8  8) u32))
        (b4 (ldb (byte 8  0) u32)))
    (declare (uuid-ub8 b1 b2 b3 b4))
    (values b1 b2 b3 b4)))

(declaim (inline uuid-disassemble-ub16))
(defun uuid-disassemble-ub16 (u16)
  (declare (type uuid-ub16 u16)
           (optimize (speed 3)))
  (let ((b1 (ldb (byte 8 8) u16))
        (b2 (ldb (byte 8 0) u16)))
    (declare (uuid-ub8 b1 b2))
    (values b1 b2)))

;;; ==============================
;; ,---- RFC4122 4.1.3. Subsection "Version"
;; | The version number is in the most significant 4 bits of the time
;; | stamp (bits 4 through 7 of the time_hi_and_version field).
;; | 
;; |  Msb0  Msb1  Msb2  Msb3   Version  Description
;; |     0     0     0     1        1     The time-based version specified in this document.
;; |     0     0     1     0        2     DCE Security version, with embedded POSIX UIDs.
;; |     0     0     1     1        3     The name-based MD5
;; |     0     1     0     0        4     The randomly or pseudo-randomly generated version
;; |     0     1     0     1        5     The name-based SHA-1
;; |     ^--bit-48
;; `----
;;
;; :TODO Tighten this up to match uuid-bit-vector-version e.g. checking for null-uuid
;;       which RFC4122 Setion 4.1.7. "Nil UUID" specifically requires
;; :TODO Currently not detecting v1 or v2 UUIDs at all.
(declaim (inline uuid-version))
(defun uuid-version (uuid)
  (declare (unique-universal-identifier uuid)
           (optimize (speed 3)))
  (let ((uuid-thav (if (slot-boundp uuid '%uuid_time-high-and-version)
                       (slot-value uuid '%uuid_time-high-and-version)
                       (error (make-condition 'unbound-slot :name '%uuid_time-high-and-version)))))
    (declare (uuid-ub16 uuid-thav))
    (if (ldb-test (byte 1 15) uuid-thav)
        (error "per RFC4122 section 4.1.3 ~
                bit 15 of %uuid_time-high-and-version should always be 0~% ~
                got: ~S" uuid)
        (or (and (ldb-test (byte 1 13) uuid-thav)
                 (ldb-test (byte 1 12) uuid-thav)
                 3)
            (and (ldb-test (byte 1 14) uuid-thav)
                 (or (and (ldb-test (byte 1 12) uuid-thav) 5)
                     (and (not (ldb-test (byte 1 13) uuid-thav)) 4)
                     (error "something wrong with UUID bit field~% got: ~S"
                            uuid-thav)))))))

;;; ==============================
;;; :TODO Finish `uuid-byte-byte-array-version'
;; (defun uuid-byte-array-version (uuid-byte-array)
;;  (declare (uuid-byte-array-16 uuid-byte-array))
;; (

;;; ==============================
;; :TODO use `uuid-string-parse-integer' to get the version from a uuid-string-32
;; :SEE The notes at `make-uuid-from-string-if' and `make-uuid-from-string'.
;; (defun uuid-string-36-version (uuis-hex-string-36) (...))
;; (defun uuid-string-32-version (uuis-hex-string-32) (...))


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
;; #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
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

;;; ==============================
;;  48 49 50 51
;; | 0  0  0  1  | 1  The time-based version specified in this document.
;; | 0  0  1  0  | 2  DCE Security version, with embedded POSIX UIDs.
;; | 0  0  1  1  | 3  The name-based MD5
;; | 0  1  0  0  | 4  The randomly or pseudo-randomly generated version
;; | 0  1  0  1  | 5  The name-based SHA-1
(declaim (inline uuid-bit-vector-version))
(defun uuid-bit-vector-version (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (optimize (speed 3)))
  (unless (zerop (sbit uuid-bit-vector 48))
    ;; :NOTE Its not likely this will happen but its wrong to let the error propogate further.
    (error "per RFC4122 section 4.1.3 bit 48 should always be 0~% got: ~S" uuid-bit-vector))
  (ecase (the bit (sbit uuid-bit-vector 49))
    (0 (ecase (the bit (sbit uuid-bit-vector 50))
         (1 (ecase (the bit (sbit uuid-bit-vector 51))
              (1  3)
              (0  2)))
         (0 (ecase (the bit (sbit uuid-bit-vector 51))
              (1 1)
              ;; RFC4122 Setion 4.1.7. "Nil UUID" specifically requires a null UUID
              ;; However, itdoesn't say anything about the version 0 UUID... 
              ;; Of course it wouldn't given how C centric nature of the entire RFC :)
              ;; So, as a way of flipping the bird to the curly brace inclined we
              ;; choose to return as if by `cl:values': 0, null-uuid
              (0 (values (or #-sbcl (and (uuid-eql uuid-bit-vector (uuid-bit-vector-zeroed)) 0) 
                             #+sbcl (and (sb-int:bit-vector-= uuid-bit-vector (uuid-bit-vector-zeroed)) 0)
                             (error "something wrong with UUID-BIT-VECTOR bit field~% got: ~S" uuid-bit-vector))
                         'null-uuid))))))
    (1 (ecase (the bit (sbit uuid-bit-vector 51))
         (0 4)
         (1 5)))))

(declaim (inline uuid-bit-vector-v3-p))
(defun uuid-bit-vector-v3-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-bit-vector-version)
           (optimize (speed 3)))
  (let ((v3-if (uuid-bit-vector-version uuid-bit-vector)))
    (declare ((integer 0 5) v3-if))
    (= v3-if 3)))

(declaim (inline uuid-bit-vector-v4-p))
(defun uuid-bit-vector-v4-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-bit-vector-version)
           (optimize (speed 3)))
  (let ((v4-if (uuid-bit-vector-version uuid-bit-vector)))
    (declare ((integer 0 5) v4-if))
    (= v4-if 4)))

(declaim (inline uuid-bit-vector-v5-p))
(defun uuid-bit-vector-v5-p (uuid-bit-vector)
  (declare (uuid-bit-vector-128 uuid-bit-vector)
           (inline uuid-bit-vector-version)
           (optimize (speed 3)))
  (let ((v5-if (uuid-bit-vector-version uuid-bit-vector)))
    (declare ((integer 0 5) v5-if))
    (= v5-if 5)))

(declaim (inline uuid-bit-vector-zeroed))
(defun uuid-bit-vector-zeroed ()
  (declare (optimize (speed 3)))
  (the uuid-bit-vector-128 (make-array 128 :element-type 'bit :initial-element 0))) 

(declaim (inline uuid-bit-vector-8-zeroed))
(defun uuid-bit-vector-8-zeroed ()
  (declare (optimize (speed 3)))
  (the uuid-bit-vector-8 (make-array 8 :element-type 'bit :initial-element 0)))

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
           (inline uuid-deposit-octet-to-bit-vector)
           (optimize (speed 3)))
  (let ((uuid-bv128 (uuid-bit-vector-zeroed)))
    (declare (uuid-bit-vector-128 uuid-bv128))
    (loop 
       for byte across uuid-byte-array
       for offset upfrom 0 by 8 below 128
       do (uuid-deposit-octet-to-bit-vector byte offset uuid-bv128)
       finally (return  uuid-bv128))))

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
;; :NOTE Return value has the integer representation: 267678999922476945140730988764022209929
;; (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ"))
(defun uuid-to-bit-vector (uuid)
  (declare (type unique-universal-identifier uuid)
           (inline uuid-disassemble-ub32 uuid-disassemble-ub16 uuid-bit-vector-zeroed)
           (optimize (speed 3)))
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
        (uuid-bv128 (the uuid-bit-vector-128 (uuid-bit-vector-zeroed))))
    (declare (list bv-lst) (uuid-bit-vector-128 uuid-bv128))
    (loop 
       for byte in bv-lst
       for offset upfrom 0 by 8 below 128
       do (uuid-deposit-octet-to-bit-vector (the uuid-ub8 byte) offset uuid-bv128)
       finally (return  uuid-bv128))))

(declaim (inline %uuid-digest-uuid-instance-md5))
(defun %uuid-digest-uuid-instance-md5 (namespace name)
  (declare (uuid-byte-array-16 namespace)
           (uuid-byte-array name)
           (optimize (speed 3)))
  (let ((digester  (ironclad:make-digest :MD5)))
    (declare (ironclad:md5 digester))
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (the (values uuid-byte-array-16 &optional) (ironclad:produce-digest digester))))

(declaim (inline %uuid-digest-uuid-instance-sha1))
(defun %uuid-digest-uuid-instance-sha1 (namespace name)
  (declare (uuid-byte-array-16 namespace)
           (uuid-byte-array name)
           (optimize (speed 3)))
  (let ((digester  (ironclad:make-digest :SHA1)))
    (declare (ironclad:sha1 digester))
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (the (values uuid-byte-array-20 &optional) (ironclad:produce-digest digester))))

(defun uuid-get-namespace-bytes (uuid)
  (declare (type unique-universal-identifier uuid)
           (optimize (speed 3)))
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
                                    (uuid-disassemble-ub48 %uuid_node)))))

;;; ==============================
;; :NOTE By dispatching on %UUID-DIGEST-UUID-INSTANCE-SHA1/%UUID-DIGEST-UUID-INSTANCE-MD5
;; We can run approx 3x faster and allocate half as much!
;;
;; However, benchmarks suggest that sb-ext:string-to-octets is significantly faster.
;; 
;; :NOTE sb-ext:string-to-octets is approx 11x faster
;;        and conses 75% less than flexi-streams:string-to-octets
;; :NOTE Significant additional gains can likely be had if we cache the namespace
;; byte-array of an uuid-namespace e.g. uuid-digest-uuid-instance-cached with
;; args DIGEST-VERSION and NAME only.
(defun uuid-digest-uuid-instance (digest-version uuid-namespace-instance name)
  ;; This is step two of RFC4122 section 4.3 
  ;; -  Compute the hash of the name space ID concatenated with the name.
  (declare ((mod 6) digest-version)
           (unique-universal-identifier uuid-namespace-instance)
           (string name)
           (inline %verify-digest-version %uuid-digest-uuid-instance-sha1 %uuid-digest-uuid-instance-md5)
           (optimize (speed 3)))
  (let ((uuid-ba   (uuid-get-namespace-bytes uuid-namespace-instance))
        (name-ba
         #-sbcl (the uuid-byte-array (flexi-streams:string-to-octets name :external-format :UTF-8))
         #+sbcl (the uuid-byte-array (sb-ext:string-to-octets name :external-format :UTF-8))))
    (declare (uuid-byte-array-16 uuid-ba)
             (uuid-byte-array    name-ba))
    (case (%verify-digest-version digest-version)
      (:MD5  (%uuid-digest-uuid-instance-md5  uuid-ba name-ba))
      (:SHA1 (%uuid-digest-uuid-instance-sha1 uuid-ba name-ba)))))



;;; ==============================
;; :NOTE Some of the following are applicable to v4 as well.
;;
;; Steps from RFC4122 Section 4.3 for generating v3 and v5 UUIDS from a "name
;; space ID" according to the hash algorithm chosen:
;;
;; Initially:
;;
;; - Allocate a UUID to use as a "name space ID" for all UUIDs
;;   generated from names in that name space; see Appendix C for some
;;   pre-defined values.
;;
;; - Choose either MD5 or SHA-1  as the hash algorithm; 
;;   If backward compatibility is not an issue, SHA-1 is preferred.
;;
;;
;;  1) 
;;     - Convert the name to a canonical sequence of octets (as defined by
;;       the standards or conventions of its namespace); 
;;       `-> `sb-ext:string-to-octets'/`flexi-streams:string-to-octets'
;; 
;;     - put the namespace ID in network byte order;
;;       `--> `uuid-get-namespace-bytes'
;;
;;  2) Compute the hash of the namespace ID concatenated with the name.
;;     `--> `uuid-digest-uuid-instance'
;;
;;  3) Set octets zero through 3 of the time_low field to octets zero through 3 of the hash. 
;;     `--> `%uuid_time-low-request'
;;
;;  4) Set octets zero and one of the time_mid field to octets 4 and 5 of the hash.
;;     `--> `%uuid_time-mid-request'
;; ,----
;; | 5) Set octets zero and one of the time_hi_and_version field to octets 6 and 7 of the hash.
;; |    `--> `%uuid_time-high-and-version-request'
;; | 
;; | 6) Set the four most significant bits (bits 12 through 15) of the time_hi_and_version field 
;; |    to the appropriate 4-bit version number.
;; |    `--> `%uuid_time-high-and-version-request'  
;; |
;; |    IOW, set top bits #*0101 for SHA1 or #*0011 for MD5.
;; | 
;; |    :NOTE Step 6 correpsonds with Step 2 of Section 4.4 bits #*0100 for v4 random
;; `----
;; ,----
;; | 7) Set the clock_seq_hi_and_reserved field to octet 8 of the hash.
;; |    `--> `%uuid_clock-seq-and-reserved-request'
;; | 
;; | 8) Set the two most significant bits (bits 6 and 7) of the clock_seq_hi_and_reserved
;; |    to zero and one, respectively.
;; |    `--> `%uuid_clock-seq-and-reserved-request'
;; | 
;; |    :NOTE These correpsonds with Step 1 of Section 4.4
;; `----
;;
;;  9) Set the clock_seq_low field to octet 9 of the hash.
;;     `--> `%uuid_clock-seq-low-request'
;;
;; 10) Set octets zero through five of the node field to octets 10 through 15 of the hash.
;;     `--> `%uuid_node-request'
;;
;; 11) Convert the resulting UUID to local byte order.
;;     :NOTE The ironclad/lisp should handle this transparently.
;;
;;; ==============================

(declaim (inline %uuid_time-low-request))
(defun %uuid_time-low-request (byte-array)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))
  (the (values uuid-ub32 &optional)
    (uuid-request-integer byte-array 0 4)))

(declaim (inline %uuid_time-mid-request))
(defun %uuid_time-mid-request (byte-array)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))
  (the (values uuid-ub16 &optional)
    (uuid-request-integer byte-array 4 2)))

(declaim (inline %uuid_time-high-and-version-request))
(defun %uuid_time-high-and-version-request (byte-array version)
  (declare (type uuid-byte-array byte-array)
           ((mod 6) version)
           (optimize (speed 3)))
  (the uuid-ub16 
    (dpb version (byte 4 12)
         (the uuid-ub16 (uuid-request-integer byte-array 6 2)))))

(declaim (inline %uuid_clock-seq-and-reserved-request))
(defun %uuid_clock-seq-and-reserved-request (byte-array)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))
  (the uuid-ub8 (dpb #b0010 (byte 2 6) (the uuid-ub8 (aref byte-array 8)))))

(declaim (inline %uuid_clock-seq-low-request))
(defun %uuid_clock-seq-low-request (byte-array)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))  
  (the uuid-ub8 (aref byte-array 9)))

(declaim (inline %uuid_node-request))
(defun %uuid_node-request (byte-array)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))
  (the (values uuid-ub48 &optional)
    (uuid-request-integer byte-array 10 6)))

(declaim (inline digested-v5-uuid))
(defun digested-v5-uuid (v5-digest-byte-array)
  (declare (uuid-byte-array-20 v5-digest-byte-array)
           (inline %uuid_time-low-request %uuid_time-mid-request %uuid_time-high-and-version-request
                   %uuid_clock-seq-and-reserved-request %uuid_node-request)
           (optimize (speed 3)))
  (make-instance 'unique-universal-identifier
                 :%uuid_time-low (%uuid_time-low-request v5-digest-byte-array)
                 :%uuid_time-mid (%uuid_time-mid-request v5-digest-byte-array)
                 :%uuid_time-high-and-version (%uuid_time-high-and-version-request v5-digest-byte-array 5)
                 :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v5-digest-byte-array)
                 :%uuid_clock-seq-low (the uuid-ub8 (%uuid_clock-seq-low-request v5-digest-byte-array))
                 :%uuid_node (%uuid_node-request v5-digest-byte-array)))

(declaim (inline digested-v3-uuid))
(defun digested-v3-uuid (v3-digest-byte-array)
  (declare (uuid-byte-array-16 v3-digest-byte-array)
           (inline %uuid_time-low-request %uuid_time-mid-request %uuid_time-high-and-version-request
                   %uuid_clock-seq-and-reserved-request %uuid_node-request)
           (optimize (speed 3)))
  (make-instance 'unique-universal-identifier
                 :%uuid_time-low (%uuid_time-low-request v3-digest-byte-array)
                 :%uuid_time-mid (%uuid_time-mid-request v3-digest-byte-array)
                 :%uuid_time-high-and-version (%uuid_time-high-and-version-request v3-digest-byte-array 3)
                 :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v3-digest-byte-array)
                 :%uuid_clock-seq-low (%uuid_clock-seq-low-request v3-digest-byte-array)
                 :%uuid_node (the uuid-ub48 (%uuid_node-request v3-digest-byte-array))))

(declaim (inline %verify-version-3-or-5))
(defun %verify-version-3-or-5 (version)
  (declare ((mod 6) version)
           (optimize (speed 3)  (debug 0)))
  (or (and (typep version '(unsigned-byte 3)) 
           (or (= version 3) (= version 5))
           version)
   (error "arg VERSION is not integer 3 nor 5")))

(declaim (inline %verify-digest-version))
(defun %verify-digest-version (chk-version)
  (declare ((mod 6) chk-version)
           (inline %verify-version-3-or-5)
           (optimize (speed 3)))
  (or (and (= (%verify-version-3-or-5 chk-version) 3) :MD5) :SHA1))

(declaim (inline digested-v3or5-uuid))
(defun digested-v3or5-uuid (digest-byte-array uuid-version) 
  (declare (uuid-byte-array digest-byte-array)
           (inline %verify-version-3-or-5 digested-v3-uuid digested-v5-uuid)
           (optimize (speed 3)))
  (let ((version-if (%verify-version-3-or-5 uuid-version)))
    (declare ((integer 3 5) version-if))
    (ecase version-if 
      (3 (digested-v3-uuid (the uuid-byte-array-16 digest-byte-array)))
      (5 (digested-v5-uuid (the uuid-byte-array-20 digest-byte-array))))))

(defun make-v3-uuid (namespace name)
  (declare (type string name)
           (unique-universal-identifier namespace)
           (inline digested-v3or5-uuid)
           (optimize (speed 3)))
  (digested-v3or5-uuid (the uuid-byte-array-16 (uuid-digest-uuid-instance 3 namespace name)) 3))

(defun make-v5-uuid (namespace name)
  (declare (type string name)
           (unique-universal-identifier namespace)
           (inline digested-v3or5-uuid)
           (optimize (speed 3)))
  (digested-v3or5-uuid (the uuid-byte-array-20 (uuid-digest-uuid-instance 5 namespace name))  5))

;;; ==============================
;; ,---- RFC4122 Section 4.4. "Creating UUIDs from Truly-Random/Pseudo-Random Numbers":
;; | 
;; | The version 4 UUID is meant for generating UUIDs from truly-random or
;; | pseudo-random numbers.  The algorithm is as follows:
;; |
;; |  1)  Set the two most significant bits (bits 6 and 7) of the
;; |     clock_seq_hi_and_reserved to zero and one, respectively.
;; |     `-> Slot `%uuid_clock-seq-and-reserved' 
;; |     :NOTE Steps 7 and 8 for v3 and v5 with `%uuid_clock-seq-and-reserved-request' 
;; |
;; |  2) Set the four most significant bits (bits 12 through 15) of the
;; |     time_hi_and_version field to the 4-bit version number from Section 4.1.3.
;; |     `-> Slot `%uuid_time-high-and-version' e.g. #*0100 
;; |     :NOTE Correspond with step 6 for v3 and v5 which sets top bits 
;; |     #*01010 for SHA1 or #*0011 for MD5 with `%uuid_time-high-and-version-request'
;; |     
;; |  3) Set all the other bits to randomly (or pseudo-randomly) chosen
;; |     values.
;; |     `-> Slots `%uuid_time-low', `%uuid_time-mid', `%uuid-clock-seq-low', `%uuid_node'
;; `----
(defun make-v4-uuid ()
  (declare (special *random-state-uuid*))
  (let ((*random-state* (the random-state *random-state-uuid*)))
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low (the uuid-ub32 (random #xFFFFFFFF))
                   :%uuid_time-mid (the uuid-ub16 (random #xFFFF))
                   :%uuid_time-high-and-version  
                   (the uuid-ub16 (dpb #b0100 (byte 4 12) (ldb (byte 12 0) (the uuid-ub16 (random #xFFFF)))))
                   :%uuid_clock-seq-and-reserved
                   (the uuid-ub8  (dpb #b0010 (byte 2  6) (ldb (byte  8 0) (the uuid-ub8 (random #xFF)))))
                   :%uuid_clock-seq-low (the uuid-ub8 (random #xFF))
                   :%uuid_node (the uuid-ub48 (random #xFFFFFFFFFFFF)))))

(defun uuid-as-urn-string (stream uuid)
  (declare (type STREAM-OR-BOOLEAN-OR-STRING-WITH-FILL-POINTER stream) 
           (unique-universal-identifier uuid))
  ;; :NOTE RFC4122 Section 3. "Namespace Registration Template"
  ;; Case is significant on output.
  (format stream "~(urn:uuid:~A~)" uuid))

;; ,---- RFC4122 Setion 4.1.7. "Nil UUID":
;; | The nil UUID is special form of UUID that is specified to have all
;; | 128 bits set to zero.
;; `----
(declaim (inline make-null-uuid))
(defun make-null-uuid ()
  (make-instance 'unique-universal-identifier))

;;; ==============================
;; :NOTE Should there be a generic function which dispatches on the UUID's
;; representation , e.g. uuid-bit-vector-128, uuid-byte-array-16,
;; unique-universal-identifier, uuid-string-32, uuid-string-36?
;; :NOTE Consider renaming this to `serialize-uuid-byte-array'
(defun serialize-uuid (uuid stream)
  (declare (type unique-universal-identifier uuid)
           (type stream stream)
           (optimize (speed 3)))
  (loop 
     with bv = (the uuid-byte-array-16 (uuid-get-namespace-bytes uuid))
     for i from 0 below 16
     do (write-byte (aref bv i) stream)))

(defun uuid-string-to-sha1-byte-array (string)
  (declare (type string string))
  (let ((digester (ironclad:make-digest :sha1)))
    (declare (ironclad:sha1 digester))
    (ironclad:update-digest digester 
                            #+sbcl (sb-ext:string-to-octets string :external-format :UTF-8)
                            #-sbcl (flexi-streams:string-to-octets string :external-format :UTF-8))
    (ironclad:produce-digest digester)))

;;; ==============================
;; :TODO we can get rid of the macrolet and dispatch with the %uuid-*-request
;; fncns However %uuid_time-high-and-version requires that we first find he
;; uuid-version represented by BYTE-ARRAY in order to properly disassemble b/c
;; not currently comfortable with the thought of accidentally messing up the
;; version bits.
(defun uuid-from-byte-array (byte-array)
  ;; :NOTE We declare this a uuid-byte-array-16 even though SHA-1s are arrays of 20 elts
  ;; IOW if we call this from uuid-digest-uuid-instance we deserve to fail.
  (declare (type uuid-byte-array-16 byte-array))
  #-sbcl (assert (uuid-byte-array-p byte-array) (byte-array)
                 "Arg BYTE-ARRAY does not satisfy `uuid-byte-array-p'")
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
;; :TODO This should also check for a uuid-hex-string-32 and parse at different
;; offsets via dispatch to the appropriate function.
;;
;; :NOTE What about `cl:read-from-string' instead of `cl:parse-integer' e.g.:
;;   (let ((*read-base* 16)) (read-from-string "88"))
;; No. When asked on #lisp noting that the bounds fo string and its contents are
;; known and pre-verified to be all hexadecimal chars -- nyef says:
;; ,----
;; | Use PARSE-INTEGER. It's more efficient, and makes an explicit
;; | statement about what syntax you're expecting as input.
;; `----
(defun make-uuid-from-string (uuid-or-hex-string-36)
  (declare ((or unique-universal-identifier string) uuid-or-hex-string-36)
           (inline make-uuid-from-string-if
                   uuid-hex-string-36-p)
           (optimize (speed 3)))
  (let ((chk-uuid-str (typecase uuid-or-hex-string-36
                        (unique-universal-identifier 
                         (return-from make-uuid-from-string (uuid-copy-uuid uuid-or-hex-string-36)))
                        (string (make-uuid-from-string-if uuid-or-hex-string-36)))))
    (declare (uuid-string-36 chk-uuid-str)
             (optimize (speed 3)))
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low (uuid-string-parse-integer chk-uuid-str  0  8  uuid-ub32) 
                   :%uuid_time-mid (uuid-string-parse-integer chk-uuid-str  9 13  uuid-ub16)
                   :%uuid_time-high-and-version (uuid-string-parse-integer chk-uuid-str 14 18 uuid-ub16)
                   :%uuid_clock-seq-and-reserved (uuid-string-parse-integer chk-uuid-str 19 21 uuid-ub8)
                   :%uuid_clock-seq-low (uuid-string-parse-integer chk-uuid-str 21 23 uuid-ub8)
                   :%uuid_node (uuid-string-parse-integer chk-uuid-str 24 36  uuid-ub48))))


;;; ==============================
;;; :NOTES Regarding functions idioms to incorporate from vivace-graph-v2
;;; ==============================
;; :NOTE Given that the bit-vector representation is guaranteed to be a
;;  `uuid-bit-vector-128' we should be able to just bit dwiddle are way from
;;  index 0 to 127 until we find the "most significiant bit" at which point we
;;  have have the rudiments of less-than/greater-than without the hassle of
;;  string comparisons and with the benefit of integer/numeric sorts...
;; 
;; :FILE vivace-graph-v2-GIT/utilities.lisp
;; (defgeneric less-than (x y)
;;   (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
;;   (:method ((x symbol) (y uuid:uuid))
;;     (string< (symbol-name x) (uuid:print-bytes nil y)))
;;   (:method ((x number) (y uuid:uuid)) 
;;     (string< (write-to-string x) (uuid:print-bytes nil y)))
;;   (:method ((x string) (y uuid:uuid)) 
;;     (string< x (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y uuid:uuid))
;;     (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y string)) (string< (uuid:print-bytes nil x) y))
;;   (:method ((x uuid:uuid) (y symbol)) 
;;     (string< (uuid:print-bytes nil x) (symbol-name y)))
;;   (:method ((x uuid:uuid) (y number)) 
;;     (string< (uuid:print-bytes nil x) (write-to-string y))))
;;
;; (defgeneric greater-than (x y)
;;   (:documentation "Generic greater-than operator.  Allows comparison of apples and oranges.")
;;   (:method ((x symbol) (y uuid:uuid)) (string> (symbol-name x) (uuid:print-bytes nil y)))
;;   (:method ((x number) (y uuid:uuid)) (string> (write-to-string x) (uuid:print-bytes nil y)))
;;   (:method ((x string) (y uuid:uuid)) (string> x (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y uuid:uuid)) (string> (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y string)) (string> (uuid:print-bytes nil x) y))
;;   (:method ((x uuid:uuid) (y symbol)) (string> (uuid:print-bytes nil x) (symbol-name y)))
;;   (:method ((x uuid:uuid) (y number)) (string> (uuid:print-bytes nil x) (write-to-string y))))
;;; ==============================

;;; ==============================
;; vivace-graph-v2's use of `make-hash-table-uuid' 
;; :FILE vivace-graph-v2-GIT/store.lisp
;; (defun make-fresh-store (name location &key (num-locks 10000))
;;   (let ((store
;; 	 (make-instance 'local-triple-store
;;  			:name name
;; 			:location location
;; 			:main-idx (make-hierarchical-index)
;; 			:lock-pool (make-lock-pool num-locks)
;; 			:locks (make-hash-table :synchronized t :test 'equal)
;; 			:text-idx (make-skip-list :key-equal 'equalp
;; 						  :value-equal 'uuid:uuid-eql
;; 						  :duplicates-allowed? t)
;; 			:log-mailbox (sb-concurrency:make-mailbox)
;; 			:index-queue (sb-concurrency:make-queue)
;; 			:delete-queue (sb-concurrency:make-queue)
;; 			:templates (make-hash-table :synchronized t :test 'eql)
;; 			:indexed-predicates (make-hash-table :synchronized t 
;; 							     :test 'eql))))
;;     (add-to-index (main-idx store) (make-uuid-table :synchronized t) :id-idx)
;;     (setf (logger-thread store) (start-logger store))
;;     store))
;;
;; :FILE vivace-graph-v2-GIT/transaction.lisp
;; (defstruct (transaction
;; 	     (:print-function print-transaction)
;; 	     (:conc-name tx-)
;; 	     (:predicate transaction?))
;;   (id (make-uuid))
;;   (queue nil)
;;   (rollback nil)
;;   (mailbox (sb-concurrency:make-mailbox))
;;   (thread (current-thread))
;;   (store nil)
;;   (locks nil))
;;
;; :FILE vivace-graph-v2-GIT/triples.lisp
;; (defgeneric triple-equal (t1 t2)
;;   (:method ((t1 triple) (t2 triple)) (uuid:uuid-eql (id t1) (id t2)))
;;   (:method (t1 t2) nil))
;;
;; (defun make-anonymous-node ()
;;   "Create a unique anonymous node."
;;   (format nil "_anon:~A" (make-uuid)))
;;
;; (let ((regex 
;;        "^_anon\:[0-9abcdefABCEDF]{8}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{12}$"))
;;   (defun anonymous? (node)
;;     (when (stringp node)
;;       (cl-ppcre:scan regex node))))
;;
;;; ==============================


#+nil
(defmacro uuid-get-bytes-for-integer (unsigned-integer)
  ;; (macroexpand-1 '(uuid-get-bytes-for-integer 281474976710655))
  (declare (optimize (speed 3)))
  `(etypecase (integer-length ,unsigned-integer)
     ,@(loop 
          for cnt upfrom 1 below 7
          for x upfrom 0 below 48 by 8
          for low = 0 then (case x 
                             (24 17) ; Skip returning 3 octet value 17-24, bump to 4
                             (40 33) ; Skip returning 5 octet value 33-40, bump to 6
                             (t (+ x 1)))
          for high = 8 then (+ x 8)
          unless  (and (oddp cnt) (> cnt 1))
          collect (list (list 'integer low high) cnt))))


#+nil
(defun uuid-to-byte-array (uuid)
  ;; :NOTE `uuid-get-namespace-bytes' and `uuid-to-byte-array' are essentially the same.
  (declare (unique-universal-identifier uuid)
           (optimize (speed 3)))
  (let ((array (make-array 16 :element-type 'uuid-ub8)))
    (declare (uuid-byte-array-16 array))
    (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
                 %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
        uuid
      (declare (type uuid-ub32 %uuid_time-low)
               (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
               (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
               (type uuid-ub48 %uuid_node))
      (loop
         for i from 3 downto 0
         do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) %uuid_time-low)))
      (loop
         for i from 5 downto 4
         do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) %uuid_time-mid)))
      (loop
         for i from 7 downto 6
         do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) %uuid_time-high-and-version)))
      (setf (aref array 8) (ldb (byte 8 0) %uuid_clock-seq-and-reserved))
      (setf (aref array 9) (ldb (byte 8 0) %uuid_clock-seq-low))
      (loop
         for i from 15 downto 10
         do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) %uuid_node)))
      (the uuid-byte-array-16 array))))

#+nil
(deftype uuid-integer-length ()
  ;; expands to: (or (integer 1 2) (integer 4 4) (integer 6 6))
  '(member 1 2 4 6))

#+nil
(defun uuid-number-to-byte-array (num &optional size)
  ;; :EXAMPLE
  ;; (uuid-number-to-byte-array 825973027016)
  ;;  => #(200 48 212 79 192 0), 6
  ;;
  ;; (slot-value *uuid-namespace-dns* '%uuid_node) ;=> 825973027016
  ;; (logand 825973027016 255)          ;=> 200
  ;; (logand (ash 825973027016 -8) 255) ;=> 48
  ;; (logand (ash 3226457136 -8) 255)   ;=> 212
  ;; (logand (ash 12603348 -8) 255)     ;=> 79
  ;; (logand (ash 49231 -8) 255)        ;=> 192
  ;; (ash 192 -8)                       ;=> 0
  (declare ((integer 0 *) num)
           ((or null uuid-integer-length) size))
  ;; :NOTE This SIZE slot is required b/c for approx. ~1/100 of
  ;; every uuid's generated the %UUID_TIME-MID slot is 0!
  ;; We used to short circuit early whe NUM was `cl:zerop' and simply bail
  ;; returning the `cl:values': => #(0), 1
  ;; However, this was _bad_ b/c a %UUID_TIME-MID must be of length 2 e.g.:
  ;; => #(0 0), 2
  ;; This in turn has later consequences b/c we lost our padding and corrupted
  ;; the valid length of genenerated uuid-byte-arrays of type `uuid-byte-array-16'.
  (if (zerop num)
      (values (make-array 1 :element-type 'uuid-ub8 :initial-element 0) 1)
      (let* ((type-cnt (or size (uuid-get-bytes-for-integer num)))
             (byte-arr (make-array type-cnt :element-type 'uuid-ub8 :initial-element 0)))

        (declare ((mod 7) type-cnt) 
                 ((integer 1 *) num)
                 ((simple-array (unsigned-byte 8) (*)) byte-arr))
        (loop
           for val = num then (ash val -8)
           for count downfrom (1- type-cnt) downto 0
           do (setf (aref byte-arr count) (logand val #XFF))
           finally (return (values byte-arr type-cnt))))))

#+nil
(defun uuid-load-bytes (byte-array &key (byte-size 8) (start 0) end)
  (declare (type uuid-byte-array byte-array)
           (optimize (speed 3)))
  (let ((ret-val 0))
    (loop 
       for i from start to end
       for pos from (- end start) downto 0
       do (setf ret-val (dpb (aref  byte-array i) (byte byte-size (* pos byte-size)) ret-val)))
    ret-val))

#+nil (declaim (inline %uuid-get-bytes-if))
#+nil
(defun %uuid-get-bytes-if (chk-uuid-str)
  (or (and (uuid-hex-string-32-p chk-uuid-str)
           (the uuid-string-32 chk-uuid-str))
      #-mon (error "Arg CHK-UUID-STR not `uuid-hex-string-32-p'~% ~
             got: ~S~% ~
             type-of: ~S~%" chk-uuid-str (type-of chk-uuid-str))
      #+mon (mon:simple-error-mon :w-sym '%uuid-get-bytes-if
                                  :w-type 'function
                                  :w-spec "arg CHK-UUID-STR not `uuid-hex-string-32-p'"
                                  :w-got chk-uuid-str
                                  :w-type-of t
                                  :signal-or-only nil)
      ))
#+nil
(defun uuid-get-bytes (uuid-string)
  (declare (optimize (speed 3) (debug 0))
           (inline %uuid-get-bytes-if))
  (let ((uuid-str-if (%uuid-get-bytes-if uuid-string))
        (outstr (make-array 16 :element-type 'character :initial-element #\NUL :fill-pointer 0)))
    (declare (uuid-string-32 uuid-str-if)
             ((and (vector character 16) (not simple-array)) outstr))
    (with-output-to-string (out outstr)
      (loop
         with max = (- (length uuid-str-if) 2)
         for i = 0 then (+ i 2)
         as  j = (+ i 2)
         as cur-pos = (the uuid-ub8 (parse-integer (subseq uuid-str-if i j) :radix 16))
         do (format out "~A" (code-char (the uuid-ub8 cur-pos)))
         while (< i max))
      (the uuid-byte-string (make-array 16 :element-type 'character :initial-contents outstr)))))

#+nil
(defun uuid-digest-uuid-string (digest-version string-uuid name)
  ;; This is step two of RFC4122 section 4.3 
  ;; -  Compute the hash of the name space ID concatenated with the name.
  (declare ((mod 6) digest-version)
           (type string name string-uuid))
  (let ((digester  (ironclad:make-digest (%verify-digest-version digest-version))))
    ;; :NOTE `mon:ascii-string-to-byte-array'
    (ironclad:update-digest digester (ironclad:ascii-string-to-byte-array string-uuid))
    (ironclad:update-digest digester 
                            ;; #-sbcl (trivial-utf-8:string-to-utf-8-bytes name))
                            ;; :NOTE sb-ext:string-to-octets is approx 11x
                            ;; faster and conses 75% less than flexi-streams:string-to-octets
                            #-sbcl (the uuid-byte-array (flexi-streams:string-to-octets name :external-format :UTF-8))
                            #+sbcl (the uuid-byte-array (sb-ext:string-to-octets name :external-format :UTF-8)))
    (the (values uuid-byte-array &optional) (ironclad:produce-digest digester))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
