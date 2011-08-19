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

;; (declaim (inline %uuid-digest-uuid-instance-md5))
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
    ;; SHA-1 uses a 160-bit (20-byte) message size
    (the (values uuid-byte-array-20 &optional) (ironclad:produce-digest digester))))

(declaim (inline %verify-version-3-or-5))
(defun %verify-version-3-or-5 (version)
  (declare ((mod 6) version)
           (optimize (speed 3)  (debug 0)))
  ;; (or (and (or (= version 3) (= version 5)) version)
  ;; (error "arg VERSION is not integer 3 nor 5"))
  (unless (logbitp 1 (logcount version))
    (error "arg VERSION is not integer 3 nor 5"))
  (the uuid-v3or5-int version))

(declaim (inline %verify-digest-version))
(defun %verify-digest-version (chk-version)
  (declare ((mod 6) chk-version)
           (inline %verify-version-3-or-5)
           (optimize (speed 3)))
  (if (logbitp (the uuid-v3or5-int (%verify-version-3-or-5 chk-version)) 0)
      :MD5
      :SHA1))

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
  (let ((uuid-ba (the (values uuid-byte-array-16 &optional)
                   (uuid-get-namespace-bytes uuid-namespace-instance)))
        (name-ba
         #-sbcl (the uuid-byte-array (flexi-streams:string-to-octets name :external-format :UTF-8))
         #+sbcl (the uuid-byte-array (sb-ext:string-to-octets name :external-format :UTF-8))))
    (declare (uuid-byte-array-16 uuid-ba)
             (uuid-byte-array    name-ba))
    (ecase (%verify-digest-version digest-version)
      (:MD5  (the (values uuid-byte-array-16 &optional)
               (%uuid-digest-uuid-instance-md5  uuid-ba name-ba))) 
      (:SHA1 
       (the (values uuid-byte-array-20 &optional)
         (%uuid-digest-uuid-instance-sha1 uuid-ba name-ba))))))


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
  (the unique-universal-identifier
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low (%uuid_time-low-request v5-digest-byte-array)
                   :%uuid_time-mid (%uuid_time-mid-request v5-digest-byte-array)
                   :%uuid_time-high-and-version (%uuid_time-high-and-version-request v5-digest-byte-array 5)
                   :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v5-digest-byte-array)
                   :%uuid_clock-seq-low (the uuid-ub8 (%uuid_clock-seq-low-request v5-digest-byte-array))
                   :%uuid_node (%uuid_node-request v5-digest-byte-array))))

(declaim (inline digested-v3-uuid))
(defun digested-v3-uuid (v3-digest-byte-array)
  (declare (uuid-byte-array-16 v3-digest-byte-array)
           (inline %uuid_time-low-request %uuid_time-mid-request %uuid_time-high-and-version-request
                   %uuid_clock-seq-and-reserved-request %uuid_node-request)
           (optimize (speed 3)))
  (the unique-universal-identifier
    (make-instance 'unique-universal-identifier
                   :%uuid_time-low (%uuid_time-low-request v3-digest-byte-array)
                   :%uuid_time-mid (%uuid_time-mid-request v3-digest-byte-array)
                   :%uuid_time-high-and-version (%uuid_time-high-and-version-request v3-digest-byte-array 3)
                   :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v3-digest-byte-array)
                   :%uuid_clock-seq-low (%uuid_clock-seq-low-request v3-digest-byte-array)
                   :%uuid_node (the uuid-ub48 (%uuid_node-request v3-digest-byte-array)))))

(declaim (inline digested-v3or5-uuid))
(defun digested-v3or5-uuid (digest-byte-array digest-3-or-5) 
  (declare (uuid-byte-array digest-byte-array)
           (inline %verify-version-3-or-5 digested-v3-uuid digested-v5-uuid)
           (optimize (speed 3)))
  (let ((version-if  (%verify-version-3-or-5 digest-3-or-5)))
    (the unique-universal-identifier
      (ecase (the uuid-v3or5-int version-if)
        (3  
         (setf version-if (the unique-universal-identifier
                            (digested-v3-uuid (the uuid-byte-array-16 digest-byte-array)))))
        (5  
         (setf version-if (the unique-universal-identifier
                            (digested-v5-uuid (the uuid-byte-array-20 digest-byte-array)))))))))

(defun make-v3-uuid (namespace name)
  (declare (type string name)
           (unique-universal-identifier namespace)
           (inline 
             digested-v3or5-uuid)
           (optimize (speed 3)))
  (the (values unique-universal-identifier &optional)
    (digested-v3or5-uuid (the uuid-byte-array-16 (uuid-digest-uuid-instance 3 namespace name)) 3)))

(defun make-v5-uuid (namespace name)
  (declare (type string name)
           (unique-universal-identifier namespace)
           (inline 
             uuid-digest-uuid-instance
             digested-v3or5-uuid)
           (optimize (speed 3)))
  (the (values unique-universal-identifier &optional)
    (digested-v3or5-uuid (the uuid-byte-array-20 (uuid-digest-uuid-instance 5 namespace name))  5)))

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
  (declare (special *random-state-uuid*)
           (optimize (speed 3)))
  (let ((*random-state* (the random-state *random-state-uuid*)))
    (the unique-universal-identifier
      (make-instance 'unique-universal-identifier
                     :%uuid_time-low (the uuid-ub32 (random #xFFFFFFFF))
                     :%uuid_time-mid (the uuid-ub16 (random #xFFFF))
                     :%uuid_time-high-and-version  
                     (the uuid-ub16 (dpb #b0100 (byte 4 12) (ldb (byte 12 0) (the uuid-ub16 (random #xFFFF)))))
                     :%uuid_clock-seq-and-reserved
                     (the uuid-ub8  (dpb #b0010 (byte 2  6) (ldb (byte  8 0) (the uuid-ub8 (random #xFF)))))
                     :%uuid_clock-seq-low (the uuid-ub8 (random #xFF))
                     :%uuid_node (the uuid-ub48 (random #xFFFFFFFFFFFF))))))

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
(defun make-null-uuid ()
  ;; (eq *uuid-null-uuid* (make-null-uuid))
  (declare (inline %unique-universal-identifier-null-p))
  (if (and *uuid-null-uuid*
           (%unique-universal-identifier-null-p *uuid-null-uuid*))
      (the unique-universal-identifier-null *uuid-null-uuid*)
      (%make-null-uuid-loadtime)))


;;; ==============================
;;; :DEPRECATED
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
