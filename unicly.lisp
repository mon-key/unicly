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
  (declare (type uuid-byte-array-16 namespace)
           (type uuid-byte-array name)
           (optimize (speed 3)))
  (let ((digester  (ironclad:make-digest :MD5)))
    (declare (ironclad:md5 digester))
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (the (values uuid-byte-array-16 &optional) (ironclad:produce-digest digester))))

(declaim (inline %uuid-digest-uuid-instance-sha1))
(defun %uuid-digest-uuid-instance-sha1 (namespace name)
  (declare (type uuid-byte-array-16 namespace)
           (type uuid-byte-array name)
           (optimize (speed 3)))
  (let ((digester  (ironclad:make-digest :SHA1)))
    (declare (type ironclad:sha1 digester))
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    ;; SHA-1 uses a 160-bit (20-byte) message size
    (the (values uuid-byte-array-20 &optional) (ironclad:produce-digest digester))))

(declaim (inline %verify-version-3-or-5))
(defun %verify-version-3-or-5 (version)
  (declare (type (mod 6) version)
           (optimize (speed 3)  (debug 0)))
  ;; (or (and (or (= version 3) (= version 5)) version)
  ;; (error "arg VERSION is not integer 3 nor 5"))
  #-sbcl (etypecase version 
           ((mod 6) version))
  (unless (logbitp 1 (logcount version))
    (error "arg VERSION is not integer 3 nor 5"))
  (the uuid-v3or5-int version))

(declaim (inline %verify-digest-version))
(defun %verify-digest-version (chk-version)
  (declare (type (mod 6) chk-version)
           (inline %verify-version-3-or-5)
           (optimize (speed 3)))
  (if (logbitp 1 (the uuid-v3or5-int (%verify-version-3-or-5 chk-version)))
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
  (declare (type (mod 6) digest-version)
           (unique-universal-identifier uuid-namespace-instance)
           (string name)
           (inline %verify-digest-version %uuid-digest-uuid-instance-sha1 %uuid-digest-uuid-instance-md5)
           (optimize (speed 3)))
  (let ((uuid-ba (the (values uuid-byte-array-16 &optional)
                   (uuid-get-namespace-bytes uuid-namespace-instance)))
        (name-ba
         #-(or sbcl clisp) (the uuid-byte-array (flexi-streams:string-to-octets name :external-format :UTF-8))
         #+clisp (the uuid-byte-array (ext:convert-string-to-bytes name charset:utf-8))
         #+sbcl  (the uuid-byte-array (sb-ext:string-to-octets name :external-format :UTF-8))))
    (declare (type uuid-byte-array-16 uuid-ba)
             (type uuid-byte-array    name-ba))
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
           (type (mod 6) version)
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
  (declare (type uuid-byte-array-20 v5-digest-byte-array)
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
  (declare (type uuid-byte-array-16 v3-digest-byte-array)
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
  (declare (type uuid-byte-array digest-byte-array)
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
           (type unique-universal-identifier namespace)
           (inline digested-v3or5-uuid)
           (optimize (speed 3)))
  (the (values unique-universal-identifier &optional)
    (digested-v3or5-uuid (the uuid-byte-array-16 (uuid-digest-uuid-instance 3 namespace name)) 3)))

(defun make-v5-uuid (namespace name)
  (declare (type string name)
           (type unique-universal-identifier namespace)
           (inline uuid-digest-uuid-instance
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
           (type unique-universal-identifier uuid))
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
;; New version approx. 2-3 times faster on SBCL x86-32 1.50
(defun uuid-from-byte-array (byte-array)
  (declare (type uuid-byte-array-16 byte-array)
           (inline %uuid-byte-array-null-p
                   %uuid_time-low-request
                   %uuid_time-mid-request
                   %uuid_byte-array-16-ub8-reqeust
                   %uuid_node-request)
           (optimize (speed 3)))
  #-sbcl (assert (uuid-byte-array-p byte-array) (byte-array)
                 "Arg BYTE-ARRAY does not satisfy `uuid-byte-array-p'")
  (when (%uuid-byte-array-null-p byte-array)
    (return-from uuid-from-byte-array 
      ;; Remember, there can only be one *uuid-null-uuid*!
      (make-instance 'unique-universal-identifier)))
  (make-instance 'unique-universal-identifier
                 :%uuid_time-low               (the uuid-ub32 (%uuid_time-low-request byte-array))
                 :%uuid_time-mid               (the uuid-ub16 (%uuid_time-mid-request byte-array))
                 :%uuid_time-high-and-version  (the uuid-ub16 (uuid-request-integer byte-array 6 2))
                 :%uuid_clock-seq-and-reserved (the uuid-ub8  (%uuid_byte-array-16-ub8-reqeust byte-array 8)) ;; (aref byte-array 8))
                 :%uuid_clock-seq-low          (the uuid-ub8  (%uuid_byte-array-16-ub8-reqeust byte-array 9)) ;; (aref byte-array 9))
                 :%uuid_node                   (the uuid-ub48 (%uuid_node-request byte-array))))

;;; ==============================
;; :OLD VERSION
;; (defun uuid-from-byte-array (byte-array)
;;   ;; :NOTE We declare this a uuid-byte-array-16 even though SHA-1s are arrays of 20 elts
;;   ;; IOW if we call this from uuid-digest-uuid-instance we deserve to fail.
;;   (declare (type uuid-byte-array-16 byte-array)
;;            (inline %uuid-byte-array-null-p))
;;   #-sbcl (assert (uuid-byte-array-p byte-array) (byte-array)
;;                  "Arg BYTE-ARRAY does not satisfy `uuid-byte-array-p'")
;;   (when (%uuid-byte-array-null-p byte-array)
;;     (return-from uuid-from-byte-array 
;;       ;; Remember, there can only be one *uuid-null-uuid*!
;;       (make-instance 'unique-universal-identifier)))
;;   (macrolet ((arr-to-bytes (from to w-array)
;;                "Helper macro used in `uuid-from-byte-array'."
;;                (declare ((mod 17) from to))
;;                `(loop 
;;                    for i from ,from to ,to
;;                    with res = 0
;;                    do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,w-array i))
;;                    finally (return res))))
;;     (make-instance 'unique-universal-identifier
;;                    :%uuid_time-low (the uuid-ub32 (arr-to-bytes 0 3 byte-array))
;;                    :%uuid_time-mid (the uuid-ub16 (arr-to-bytes 4 5 byte-array))
;;                    :%uuid_time-high-and-version (the uuid-ub16 (arr-to-bytes 6 7 byte-array))
;;                    :%uuid_clock-seq-and-reserved (the uuid-ub8 (aref byte-array 8))
;;                    :%uuid_clock-seq-low (the uuid-ub8 (aref byte-array 9))
;;                    :%uuid_node (the uuid-ub48 (arr-to-bytes 10 15 byte-array)))))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
