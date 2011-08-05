;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:24:46-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-deprecated.lisp
;;; ==============================

;;; ==============================
;;; :NOTE This file is contained of definitions for two searate classes of symbols:
;;  - Those from the original UUID library which are either not being used by
;;    unicly or which are found to be suboptimal for use with unicly.
;;  - Those from the development of unicly which are mis-attempts, are
;;    superseded by a generic/method combination or which are otherwise
;;    suboptimal and/or non-functional
;;; ==============================


(in-package #:unicly)



;;; ==============================
;;; :NOTE Everything below here is deprecated, including `uuid-digest-uuid-string'.
;;; ==============================



;;; ==============================
;;; SUPERSEDED-SLOW-OR-BUGGY
;;; ==============================

;;; ==============================
;; (defun make-uuid-from-string (uuid-or-hex-string-36)
;;   (declare ((or unique-universal-identifier string) uuid-or-hex-string-36))
;;   (let ((chk-uuid-str (typecase uuid-or-hex-string-36
;;                         (unique-universal-identifier 
;;                          (return-from make-uuid-from-string (uuid-copy-uuid uuid-or-hex-string-36)))
;;                         (string (make-uuid-from-string-if uuid-or-hex-string-36)))))
;;     (declare (uuid-string-36 chk-uuid-str)
;;              (optimize (speed 3)))
;;     (make-instance 'unique-universal-identifier
;;                    :%uuid_time-low 
;;                    (the uuid-ub32 (parse-integer chk-uuid-str :start 0 :end 8  :radix 16))
;;                    :%uuid_time-mid 
;;                    (the uuid-ub16 (parse-integer chk-uuid-str :start 9 :end 13 :radix 16))
;;                    :%uuid_time-high-and-version 
;;                    (the uuid-ub16 (parse-integer chk-uuid-str :start 14 :end 18 :radix 16))
;;                    :%uuid_clock-seq-and-reserved 
;;                    (the uuid-ub8  (parse-integer chk-uuid-str :start 19 :end 21 :radix 16))
;;                    :%uuid_clock-seq-low 
;;                    (the uuid-ub8  (parse-integer chk-uuid-str :start 21 :end 23 :radix 16))
;;                    :%uuid_node 
;;                    (the uuid-ub48 (parse-integer chk-uuid-str :start 24 :end 36 :radix 16)))))
;;
;;; ==============================

;;; ==============================
;; (declaim (inline uuid-disassemble-ub48))
;; (defun uuid-disassemble-ub48 (u48)
;;   (declare (type uuid-ub48 u48)
;;            (optimize (speed 3)))
;;   (let ((b1 (ldb (byte 8 40) u48))
;;         (b2 (ldb (byte 8 32) u48))
;;         (b3 (ldb (byte 8 24) u48))
;;         (b4 (ldb (byte 8 16) u48))
;;         (b5 (ldb (byte 8  8) u48))
;;         (b6 (ldb (byte 8  0) u48)))
;;     (declare (type uuid-ub8 b1 b1 b2 b3 b4 b5 b6))
;;     (values b1 b2 b3 b4 b5 b6)))
;;; ==============================

;;; ==============================
;; (defun uuid-number-to-byte-array (num)
;;   (declare ((integer 0 *) num))
;;   (if (zerop num)
;;       (values (make-array 1 :element-type 'uuid-ub8 :initial-contents (list 0)) 1)
;;       (loop
;;          :with type-cnt = (uuid-get-bytes-for-integer num)
;;          :for val = num :then (ash val -8)
;;          :for count :from 0 below type-cnt 
;;          ;; Knock down all 1 bits above 255 to 0 ;; #XFF -> 255
;;          :collect (logand val #XFF) :into bits
;;          :finally (return (values (make-array count 
;;                                               :element-type 'uuid-ub8 
;;                                               :initial-contents  (nreverse bits))
;;                                   count)))))
;;; ==============================

;;; ==============================
;; :NOTE now a generic-function/method specialized on `unique-universal-identifier'
;; (defun uuid-princ-to-string (uuid-instance)
;;   (declare (unique-universal-identifier uuid-instance))
;;   #-sbcl (assert (typep uuid-instance 'unique-universal-identifier)
;;                  (uuid-instance)
;;                  "Arg UUID-INSTANCE does not satisfy `unique-universal-identifier-p'")
;;   (princ-to-string uuid-instance))
;;; ==============================

;;; ==============================
;; :DOESN'T work
;; (defun %BUGGY-make-uuid-namespace-loadtime-vars (namespace-pairs)
;;   (loop 
;;      :for (nm . id-string)  namespace-pairs
;;      :when (uuid-hex-string-36-p id-string)
;;      :do  (setf nm (make-uuid-from-string id-string))))
;;; ==============================

;;; ==============================
;; (fundoc 'make-uuid-namespace-loadtime-vars
;; "Set values of NAMESPACE-PAIRS to UUIDs at loadtime.~%~@
;; NAMESPACE-PAIRS is a list of conses each of the form:~%
;;  \( <SYMBOL> . <UUID-HEX-STRING-36> \)~%~@
;; <SYMBOL> should be a variable or parameter in the current environment.~%~@
;; <UUID-HEX-STRING-36> should satisfy `dbc:uuid-hex-string-36-p'.~%~@
;; Value is set as if by `dbc:make-uuid-from-string'.~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;; ==============================

;;; ==============================
;; :NOTE unique-universal-identifier-p is now a generic-function.
;; (fundoc 'unique-universal-identifier-p
;; "Whether MAYBE-UUID-INSTANCE is a unique-universal-identifier.
;; Return T when argument is an instace of the class
;; `unique-universal-identifier' or one of its subclasses.~%~@
;; :EXAMPLE~%
;;  \(unique-universal-identifier-p *uuid-namespace-dns*\)
;;  \(unique-universal-identifier-p t\)~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;; ==============================

;;; ==============================
;; (fundoc 'uuid-princ-to-string
;;             "Return string representation fo UUID-INSTANCE as if by `cl:princ-to-string'.~%~@
;; UUID-INSTANCE should satisfy `unique-universal-identifier-p', an error is
;; signaled if not~%~@
;; :EXAMPLE~%~@
;;  \(uuid-princ-to-string *uuid-namespace-dns*\)
;;  \(uuid-princ-to-string t\)
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;; ==============================

;;; ==============================
;; :NOW a generic function 
;; (fundoc 'uuid-print-bytes
;; "Print the raw bytes of UUID in hexadecimal format to STREAM.~%~@
;; UUID is an instance of `unique-universal-identifier' class.~%~@
;; STREAM is an output-stream.
;; Output of returned has the format:~%
;;  6ba7b8109dad11d180b400c04fd430c8~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;; ==============================

;; (typedoc 'uuid-integer-length
;;          "An object of this type is an integer of type \(unsigned-byte 3\) in the set {1 2 4 6}.~%~@
;; These represent valid `cl:integer-length's capable of representing the numeric
;; value in a fully intialized instance of the class `unique-universal-identifier'.
;; :EXAMPLE~%
;;  \(mapcar #'\(lambda \(x\) \(cons \(typep x 'uuid-integer-length\) x\)\) '\(0 1 2 3 4 5 6 7\)\)~%
;; :SEE-ALSO `uuid-number-to-byte-array'.~%▶▶▶")


;;; ==============================
;;; ==============================
;;; :NOTE The type signature of the arrray returned by `uuid-digest-uuid-string' differs for :MD5 vs :SHA1
;;; v5 :SHA1 returns an array of type: (simple-array (unsigned-byte 8) (20))
;;; v3 :MD5 returns  an array of type: (simple-array (unsigned-byte 8) (16))
;;
;;; :NOTE No longer using. This was specialized on the return value of:
;;;  (uuid-get-bytes (uuid-print-bytes nil *uuid-namespace-dns*))
;; (defun uuid-digest-uuid-string (digest-version string-uuid name)
;;   ;; This is step two of RFC4122 section 4.3 
;;   ;; -  Compute the hash of the name space ID concatenated with the name.
;;   (declare ((mod 6) digest-version)
;;            (type string name string-uuid))
;;   (let ((digester  (ironclad:make-digest (%verify-digest-version digest-version))))
;;     ;; :NOTE `mon:ascii-string-to-byte-array'
;;     (ironclad:update-digest digester (ironclad:ascii-string-to-byte-array string-uuid))
;;     (ironclad:update-digest digester 
;;                             ;; #-sbcl (trivial-utf-8:string-to-utf-8-bytes name))
;;                             ;; :NOTE sb-ext:string-to-octets is approx 11x
;;                             ;; faster and conses 75% less than flexi-streams:string-to-octets
;;                             #-sbcl (the uuid-byte-array (flexi-streams:string-to-octets name :external-format :UTF-8))
;;                             #+sbcl (the uuid-byte-array (sb-ext:string-to-octets name :external-format :UTF-8)))
;;     (the (values uuid-byte-array &optional) (ironclad:produce-digest digester))))
;;
;;
;; (defun uuid-print-bytes (stream uuid)
;;   (declare (type unique-universal-identifier uuid)
;;            (optimize (speed 3)))
;;   (with-slots (%uuid_time-low
;;                %uuid_time-mid
;;                %uuid_time-high-and-version
;;                %uuid_clock-seq-and-reserved
;;                %uuid_clock-seq-low
;;                %uuid_node)
;;       uuid
;;     (declare (type uuid-ub32 %uuid_time-low)
;;              (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
;;              (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
;;              (type uuid-ub48 %uuid_node))
;;     (format stream "~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X" 
;;             %uuid_time-low
;;             %uuid_time-mid 
;;             %uuid_time-high-and-version
;;             %uuid_clock-seq-and-reserved
;;             %uuid_clock-seq-low
;;             %uuid_node)))
;;
;; :WAS The convoluted print-bytes/get-bytes route
;; (defun make-v3-uuid (namespace name)
;;   (declare (type string name)
;;            (unique-universal-identifier namespace)
;;            (optimize (speed 3)))
;;   ;;;; (digested-v3or5-uuid (uuid-digest-uuid-string 3 (uuid-get-bytes (uuid-print-bytes nil namespace)) name) 3))
;;    (digested-v3or5-uuid
;;     (the uuid-byte-array-16    
;;       (uuid-digest-uuid-string 3 
;;         (the uuid-byte-string 
;;           (uuid-get-bytes 
;;            (the uuid-string-32
;;              (uuid-print-bytes nil namespace)))) name)) 3))
;;
;; :WAS The convoluted print-bytes/get-bytes route
;; (defun make-v5-uuid (namespace name)
;;   (declare (type string name)
;;            (unique-universal-identifier namespace)
;;            (optimize (speed 3)))
;;   ;; (digested-v3or5-uuid (uuid-digest-uuid-string  5 (uuid-get-bytes (uuid-print-bytes nil namespace)) name) 5))
;;   (digested-v3or5-uuid
;;    (the uuid-byte-array-20
;;      (uuid-digest-uuid-string 5
;;        (the uuid-byte-string 
;;          (uuid-get-bytes 
;;           (the uuid-string-32
;;             (uuid-print-bytes nil namespace)))) name)) 5)
;;
;;
;;; ==============================
;; (defmacro arr-to-bytes (from to array)
;;   "Helper macro used in `uuid-from-byte-array'."
;;   `(loop for i from ,from to ,to
;; 	 with res = 0
;; 	 do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,array i))
;; 	 finally (return res)))
;;
;;; :TESTING use of `macrolet'
;; (defun arr-to-bytes-mlet (array)
;;   (macrolet ((arr-to-bytes (from to w-array)
;;                (declare ((mod 17) from to))
;;                `(loop 
;;                    for i from ,from to ,to
;;                    with res = 0
;;                    do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,w-array i))
;;                    finally (return res))))
;;     (arr-to-bytes 0 3 array )))
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
