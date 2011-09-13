;;; :FILE-CREATED <Timestamp: #{2011-08-17T16:40:17-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-string-uuid.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

(declaim (inline uuid-hex-vector-parse-time-low
                 uuid-hex-vector-parse-time-mid
                 uuid-hex-vector-parse-time-high-and-version
                 uuid-hex-vector-parse-clock-seq-and-reserved
                 uuid-hex-vector-parse-clock-seq-low
                 uuid-hex-vector-parse-node))
;;
(def-indexed-hexstring-integer-parser  "TIME-LOW"               0 uuid-hex-string-8  0 8  uuid-ub32)
(def-indexed-hexstring-integer-parser  "TIME-MID"               1 uuid-hex-string-4  0 4  uuid-ub16)
(def-indexed-hexstring-integer-parser  "TIME-HIGH-AND-VERSION"  2 uuid-hex-string-4  0 4  uuid-ub16)
(def-indexed-hexstring-integer-parser  "CLOCK-SEQ-AND-RESERVED" 3 uuid-hex-string-4  0 2  uuid-ub8)
(def-indexed-hexstring-integer-parser  "CLOCK-SEQ-LOW"          3 uuid-hex-string-4  2 4  uuid-ub8)
(def-indexed-hexstring-integer-parser  "NODE"                   4 uuid-hex-string-12 0 12 uuid-ub48)


;;; ==============================
;; :TODO This should also check for a uuid-hex-string-32 and return a symbol
;; naming the appropriate dispatch function for the correct offsets.
(declaim (inline make-uuid-from-string-if))
(defun make-uuid-from-string-if (uuid-hex-string-36-if)
  (declare (inline uuid-hex-string-36-p)
           (optimize (speed 3)))
  (multiple-value-bind (if-36 vector-5-or-null-uuid) (uuid-hex-string-36-p uuid-hex-string-36-if)
    (declare (boolean if-36)
             (type (or null function uuid-simple-vector-5) vector-5-or-null-uuid))
    (if if-36
        (etypecase vector-5-or-null-uuid
          (compiled-function    (the compiled-function vector-5-or-null-uuid))
          (function             (the function vector-5-or-null-uuid))
          (uuid-simple-vector-5 (the uuid-simple-string-vector-5 vector-5-or-null-uuid)))
        #-:MON 
        (error "Arg UUID-HEX-STRING-36-IF not `uuid-hex-string-36-p'~% ~
             got: ~S~% ~
             type-of: ~S~%" uuid-hex-string-36-if (type-of uuid-hex-string-36-if))
        #+(and :IS-MON :MON) 
        (MON:SIMPLE-ERROR-MON :w-sym  'make-uuid-from-string-if
                              :w-type 'function
                              :w-spec "Arg UUID-HEX-STRING-36-IF not `uuid-hex-string-36-p'"
                              :w-got uuid-hex-string-36-if
                              :w-type-of t
                              :signal-or-only nil))))

;; (multiple-value-bind (if-36 vector-5-or-null-uuid) (unicly::uuid-hex-string-36-p "00000000-0000-0000-0000-000000000000") 
;;   (list if-36 vector-5-or-null-uuid))
;; (multiple-value-bind (if-36 vector-5-or-null-uuid) (unicly::uuid-hex-string-36-p "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
;;   (list if-36 vector-5-or-null-uuid))
;; (typep (unicly::make-uuid-from-string-if "6ba7b810-9dad-11d1-80b4-00c04fd430c8") '(simple-array simple-string (5)))
;; (unicly::make-uuid-from-string-if "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
;; (typep (unicly::make-uuid-from-string-if "00000000-0000-0000-0000-000000000000") 'compiled-function)
;;
;; (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")

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
  (declare (type (or unique-universal-identifier string) uuid-or-hex-string-36)
           (inline make-uuid-from-string-if
                   uuid-hex-string-36-p
                   uuid-hex-vector-parse-time-low uuid-hex-vector-parse-time-mid
                   uuid-hex-vector-parse-time-high-and-version
                   uuid-hex-vector-parse-clock-seq-and-reserved
                   uuid-hex-vector-parse-clock-seq-low uuid-hex-vector-parse-node)
           (optimize (speed 3)))
  (let ((chk-uuid-str (etypecase uuid-or-hex-string-36
                        (unique-universal-identifier 
                         (return-from make-uuid-from-string (the unique-universal-identifier (uuid-copy-uuid uuid-or-hex-string-36))))
                        (string
                         (let ((vec-or-fun (make-uuid-from-string-if uuid-or-hex-string-36)))
                           (declare (type (or function compiled-function uuid-simple-vector-5) vec-or-fun))
                           (etypecase vec-or-fun
                             ((or function compiled-function)
                              (let ((null-id (funcall vec-or-fun)))
                                (declare (type unique-universal-identifier-null null-id))
                                (return-from make-uuid-from-string
                                  (the unique-universal-identifier-null null-id))))
                             (uuid-simple-vector-5 (the uuid-simple-string-vector-5 vec-or-fun))))))))
    (declare ;;(optimize (speed 3))
     ;; (type uuid-simple-vector-5 chk-uuid-str)
     (type uuid-simple-string-vector-5 chk-uuid-str))
    (the unique-universal-identifier
      (make-instance 'unique-universal-identifier
                     :%uuid_time-low               (uuid-hex-vector-parse-time-low               chk-uuid-str)
                     :%uuid_time-mid               (uuid-hex-vector-parse-time-mid               chk-uuid-str)
                     :%uuid_time-high-and-version  (uuid-hex-vector-parse-time-high-and-version  chk-uuid-str)
                     :%uuid_clock-seq-and-reserved (uuid-hex-vector-parse-clock-seq-and-reserved chk-uuid-str)
                     :%uuid_clock-seq-low          (uuid-hex-vector-parse-clock-seq-low          chk-uuid-str)
                     :%uuid_node                   (uuid-hex-vector-parse-node                   chk-uuid-str)))))



;;; ==============================

#+(or)
(defun uuid-string-to-sha1-byte-array (string)
  (declare (type string string))
  (let ((digester (ironclad:make-digest :sha1)))
    (declare (ironclad:sha1 digester))
    (ironclad:update-digest digester 
                            #+sbcl (sb-ext:string-to-octets string :external-format :UTF-8)
                            #-sbcl (flexi-streams:string-to-octets string :external-format :UTF-8))
    (ironclad:produce-digest digester)))

#+(or)
(defun uuid-string-to-md5-byte-array (string)
  (declare (type string string))
  (let ((digester (ironclad:make-digest :MD5)))
    (declare (type ironclad:MD5 digester))
    (ironclad:update-digest digester 
                            #+sbcl (sb-ext:string-to-octets string :external-format :UTF-8)
                            #-sbcl (flexi-streams:string-to-octets string :external-format :UTF-8))
    (ironclad:produce-digest digester)))

;; :SOURCE cl-crypto/source/aes16.lisp
#+(or)
(defun hex-str->bin-array (hex-str)
  "Convert a hex string to binary array. 
Length of hex string must be mulitple of 2"
  (let* ((bin-len (/ (length hex-str) 2))
	 (bin (make-array bin-len :element-type 'uint-8)))
    (dotimes (i bin-len)
      (setf (aref bin i)
	    (parse-integer hex-str :radix 16
			   :start (* 2 i)
			   :end (* 2 (1+ i)))))   
    bin))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
