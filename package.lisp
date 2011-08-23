;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:39:23-04:00Z}#{11154} - by MON>
;;; :FILE unicly/package.lisp
;;; ==============================

;; (in-package #:unicly) ;; for slime
;; *package*

(defpackage #:unicly
  (:use #:common-lisp)
  (:export 
   ;;
   ;;
   ;; unicly/unicly-specials.lisp
   ;;
   #:*uuid-namespace-dns* 
   #:*uuid-namespace-url* 
   #:*uuid-namespace-oid* 
   #:*uuid-namespace-x500*
   ;; 
   ;; #:*random-state-uuid*
   ;; #:*uuid-null-uuid*                ; Not exported, Bound at loadtime with `make-null-uuid-loadtime' 
   ;;                                   ; and treated as a special case thereafter
   ;;
 ;; unicly/macros.lisp
   ;;
   ;; #:%def-uuid-format-and-intern-symbol                ; FUNCTION
   ;; #:%def-uuid-format-and-intern-symbol-type-predicate ; FUNCTION
   ;; #:%def-uuid-format-and-intern-symbol-type-checker   ; FUNCTION
   ;;
   ;; #:def-uuid-type-definer                             ; MACRO
   ;; #:def-uuid-unsigned-byte-size                       ; MACRO
   ;; #:def-uuid-byte-array-length                        ; MACRO
   ;; #:def-uuid-unsigned-byte-integer-length             ; MACRO
   ;; #:def-uuid-bit-vector-N-type                        ; MACRO
   ;; #:def-uuid-bit-vector-length-type                   ; MACRO
   ;; #:def-uuid-uuid-hex-string-length                   ; MACRO
   ;; #:def-uuid-type-predicate-definer                   ; MACRO
   ;; #:def-uuid-type-check-definer                       ; MACRO
   ;; #:def-uuid-predicate-and-type-check-definer         ; MACRO
   ;; #:def-uuid-request-integer-bit-vector               ; MACRO
   ;;
   ;; #:def-uuid-bit-vector-zeroed                        ; MACRO
   ;; #:uuid-string-parse-integer                         ; MACRO
   ;; #:uuid-svref-for-parse-integer                      ; MACRO
   ;; #:def-indexed-hexstring-integer-parser              ; MACRO
   ;;
   ;; unicly/unicly-types.lisp
   ;;
   ;; #:uuid-unsigned-byte-size         ; SIMPLE-TYPE
   ;; #:uuid-ub128                      ; SIMPLE-TYPE
   ;; #:uuid-ub64                       ; SIMPLE-TYPE
   ;; #:uuid-ub48                       ; SIMPLE-TYPE
   ;; #:uuid-ub32                       ; SIMPLE-TYPE
   ;; #:uuid-ub24                       ; SIMPLE-TYPE
   ;; #:uuid-ub16                       ; SIMPLE-TYPE
   ;; #:uuid-ub8                        ; SIMPLE-TYPE
   ;; #:uuid-ub128-integer-length       ; SIMPLE-TYPE  
   ;; #:uuid-ub64-integer-length        ; SIMPLE-TYPE
   ;; #:uuid-ub48-integer-length        ; SIMPLE-TYPE
   ;; #:uuid-ub32-integer-length        ; SIMPLE-TYPE
   ;; #:uuid-ub24-integer-length        ; SIMPLE-TYPE
   ;; #:uuid-ub16-integer-length        ; SIMPLE-TYPE
   ;; #:uuid-ub8-integer-length         ; SIMPLE-TYPE
   ;; #:uuid-version-int                ; SIMPLE-TYPE
   ;; #:uuid-v3or5-int                  ; SIMPLE-TYPE
   ;; #:uuid-bit-vector                 ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-128             ; COMPLEX-TYPE   
   ;; #:uuid-bit-vector-48              ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-32              ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-16              ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-8               ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-length          ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-128-length      ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-48-length       ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-32-length       ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-16-length       ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-8-length        ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-valid-length    ; SIMPLE-TYPE
   ;; #:uuid-bit-vector-valid-bit-offset ;COMPLEX-TYPE
   ;; #:uuid-bit-vector-valid-bit-width ; COMPLEX-TYPE
   ;; #:uuid-simple-vector-5            : COMPLEX-TYPE
   ;; #:uuid-byte-array-16              ; COMPLEX-TYPE
   ;; #:uuid-byte-array-20              ; COMPLEX-TYPE
   ;; #:uuid-byte-string                ; COMPLEX-TYPE
   ;; #:uuid-byte-array-null            ; COMPLEX-TYPE
   ;; #:uuid-bit-vector-null            ; COMPLEX-TYPE 
   ;; #:uuid-string-32                  ; COMPLEX-TYPE
   ;; #:uuid-string-36                  ; COMPLEX-TYPE
   ;; #:uuid-hex-string-32              ; COMPLEX-TYPE
   ;; #:uuid-hex-string-36              ; SIMPLE-TYPE
   ;; #:uuid-hex-string-length          ; SIMPLE-TYPE
   ;; #:uuid-hex-string-12              ; SIMPLE-TYPE
   ;; #:uuid-hex-string-8               ; SIMPLE-TYPE
   ;; #:uuid-hex-string-4               ; SIMPLE-TYPE
   ;; #:uuid-hex-string-2               ; SIMPLE-TYPE
   ;;
   #:uuid-bit-vector-128-p
   ;; #:uuid-bit-vector-48-p
   ;; #:uuid-bit-vector-32-p
   ;; #:uuid-bit-vector-16-p
   ;; #:uuid-bit-vector-8-p
   ;; #:uuid-bit-vector-128-check-type
   ;; #:uuid-bit-vector-48-check-type
   ;; #:uuid-bit-vector-32-check-type
   ;; #:uuid-bit-vector-16-check-type
   ;; #:uuid-bit-vector-8-check-type
   ;;
   ;; #:uuid-simple-vector-5-p
   ;; #:uuid-simple-vector-5-check-type
   #:uuid-string-32-p
   ;; #:uuid-string-32-check-type
   #:uuid-string-36-p
   ;; #:uuid-string-36-check-type
   ;; #:uuid-byte-array-p
   ;; #:uuid-byte-array-check-type
   #:uuid-byte-array-16-p
   ;; #:uuid-byte-array-16-check-type
   ;; #:uuid-byte-array-20-p
   ;; #:uuid-byte-array-20-check-type
   #:uuid-byte-string-p
   ;; #:uuid-byte-string-check-type
   #:uuid-byte-array-null-p
   ;; #:uuid-byte-array-null-check-type
   ;; 
   ;;
   ;; #:string-with-fill-pointer-p
   ;; #:string-with-fill-pointer-check-type
   ;;
   ;; #:uuid-delimited-string-36-p
   ;;
   ;; #:%uuid-hex-string-36-null-string-p
   #:uuid-hex-string-32-p
   #:uuid-hex-string-36-p
   ;;
   ;;
 ;; unicly/unicly-class.lisp   
   ;;
   #:unique-universal-identifier       ; <CLASS>
   #:unique-universal-identifier-p     ; <GENERIC>
   #:uuid-eql                          ; <GENERIC>
   #:uuid-print-bit-vector             ; <GENERIC>
   #:uuid-print-bytes-to-string        ; <GENERIC>
   #:uuid-princ-to-string              ; <GENERIC> 
   #:uuid-copy-uuid
   #:unique-universal-identifier-null-p
   ;; 
   ;; #:%unique-universal-identifier-null-p
   ;; #:%make-null-uuid-loadtime
   ;; #:unique-universal-identifier-null
   ;; #:%verify-slot-boundp-and-type
   ;; #:uuid-print-bytes                     ; <GENERIC>-INTERNAL
   ;;
 ;; unicly/unicly-conditions.lisp
   ;;
   ;; #:uuid-error                     ; CONDITION
   ;; #:uuid-simple-error              ; CONDITION
   ;; #:uuid-slot-unbound-error        ; CONDITION
   ;; #:uuid-simple-type-error         ; CONDITION & FUNCTION
   ;; #:uuid-bit-48-error              ; CONDITION
   ;;
 ;; unicly/unicly-integers.lisp
   ;;
   ;; #:uuid-request-integer
   ;; #:uuid-disassemble-ub48
   ;; #:uuid-disassemble-ub32
   ;; #:uuid-disassemble-ub16
   ;; #:uuid-assemble-ub48
   ;; #:uuid-assemble-ub32
   ;; #:uuid-assemble-ub16
   ;;
 ;; unicly/unicly-bit-vectors.lisp
   ;;
   ;; #:uuid-bit-vector-128-zeroed
   ;; #:uuid-bit-vector-48-zeroed
   ;; #:uuid-bit-vector-32-zeroed
   ;; #:uuid-bit-vector-16-zeroed
   ;; #:uuid-bit-vector-8-zeroed
   ;;
   ;; #:%uuid_time-low-request-bit-vector
   ;; #:%uuid_time-mid-request-bit-vector
   ;; #:%uuid_time-high-and-version-request-bit-vector
   ;; #:%uuid_clock-seq-and-reserved-request-bit-vector
   ;; #:%uuid_clock-seq-low-request-bit-vector
   ;; #:%uuid_node-request-bit-vector
   ;;
   ;; #:%uuid-version-bit-vector-if
   ;; #:uuid-version-bit-vector
   #:uuid-bit-vector-v3-p
   #:uuid-bit-vector-v4-p
   #:uuid-bit-vector-v5-p
   ;;
   ;; #:uuid-octet-to-bit-vector-8
   ;; #:uuid-bit-vector-to-integer
   ;; #:uuid-integer-128-to-bit-vector
   ;; #:uuid-deposit-octet-to-bit-vector
   ;; 
   #:uuid-bit-vector-to-byte-array
   #:uuid-byte-array-to-bit-vector
   ;;
   #:uuid-bit-vector-eql
   #:uuid-bit-vector-null-p
   ;;
   #:uuid-from-bit-vector
   #:uuid-to-bit-vector
   ;;
 ;; unicly/unicly-byte-arrays.lisp
   ;;
   #:uuid-get-namespace-bytes
   #:uuid-byte-array-16-zeroed   
   #:uuid-from-byte-array
   #:uuid-byte-array-16-to-integer
   ;;
   ;;
   ;;
 ;; unicly/unicly.lisp
   ;;
   #:make-v3-uuid
   #:make-v4-uuid
   #:make-v5-uuid
   #:make-null-uuid
   #:uuid-as-urn-string
   #:uuid-version-uuid
   ;;
   ;; #:%uuid_time-mid-request
   ;; #:%uuid_time-low-request
   ;; #:%uuid_time-high-and-version-request
   ;; #:%uuid_clock-seq-and-reserved-request
   ;; #:%uuid_clock-seq-low-request
   ;; #:%uuid_node-request
   ;; #:uuid-digest-uuid-instance
   ;; #:%uuid-digest-uuid-instance-md5
   ;; #:%uuid-digest-uuid-instance-sha1
   ;; #:%verify-digest-version
   ;; #:%verify-version-3-or-5
   ;; #:digested-v3or5-uuid
   ;; #:digested-v3-uuid
   ;; #:digested-v5-uuid
   ;;
   ;; #:uuid-get-bytes-for-integer           ; DEPRECATED
   ;; #:uuid-integer-length                  ; DEPRECATED
   ;; #:uuid-digest-uuid-string              ; DEPRECATED
   ;; #:uuid-get-bytes                       ; DEPRECATED
   ;; #:uuid-load-bytes                      ; DEPRECATED
   ;; #:%uuid-get-bytes-if                   ; DEPRECATED
   ;; #:uuid-to-byte-array                   ; DEPRECATED
   ;; #:uuid-number-to-byte-array            ; DEPRECATED
   ;; 
   ;;
 ;; unicly/unicly-string-uuid.lisp
   ;;
   #:make-uuid-from-string
   ;; #:make-uuid-from-string-if
   ;; #:uuid-hex-vector-parse-time-low
   ;; #:uuid-hex-vector-parse-time-mid
   ;; #:uuid-hex-vector-parse-time-high-and-version
   ;; #:uuid-hex-vector-parse-clock-seq-and-reserved
   ;; #:uuid-hex-vector-parse-clock-seq-low
   ;; #:uuid-hex-vector-parse-node
   ;; #:uuid-string-to-sha1-byte-array       ; DEPRECATED
   ;;
   ;;
 ;; unicly/unicly-hash-table.lisp
   ;;
   #:sxhash-uuid
   #:make-hash-table-uuid
   ;;
 ;; unicly/unicly-io.lisp
   ;;
   ;; #:uuid-valid-stream-p
   ;; #:uuid-valid-stream-verify-for-output
   ;; #:uuid-valid-stream-verify-for-input
   ;; #:uuid-valid-stream-verify-for-output
   ;;
   ;; #:uuid-serialize-byte-array-bytes
   ;; #:uuid-serialize-bit-vector-bits
   ;; #:uuid-deserialize-bit-vector-bits
   ;; #:uuid-write-bit-vector-bits
   ;; #:uuid-read-bit-vector-bits
   ;;
 ;; unicly/unicly-utils.lisp
   ;;
   ;; #:string-all-hex-char-p
   ;; #:string-not-null-or-empty-p
   ;; #:hexadecimal-char-p
   ;; #:type-specifier-p
   ;; #:doc-set
   ;; #:fundoc
   ;; #:vardoc
   ;; #:typedoc
   ;; #:string-or-null                     ; TYPE   
   ;; #:hexadecimal-char                   ; TYPE
   ;; #:not-null                           ; TYPE
   ;; #:string-not-null-or-empty           ; TYPE
   ;; #:string-not-empty                   ; TYPE
   ;; #:string-empty                       ; TYPE
   ;; #:string-or-null                     ; TYPE
   ;; #:*hexadecimal-chars*                ; VARIABLE
   ;;
 ;; unicly/unicly-compat.lisp
   ;;
   ;; #:*clock-seq-uuid*
   ;; #:*node-uuid*
   ;; #:*ticks-per-count-uuid*
   ;; #:get-node-id
   ;; #:get-timestamp-uuid
   ;; #:make-v1-uuid
   ))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
