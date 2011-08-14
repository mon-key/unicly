;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:39:23-04:00Z}#{11154} - by MON>
;;; :FILE unicly/package.lisp
;;; ==============================

;; (in-package #:unicly) ;; for slime
;; *package*

(defpackage #:unicly
  (:use #:common-lisp)
  (:export 
   ;;
   #:*uuid-namespace-dns* 
   #:*uuid-namespace-url* 
   #:*uuid-namespace-oid* 
   #:*uuid-namespace-x500*
   ;; #:*uuid-null-uuid*
   ;;
   #:uuid-byte-string                   ; TYPE
   #:uuid-unsigned-byte-size            ; TYPE
   #:uuid-ub128                         ; TYPE
   #:uuid-ub64                          ; TYPE
   #:uuid-ub48                          ; TYPE
   #:uuid-ub32                          ; TYPE
   #:uuid-ub24                          ; TYPE
   #:uuid-ub16                          ; TYPE
   #:uuid-ub8                           ; TYPE
   #:uuid-ub128-integer-length          ; TYPE  
   #:uuid-ub64-integer-length           ; TYPE
   #:uuid-ub48-integer-length           ; TYPE
   #:uuid-ub32-integer-length           ; TYPE
   #:uuid-ub24-integer-length           ; TYPE
   #:uuid-ub16-integer-length           ; TYPE
   #:uuid-ub8-integer-length            ; TYPE
   #:uuid-bit-vector                    ; TYPE
   #:uuid-bit-vector-128                ; TYPE   
   #:uuid-bit-vector-48                 ; TYPE
   #:uuid-bit-vector-32                 ; TYPE
   #:uuid-bit-vector-16                 ; TYPE
   #:uuid-bit-vector-8                  ; TYPE
   #:uuid-bit-vector-length             ; TYPE
   #:uuid-bit-vector-128-length         ; TYPE
   #:uuid-bit-vector-48-length          ; TYPE
   #:uuid-bit-vector-32-length          ; TYPE
   #:uuid-bit-vector-16-length          ; TYPE
   #:uuid-bit-vector-8-length           ; TYPE
   #:uuid-bit-vector-valid-length       ; TYPE
   #:uuid-byte-array-16                 ; TYPE
   #:uuid-byte-array-20                 ; TYPE
   #:uuid-byte-array-null               ; TYPE
   #:uuid-bit-vector-null               ; TYPE 
   #:uuid-string-32                     ; TYPE
   #:uuid-string-36                     ; TYPE
   #:uuid-hex-string-32                 ; TYPE
   #:uuid-hex-string-36                 ; TYPE
   ;;
   #:uuid-bit-vector-128-p
   #:uuid-byte-array-16-p
   #:uuid-byte-array-20-p
   #:uuid-bit-vector-null-p
   #:uuid-byte-array-null-p
   #:uuid-byte-string-p
   #:uuid-string-32-p
   #:uuid-string-36-p
   #:uuid-hex-string-32-p
   #:uuid-hex-string-36-p
   #:unique-universal-identifier-p
   #:uuid-eql
   #:uuid-bit-vector-eql
   ;;
   ;; #:uuid-error                     ; CONDITION
   ;; #:uuid-simple-error              ; CONDITION
   ;; #:uuid-slot-unbound-error        ; CONDITION
   ;; #:uuid-bit-48-error              ; CONDITION
   ;;
   #:unique-universal-identifier
   #:make-v3-uuid
   #:make-v4-uuid
   #:make-v5-uuid
   #:make-null-uuid
   ;; #:%unique-universal-identifier-null-p
   ;; #:%make-null-uuid-loadtime
   ;; #:unique-universal-identifier-null
   #:make-uuid-from-string
   #:uuid-copy-uuid
   #:serialize-uuid
   ;; #:deserialize-uuid                 ;(UNIMPLEMENTED)
   #:sxhash-uuid
   #:make-hash-table-uuid
   #:uuid-print-bit-vector
   #:uuid-princ-to-string
   #:uuid-print-bytes-to-string
   #:uuid-as-urn-string
   #:uuid-get-bytes-for-integer
   #:uuid-get-namespace-bytes
   #:uuid-string-to-sha1-byte-array
   #:uuid-bit-vector-zeroed
   #:uuid-byte-array-zeroed
   #:uuid-bit-vector-8-zeroed
   #:uuid-octet-to-bit-vector-8
   #:uuid-bit-vector-48-zeroed 
   #:uuid-bit-vector-32-zeroed 
   #:uuid-bit-vector-16-zeroed
   #:uuid-bit-vector-8-zeroed  
   #:uuid-byte-array-to-bit-vector
   #:uuid-to-bit-vector
   #:uuid-from-byte-array
   #:uuid-deposit-octet-to-bit-vector
   #:uuid-bit-vector-version
   #:uuid-version
   #:uuid-bit-vector-v3-p
   #:uuid-bit-vector-v4-p
   #:uuid-bit-vector-v5-p
   ;; #:*random-state-uuid*                  ; INTERNAL
   ;; #:make-uuid-from-string-if             ; INTERNAL
   ;; #:%verify-slot-boundp-and-type         ; INTERNAL
   ;; #:%uuid_time-mid-request               ; INTERNAL
   ;; #:%uuid_time-low-request               ; INTERNAL
   ;; #:%uuid_time-high-and-version-request  ; INTERNAL
   ;; #:%uuid_clock-seq-and-reserved-request ; INTERNAL
   ;; #:%uuid_clock-seq-low-request          ; INTERNAL
   ;; #:%uuid_node-request                   ; INTERNAL
   ;; #:uuid-request-integer                 ; INTERNAL
   ;; #:uuid-digest-uuid-instance            ; INTERNAL
   ;; #:%uuid-digest-uuid-instance-md5       ; INTERNAL
   ;; #:%uuid-digest-uuid-instance-sha1      ; INTERNAL
   ;; #:%verify-digest-version               ; INTERNAL
   ;; #:%verify-version-3-or-5               ; INTERNAL
   ;; #:digested-v3or5-uuid                  ; INTERNAL
   ;; #:digested-v3-uuid                     ; INTERNAL
   ;; #:digested-v5-uuid                     ; INTERNAL
   ;; #:uuid-string-parse-integer            ; INTERNAL MACRO
   ;; #:uuid-disassemble-ub32                ; INTERNAL
   ;; #:uuid-disassemble-ub16                ; INTERNAL
   ;; #:uuid-disassemble-ub48                ; INTERNAL
   ;;
   ;; #:uuid-integer-length                  ; DEPRECATED
   ;; #:uuid-digest-uuid-string              ; INTERNAL DEPRECATED
   ;; #:uuid-get-bytes                       ; INTERNAL DEPRECATED
   ;; #:uuid-load-bytes                      ; INTERNAL DEPRECATED
   ;; #:%uuid-get-bytes-if                   ; INTERNAL DEPRECATED
   ;; #:uuid-to-byte-array                   ; INTERNAL DEPRECATED
   ;; #:uuid-number-to-byte-array            ; INTERNAL DEPRECATED
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
