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
   ;; #:*uuid-null-uuid*                ; Not exported, Bound at loadtime with `make-null-uuid-loadtime' 
   ;;                                   ; and treated as a special case thereafter
   ;;
   ;; unicly/unicly-types.lisp
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
   ;; #:uuid-simple-vector-5            : TYPE
   #:uuid-byte-array-16                 ; TYPE
   #:uuid-byte-array-20                 ; TYPE
   #:uuid-byte-array-null               ; TYPE
   #:uuid-bit-vector-null               ; TYPE 
   #:uuid-string-32                     ; TYPE
   #:uuid-string-36                     ; TYPE
   #:uuid-hex-string-32                 ; TYPE
   #:uuid-hex-string-36                 ; TYPE
   #:uuid-hex-string-length             ; TYPE
   #:uuid-hex-string-12                 ; TYPE
   #:uuid-hex-string-8                  ; TYPE
   #:uuid-hex-string-4                  ; TYPE
   #:uuid-hex-string-2                  ; TYPE
   ;;
   #:uuid-bit-vector-128-p              ; PREDICATE
   #:uuid-byte-array-16-p               ; PREDICATE
   #:uuid-byte-array-20-p               ; PREDICATE  
   #:uuid-byte-array-null-p             ; PREDICATE
   #:uuid-byte-string-p                 ; PREDICATE  
   #:uuid-string-32-p                   ; PREDICATE 
   #:uuid-string-36-p                   ; PREDICATE
   #:uuid-delimited-string-36-p         ; PREDICATE
   ;;
   ;; #:%uuid-hex-string-36-null-string-p ; INTERNAL-PREDICATE
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
   ;; #:%unique-universal-identifier-null-p  ; INTERNAL
   ;; #:%make-null-uuid-loadtime             ; INTERNAL
   ;; #:unique-universal-identifier-null     ; INTERNAL  
   ;; #:%verify-slot-boundp-and-type         ; INTERNAL
   ;; #:uuid-print-bytes                     ; <GENERIC>-INTERNA
   ;;
 ;; unicly/unicly-conditions.lisp
   ;;
   ;; #:uuid-error                     ; CONDITION
   ;; #:uuid-simple-error              ; CONDITION
   ;; #:uuid-slot-unbound-error        ; CONDITION
   ;; #:uuid-bit-48-error              ; CONDITION
   ;;
 ;; unicly/unicly-integers.lisp
   ;;

 ;; unicly/unicly-bit-vectors.lisp
   ;;
   ;; #:uuid-bit-vector-128-zeroed       ; INTERNAL
   ;; #:uuid-bit-vector-48-zeroed        ; INTERNAL
   ;; #:uuid-bit-vector-32-zeroed        ; INTERNAL
   ;; #:uuid-bit-vector-16-zeroed        ; INTERNAL
   ;; #:uuid-bit-vector-8-zeroed         ; INTERNAL
   ;;
   ;; %uuid-version-bit-vector-if        ; INTERNAL
   ;; #:uuid-version-bit-vector          ; INTERNAL
   #:uuid-bit-vector-v3-p
   #:uuid-bit-vector-v4-p
   #:uuid-bit-vector-v5-p
   ;;
   ;; #:uuid-octet-to-bit-vector-8       ; INTERNAL
   ;; #:uuid-bit-vector-to-integer
   #:uuid-deposit-octet-to-bit-vector
   ;;
   #:uuid-bit-vector-eql
   #:uuid-bit-vector-null-p
   ;;
   ;;
 ;; unicly.lisp
   ;;
   #:make-v3-uuid
   #:make-v4-uuid
   #:make-v5-uuid
   #:make-null-uuid
   ;;
   ;;
   ;; #:uuid-hex-vector-parse-time-low
   ;; #:uuid-hex-vector-parse-time-mid
   ;; #:uuid-hex-vector-parse-time-high-and-version
   ;; #:uuid-hex-vector-parse-clock-seq-and-reserved
   ;; #:uuid-hex-vector-parse-clock-seq-low
   ;; #:uuid-hex-vector-parse-node
   #:make-uuid-from-string
   #:serialize-uuid
   ;; #:deserialize-uuid                 ;(UNIMPLEMENTED)
   #:sxhash-uuid
   #:make-hash-table-uuid
   #:uuid-as-urn-string
   ;; #:uuid-get-bytes-for-integer        ; <DEPRECATED>
   #:uuid-get-namespace-bytes
   ; #:uuid-string-to-sha1-byte-array
   #:uuid-byte-array-zeroed
   #:uuid-byte-array-to-bit-vector
   #:uuid-to-bit-vector
   #:uuid-from-byte-array
   #:uuid-version-bit-vector
   #:uuid-version-uuid
   #:uuid-bit-vector-v3-p
   #:uuid-bit-vector-v4-p
   #:uuid-bit-vector-v5-p
   ;; #:*random-state-uuid*                  ; INTERNAL
   ;; #:make-uuid-from-string-if             ; INTERNAL
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
   ;;
 ;; unicly/unicly-hash-table.lisp
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
