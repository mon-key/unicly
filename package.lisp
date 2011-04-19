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
   ;;
   #:uuid-byte-string                   ; TYPE
   #:uuid-ub48                          ; TYPE
   #:uuid-ub32                          ; TYPE
   #:uuid-ub24                          ; TYPE
   #:uuid-ub16                          ; TYPE
   #:uuid-ub8                           ; TYPE
   #:uuid-bit-vector                    ; TYPE
   #:uuid-bit-vector-8                  ; TYPE
   #:uuid-bit-vector-128                ; TYPE
   #:uuid-byte-array-16                 ; TYPE
   #:uuid-byte-array-20                 ; TYPE
   #:uuid-string-32                     ; TYPE
   #:uuid-string-36                     ; TYPE
   #:uuid-hex-string-32                 ; TYPE
   #:uuid-hex-string-36                 ; TYPE
   ;;
   #:uuid-byte-array-16-p
   #:uuid-byte-array-20-p
   #:uuid-byte-string-p
   #:uuid-string-32-p
   #:uuid-string-36-p
   #:uuid-hex-string-32-p
   #:uuid-hex-string-36-p
   #:unique-universal-identifier-p
   #:uuid-eql
   #:uuid-bit-vector-eql
   ;;
   #:unique-universal-identifier
   #:make-v3-uuid
   #:make-v4-uuid
   #:make-v5-uuid
   #:make-null-uuid
   #:make-uuid-from-string
   #:uuid-copy-uuid
   #:serialize-uuid
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
   #:uuid-bit-vector-8-zeroed
   #:uuid-octet-to-bit-vector-8
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
