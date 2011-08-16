;;; :FILE-CREATED <Timestamp: #{2011-08-16T12:11:33-04:00Z}#{11332} - by MON>
;;; :FILE unicly/unicly-macros.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

(defun %def-uuid-format-and-intern-symbol (format-string format-arg)
  (let ((generated-name (format nil (string-upcase format-string) format-arg)))
    (intern generated-name (find-package "UNICLY"))))

;;; ==============================
;;; Following macros expanded in :FILE unicly/unicly-types.lisp
;;; `def-uuid-type-definer', `def-uuid-unsigned-byte-size'
;;; `def-uuid-byte-array-length', `def-uuid-unsigned-byte-integer-length',
;;; `def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
;;; `def-uuid-uuid-hex-string-length',
;;;
(defmacro def-uuid-type-definer (parent-type format-string length-arg)
  ;;  (macroexpand-1 '(def-uuid-type-definer uuid-bit-vector "UUID-BIT-VECTOR-~D" 128))
  (let ((definer-interned-name (%def-uuid-format-and-intern-symbol format-string length-arg)))
    `(deftype ,definer-interned-name ()
      '(,parent-type ,length-arg))))
;;
(defmacro def-uuid-unsigned-byte-size (size-bytes)
  ;; (macroexpand-1 '(def-uuid-unsigned-byte-size 128))
  `(def-uuid-type-definer uuid-unsigned-byte-size "UUID-UB~D" ,size-bytes))
;;
(defmacro def-uuid-byte-array-length (byte-array-length)
  ;; (macroexpand-1 '(def-uuid-unsigned-byte-size 128))
  `(def-uuid-type-definer uuid-byte-array "UUID-BYTE-ARRAY-~D" ,byte-array-length))
;;
(defmacro def-uuid-unsigned-byte-integer-length (unsigned-length)
  ;; (macroexpand-1 '(def-uuid-unsigned-byte-integer-length 128))
  ;; :NOTE We have to generate the symbol name first b/c
  ;; `uuid-unsigned-byte-integer-length' is specified as: (mod
  ;; <UNSIGNED-LENGTH>) but if the macro expands (1+ unsigned-length) the we
  ;; will get symbol-names like `uuid-ub129-integer-length' instead of
  ;; `uuid-ub128-integer-length'
  (let ((generated-name-string (format nil "UUID-UB~D-INTEGER-LENGTH" unsigned-length)))
    `(def-uuid-type-definer uuid-unsigned-byte-integer-length ,generated-name-string ,(1+ unsigned-length))))
;;
(defmacro def-uuid-bit-vector-N-type (bv-length-type)
  ;; (macroexpand-1 '(def-uuid-bit-vector-N-type 16))
  ;; (let ((interned-name (uuid-format-and-intern-symbol "UUID-BIT-VECTOR-~D" bv-length-type)))
  ;;   `(deftype ,interned-name ()
  ;;      '(uuid-bit-vector ,bv-length-type)))
  `(def-uuid-type-definer uuid-bit-vector "UUID-BIT-VECTOR-~D" ,bv-length-type))
;;
(defmacro def-uuid-bit-vector-length-type (bv-length)
  ;; (macroexpand-1 '(def-uuid-bit-vector-length-type 16))
  ;; (let ((interned-name (uuid-format-and-intern-symbol "UUID-BIT-VECTOR-~D-LENGTH" bv-length)))
  ;;   `(deftype ,interned-name ()
  ;;      '(uuid-bit-vector-length ,bv-length))))
  `(def-uuid-type-definer uuid-bit-vector-length "UUID-BIT-VECTOR-~D-LENGTH" ,bv-length))
;;
(defmacro def-uuid-uuid-hex-string-length (hex-string-length)
  ;; (macroexpand '(def-uuid-uuid-hex-string-length 12))
  `(def-uuid-type-definer uuid-hex-string-length "UUID-HEX-STRING-~D" ,hex-string-length))
;;
;;; ==============================


;;; ==============================
;;; Following macros expanded in :FILE unicly/unicly-bit-vectors.lisp
;;; `declared-uuid-array-zeroed-of-size',
;;; `declared-uuid-bit-vector-zeroed-of-size', `def-uuid-bit-vector-zeroed'
;;
(defmacro declared-uuid-array-zeroed-of-size (size array-type array-elt-type)
  ;; (macroexpand-1 '(declared-uuid-array-zeroed-of-size 16 uuid-byte-array-16 uuid-ub8))
  `(the ,array-type
     (make-array ,size :element-type ',array-elt-type :initial-element 0)))
;;
(defmacro declared-uuid-bit-vector-zeroed-of-size (size bit-vector-type)
  ;; (macroexpand-1 '(declared-uuid-bit-vector-zeroed-of-size 16 uuid-bit-vector-16))
  ;; `(the ,bit-vector-type (make-array ,size :element-type 'bit :initial-element 0))
  `(declared-uuid-array-zeroed-of-size ,size ,bit-vector-type bit))
;;
(defmacro def-uuid-bit-vector-zeroed (name size)
  `(defun ,name ()
     (uuid-bit-vector-zeroed-of-size ,size)))
;;
;; (defmacro @uuid-bit-vector (bit-vector-type bit-vector index)
;;   `(sbit (the ,bit-vector-type ,bit-vector) ,index))
;;
;;; ==============================


;;; ==============================
;;; Following macros expanded in :FILE unicly/unicly-class.lisp
;;; `uuid-string-parse-integer', `uuid-svref-for-parse-integer', `def-indexed-hexstring-integer-parser'
;;;
(defmacro uuid-string-parse-integer (uuid-hex-string start end integer-type)
  ;; (macroexpand-1 '(uuid-string-parse-integer "6ba7b810-9dad-11d1-80b4-00c04fd430c8" 0 8 uuid-ub32))
  `(the ,integer-type (parse-integer ,uuid-hex-string :start ,start :end ,end :radix 16)))
;;
;;; ==============================
;; (uuid-svref-for-parse-integer <VECTOR> <INDEX> <STRING-TYPE>)
;;
;; (macroexpand-1
;;  '(uuid-svref-for-parse-integer
;;    (nth-value 1 (uuid-hex-string-36-p (uuid-princ-to-string (make-v4-uuid))))
;;    4 uuid-hex-string-12))
(defmacro uuid-svref-for-parse-integer (simple-vector-5 index string-type)
  `(the ,string-type (svref (the uuid-simple-vector-5 ,simple-vector-5) ,index)))
;;
;;; ==============================
;; (uuid-string-parse-integer 
;;  (uuid-svref-for-parse-integer <VECTOR> <INDEX> <STRING-TYPE>)
;;  <START> <END> <INTEGER-TYPE> )
;;
;; (macroexpand '(def-indexed-hexstring-integer-parser
;;                  uuid-hex-vector-parse-time-low
;;                  0
;;                  uuid-hex-string-8
;;                  0 8 uuid-ub32))
(defmacro def-indexed-hexstring-integer-parser (fun-name vec-index string-type-at-index string-start string-end string-integer-type)
  (let ((generated-fun-name (%def-uuid-format-and-intern-symbol "UUID-HEX-VECTOR-PARSE-~@:(~A~)" fun-name)))
    ;; `(defun ,fun-name (hex-vector-5)
    `(defun ,generated-fun-name (hex-vector-5)
       (declare (uuid-simple-vector-5 hex-vector-5)
                (optimize (speed 3)))
       (uuid-string-parse-integer 
        (uuid-svref-for-parse-integer hex-vector-5 ,vec-index ,string-type-at-index)
        ,string-start ,string-end ,string-integer-type))))
;;
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
