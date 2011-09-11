;;; :FILE-CREATED <Timestamp: #{2011-05-27T17:07:12-04:00Z}#{11215} - by MON>
;;; :FILE unicly/unicly-conditions.lisp
;;; ==============================

;;; ==============================
;; :UNICLY-CONDITION-HEIRARCHY
;;
;; uuid-error               (error)
;; uuid-simple-error        (uuid-error simple-error)
;; uuid-slot-type-error     (uuid-error type-error)
;; uuid-slot-unbound-error  (uuid-error)
;; uuid-bit-48-error        (uuid-error)
;;
;;; ==============================


(in-package #:unicly)
;; *package*

(define-condition uuid-error (error)
  ()
  (:documentation "Superclass for all errors related to unicly."))

(define-condition uuid-simple-error (uuid-error simple-error)
  ()
  (:default-initargs :format-control "UUID-SIMPLE-ERROR" 
                     :format-arguments nil)
  (:documentation "Conditions for simple unicly errors."))

;; :NOTE Unused
(define-condition uuid-slot-type-error (uuid-error type-error)
  ()
  (:documentation "Conditions for unicly slot-type errors."))

(define-condition uuid-simple-type-error (uuid-simple-error type-error)
  ()
  (:report (lambda (uste-condition uste-stream)
             (declare (condition uste-condition) (stream uste-stream))
             (let* ((uste-datum         (type-error-datum uste-condition))
                    (uste-type-of       (type-of uste-datum))
                    (uste-expect        (type-error-expected-type uste-condition))
                    (uste-fmt-and-args  (list
                                         (simple-condition-format-control condition)
                                         uste-datum uste-type-of uste-expect)))
               (apply #'format uste-stream uste-fmt-and-args))))
  (:default-initargs :format-control "UUID-SIMPLE-TYPE-ERROR~%~Tgot-val: ~S~%~Ttype-of:  ~S~%~Texpected: ~S~T")
  (:documentation 
   #.(format nil "Conditions for failed Unicly type declarations.~%~@
                  Keyword FORMAT-CONTROL has a default-initarg which displays as:~%~% ~
                  UUID-SIMPLE-TYPE-ERROR~%~Tgot-val: <GOT>~%~Ttype-of: <DATUM-TYPE-OF>~%~Texpected: <EXPECTED-TYPE>~%~%~
                  Do not override FORMAT-CONTROL by providing an alternative argument as keyword~%~
                  FORMAT-ARGUMENTS is ignored.~%~@
                  Keywords DATUM and EXPECTED-TYPE are as per condition class CL:TYPE-ERROR.~%~@
                  Convenience function `uuid-simple-type-error' is provided for signalling this condition.~%~@
                  :EXAMPLE~%~% ~
                 \(error 'uuid-simple-type-error :datum \"bubba\" :expected-type 'simple-bit-vector\)~%
                 \(uuid-simple-type-error :datum \"bubba\" :expected-type 'simple-bit-vector\)~%")))
;;
(defun uuid-simple-type-error (&key datum expected-type)
  (declare (optimize (speed 3)))
  (error 'uuid-simple-type-error :datum datum :expected-type expected-type))

(define-condition uuid-slot-unbound-error (uuid-error)
  ((uuid-slot-unbound-name
    :initarg :uuid-slot-unbound-name
    :reader uuid-slot-unbound-name)
   (uuid-slot-unbound-object
    :initarg :uuid-slot-unbound-object
    :reader uuid-slot-unbound-object))
  (:report (lambda (condition stream)
             (let* ((uuid-obj (uuid-slot-unbound-object condition))
                    (uuid-obj-type (or (and uuid-obj 
                                            (type-of uuid-obj))
                                       (string 'UNIQUE-UNIVERSAL-IDENTIFIER)))
                    (uuid-obj-slot (or (uuid-slot-unbound-name condition) 
                                       (make-string 0 :initial-element #\NUL))))
               (format stream "UUID object has slot not `cl:slot-boundp'~%~T~
                               OBJECT-TYPE: ~S~%~T~
                               OBJECT-SLOT: ~A~%"
                       uuid-obj-type uuid-obj-slot))))
  (:documentation 
   #.(format nil
   "Condition for `unicly:unique-universal-identifier' objects with unbound slots.~%~@
UUID-SLOT-UNBOUND-NAME is a symbol naming the unbound slot in object.~%~@
UUID-SLOT-UNBOUND-OBJECT is the uuid instance of with the unbound slot.~%~@
:EXAMPLE~%
 \(let \(\(v4uuid \(make-v4-uuid\)\)\)
   \(slot-makunbound v4uuid '%uuid_time-high-and-version\)
   \(error 'uuid-slot-unbound-error 
          :uuid-slot-unbound-name '%uuid_time-high-and-version
          :uuid-slot-unbound-object v4uuid\)\)~%~@
:SEE-ALSO `<XREF>'.~%")))

(define-condition uuid-bit-48-error (uuid-error)
  ((uuid-bit-48-error-datum 
    :initarg :uuid-bit-48-error-datum
    :reader  uuid-bit-48-error-datum)
   (uuid-bit-48-error-expected-type
    :initform '(mod 1)
    :reader uuid-bit-48-error-expected-type))
  (:report (lambda (condition stream)
             (declare (type stream stream)
                      (type condition condition))
             (let* ((bv-or-uuid (uuid-bit-48-error-datum condition))
                    (bv-or-uuid-fmt 
                     (etypecase bv-or-uuid
                       ;; The use of cl:type-of in the first case clause below
                       ;; is to account for situations where user code has
                       ;; subclassed `unicly:unique-universal-identifier' b/c
                       ;; the cl:print-object method specialized on it does not
                       ;; evaluate cl:print-unreadable-object, as such there is
                       ;; no :type <TYPE> value for print-object to convey and
                       ;; our report around uuid-bit-48-error-datum may not
                       ;; carry the correct type info for the printed object
                       ;; were reporting.
                       (unique-universal-identifier 
                        (format nil "got uuid object of type ~S:~%~4@T~S" 
                                (type-of bv-or-uuid) bv-or-uuid))
                       ;; :TODO
                       ;; (uuid-byte-array-16 (cons 'uuid-byte-array-16 (...)) 
                       ;;
                       (uuid-bit-vector-128 
                        (format nil "got UUID-BIT-VECTOR-128 with value of subseq [48,63]:~%~4@T~S"
                                (subseq bv-or-uuid 48 63)))))
                    (fmt-cntl 
                     (format nil "UUID object with bit 48 not `cl:zerop'~%~T~
                                  per RFC4122 section 4.1.3 it should always be of type: ~S~%~T~
                                  ~A~%"
                             (uuid-bit-48-error-expected-type condition) bv-or-uuid-fmt)))
               (format stream fmt-cntl))))
  (:documentation 
   #.(format nil
             "Condition for uuid related objects whose bit 48 is not `cl:zerop'.~%~@
UUID-BIT-48-ERROR-DATUM is the object of the offending error.~%~% ~
 - When it is of type `unicly:unique-universal-identifier' this indicates that~%   ~
   Msb0 \(byte 1 15\) of the '%uuid_time-high-and-version slot-value is 1.~%~% ~
 - When it is of type `unicly:uuid-bit-vector-128' this indicates that~%   ~
   \(sbit UUID-BIT-48-ERROR-DATUM 48\) is 1.~%~@
UUID-BIT-48-ERROR-EXPECTED-TYPE is a type specifier, it is defaulted to (mod 1).~%~@
:EXAMPLE~%~% ~
 \(error \(make-condition 'uuid-bit-48-error :uuid-bit-48-error-datum \(make-v4-uuid\)\)\)~%~% ~
 \(error \(make-condition 'uuid-bit-48-error :uuid-bit-48-error-datum \(uuid-bit-vector-128-zeroed\)\)\)~%~% ~
 \(error \(make-condition 'uuid-bit-48-error :uuid-bit-48-error-datum \(make-null-uuid\)\)\)~%~%~
:NOTE Above examples are for illustrative purposes only, the reported objects
are in fact valid. Following examples are for legitimately invalid objects.~%~% ~
 \(let \(\(v4uuid \(make-v4-uuid\)\)\)
   \(setf \(slot-value v4uuid '%uuid_time-high-and-version\) #xFFFF\)
   \(%uuid-version-uuid-if \(slot-value v4uuid '%uuid_time-high-and-version\) v4uuid\)\)~%
 \(let* \(\(v4uuid \(make-v4-uuid\)\)
        \(v4-bv  \(uuid-to-bit-vector v4uuid\)\)\)
   \(setf \(sbit v4-bv 48\) 1\)
   \(%uuid-version-bit-vector-if v4-bv\)\)~%~@
:NOTE Intended callers of this condition are internal unicly dispatching functions:~% ~
 - `unicly::%uuid-version-uuid-if'
 - `unicly::%uuid-version-bit-vector-if'~%~@
Under normal circumstances, neither of these functions likely to ever siganl
so long as their arguments are uuid objects properly instantiated via the exposed
unicly API, e.g. with `unicly:make-v[345]-uuid'.  However, it is possible for an
abstract representation of a uuid to be corrupt such that uuid objects
instantiated from them by way of:~% ~
 - `unicly:make-uuid-from-string'
 - `unicly:uuid-from-byte-array'
 - `unicly:uuid-from-bit-vector'~%~@
Where the resultant uuid could be faulty w/r/t its uuid version we should identify it.~%~@
Of more concern are attempts to migrate uuid objects with origins from
the equivalent UUID API esp. where it may create spurious uuid objects by way of
`uuid:make-v1-uuid' which sets the wrong bits of the class uuid's
time-high-and-version slot-value.  It is wrong to propogate the errors of that implementations
API further and we make some attempt to identify them.~%~@
:SEE-ALSO `uuid-version-uuid', `uuid-version-bit-vector'.~%")))




;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
