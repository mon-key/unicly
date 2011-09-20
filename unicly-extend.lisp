;;; :FILE-CREATED <Timestamp: #{2011-09-15T19:19:50-04:00Z}#{11374} - by MON>
;;; :FILE unicly/unicly-extend.lisp
;;; ==============================

;;; ==============================
;; :NOTE The inteface defined here is experimental and subject to change!
;;
;; The current interface as defined below is not hooking into the MOP and
;; everything is written out by hand as a bunch of macros.
;;
;; There are some issues around defining a direct CLOS oriented
;; defgeneric/defmethod interface w/r/t subclassing
;; `unique-universal-identifier' b/c of the way we've chosen to interact with
;; the base UUID class `unique-universal-identifier' and do some non-CLOS
;; friendly things by treating the slot values of its instances as immutable
;; once instantiated and try to go out of our way to protect those slots.  It
;; may have been wiser to define the base class as a structure (which would
;; likely open up other complications) and then move the slot values of an
;; instantiated UUID structure into a CLOS thing.  Our original intent behind
;; defining UUIDs as classes was to implement the class
;; `unique-universal-identifier' in a manner similar enough to the class `uuid:uuid'
;; that existing 3 party code using the uuid system would retain a (mostly)
;; familar interface.  Should we ever decide to move to structure based
;; instances of unique-universal-identifier we would likely have to allocate
;; both a class object and a structure object for each UUID created in much the
;; same way that we are now doing with subclasses.  In any event, as it stands,
;; we're sticking with the existing class interface.
;;
;; :USAGE
;;
;; (defpackage #:tt-uuid-extended (:use #:common-lisp #:unicly))
;;
;; (defclass indexable-uuid (unicly:unique-universal-identifier)
;;  ((bit-vector 
;;    :reader bit-vector-of-uuid)
;;   (integer-128
;;    :reader integer-128-of-uuid)))
;;
;; (unicly::def-make-uuid-extend-class-fun indexed indexable-uuid)
;;
;; (defmethod update-instance-for-different-class  ((old unicly:unique-universal-identifier)
;;                                                  (new indexable-uuid)
;;                                                  &key)
;;   (with-slots (unicly::%uuid_time-low
;;                unicly::%uuid_time-mid
;;                unicly::%uuid_time-high-and-version
;;                unicly::%uuid_clock-seq-and-reserved
;;                unicly::%uuid_clock-seq-low
;;                unicly::%uuid_node)
;;       old
;;     (setf (slot-value new 'unicly::%uuid_time-low) unicly::%uuid_time-low
;;           (slot-value new 'unicly::%uuid_time-mid) unicly::%uuid_time-mid
;;           (slot-value new 'unicly::%uuid_time-high-and-version) unicly::%uuid_time-high-and-version
;;           (slot-value new 'unicly::%uuid_clock-seq-and-reserved) unicly::%uuid_clock-seq-and-reserved
;;           (slot-value new 'unicly::%uuid_clock-seq-low) unicly::%uuid_clock-seq-low
;;           (slot-value new 'unicly::%uuid_node) unicly::%uuid_node
;;           (slot-value new 'bit-vector)  (unicly:uuid-to-bit-vector old)
;;           (slot-value new 'integer-128) (unicly::uuid-bit-vector-to-integer (slot-value new 'bit-vector)))))
;;
;; (make-v5-uuid-indexed unicly:*uuid-namespace-dns* "bubba")
;; => eea1105e-3681-5117-99b6-7b2b5fe1f3c7
;;
;; (make-v5-uuid-indexed  (make-v5-uuid-indexed unicly:*uuid-namespace-dns* "foo") "bar")
;; => c62a67b5-d166-5e74-9e17-ec4d173bddc9
;;
;; (make-v3-uuid-indexed unicly:*uuid-namespace-dns* "bubba")
;; => 5e320838-7157-3039-8383-652d96705a7d
;;
;; (make-v4-uuid-indexed)
;; => 0c813195-5c4d-40a4-acc1-994c26d773b3
;;
;; (make-uuid-from-bit-vector-indexed 
;;  (unicly:uuid-to-bit-vector 
;;   (make-v5-uuid-indexed unicly:*uuid-namespace-dns* "bubba")))
;; => eea1105e-3681-5117-99b6-7b2b5fe1f3c7
;;
;; (make-uuid-from-byte-array-indexed 
;;  (unicly::uuid-to-byte-array 
;;   (make-v3-uuid-indexed unicly:*uuid-namespace-dns* "bubba")))
;; => 5e320838-7157-3039-8383-652d96705a7d
;;
;; (make-uuid-from-string-indexed "eea1105e-3681-5117-99b6-7b2b5fe1f3c7")
;; => eea1105e-3681-5117-99b6-7b2b5fe1f3c7
;;
;; (null (ignore-errors (make-uuid-from-string-indexed "00000000-0000-0000-0000-000000000000")))
;; => T
;;
;; (null (ignore-errors (make-v5-uuid-indexed *uuid-namespace-dns* "")))
;; => T
;;
;; (null (ignore-errors (make-v5-uuid-indexed (make-instance 'indexable-uuid) "bubba")))
;; => T
;;
;; (let ((unicly::*uuid-allow-empty-string-name-args* t) 
;;       (unicly::*uuid-allow-null-like-namespace-args* t))
;;   (make-v5-uuid-indexed (make-instance 'indexable-uuid) "bubba"))
;; => ca773f8d-32a5-51fa-915e-1600b9c37958
;;
;;; ==============================
;;
;; :TODO macros for 
;; `uuid-copy-uuid' as `def-make-uuid-copy-uuid-extended'
;;
;;; ==============================

(in-package #:unicly)

(defun %verify-valid-uuid-subclass-type (maybe-valid-uuid-subclass)
  (unless (symbolp  maybe-valid-uuid-subclass)
    (error "Arg MAYBE-VALID-UUID-SUBCLASS must satisfy `cl:symbolp'~% got: ~S~% type-of: ~S"
           maybe-valid-uuid-subclass (type-of maybe-valid-uuid-subclass)))
  (when (eq maybe-valid-uuid-subclass 'unicly:unique-universal-identifier)
    (error "Arg MAYBE-VALID-UUID-SUBCLASS must be a subclass of `unicly:unique-universal-identifier'"))
  (unless (equal '(T T)
                 (multiple-value-list
                  (subtypep maybe-valid-uuid-subclass 'unique-universal-identifier)))
    (error "Arg MAYBE-VALID-UUID-SUBCLASS not `cl:subtypep' the class `unicly:unique-universal-identifier'~% ~
            got: ~S~% type-of: ~S~%" maybe-valid-uuid-subclass (type-of maybe-valid-uuid-subclass)))
  (the symbol maybe-valid-uuid-subclass))

(defun %verify-valid-uuid-subclass-slots (class-to-verify)
  (let ((obj (make-instance class-to-verify)))
    (loop for slot in (list 
                       '%uuid_time-low
                       '%uuid_time-mid
                       '%uuid_time-high-and-version 
                       '%uuid_clock-seq-and-reserved
                       '%uuid_clock-seq-low
                       '%uuid_node)
       unless (slot-exists-p obj slot)
       do (error "Class slot ~S does not satisfy `cl:slot-exists-p' for class ~S"
                 slot class-to-verify)
       end
       unless (zerop (slot-value obj slot))
       do (error "Default value of slot ~S does not satisfy `cl:zero-p' for class ~S"
                 slot class-to-verify))
    class-to-verify))


(defun %verify-valid-subclass-and-slots (subclass-to-verify)
  (%verify-valid-uuid-subclass-slots (%verify-valid-uuid-subclass-type subclass-to-verify)))

(declaim (inline %make-uuid-from-string-extended-null-string-error))
(defun %make-uuid-from-string-extended-null-string-error (maybe-valid-uuid-hex-string-36)
  ;; :TODO We originally declared MAYBE-VALID-UUID-HEX-STRING-36 as `unicly:string-or-null'
  ;; verify why we might ever expect MAYBE-VALID-UUID-HEX-STRING-36 to be other than of type
  ;; unicly::uuid-string-36
  (declare (type uuid-string-36 +uuid-null-string+)
           (inline uuid-string-36-check-type)
           (optimize (speed 3)))
  (when (uuid-string-36-check-type maybe-valid-uuid-hex-string-36)
    (locally 
        (declare (type uuid-string-36 maybe-valid-uuid-hex-string-36))
      (if (string= maybe-valid-uuid-hex-string-36 +uuid-null-string+)
          (error "Arg MAYBE-VALID-UUID-HEX-STRING-36 must not be `cl:string=' the constant `unicly::+uuid-null-string+'")
          maybe-valid-uuid-hex-string-36))))

(declaim (inline %make-uuid-from-byte-array-extended-null-array-error))
(defun %make-uuid-from-byte-array-extended-null-array-error (maybe-valid-uuid-byte-array)
  (declare (inline uuid-byte-array-16-check-type
                   %uuid-byte-array-null-p)
           (optimize (speed 3)))
  (when (uuid-byte-array-16-check-type maybe-valid-uuid-byte-array)
    (locally 
        (declare (type uuid-byte-array-16 maybe-valid-uuid-byte-array))
      (if (%uuid-byte-array-null-p maybe-valid-uuid-byte-array)
          (error "Arg MAYBE-VALID-UUID-BYTE-ARRAY must be an array of type `unicly:uuid-byte-array-16' without all octets `cl:zerop'")
          maybe-valid-uuid-byte-array))))

(declaim (inline %make-uuid-from-bit-vector-extendable-bv-zeroed-error))
(defun %make-uuid-from-bit-vector-extendable-bv-zeroed-error (maybe-valid-uuid-bit-vector)
  (declare (inline uuid-bit-vector-null-p
                   uuid-bit-vector-128-check-type)
           (optimize (speed 3)))
  (if (uuid-bit-vector-128-check-type  maybe-valid-uuid-bit-vector)
      (locally 
          (declare (type uuid-bit-vector-128 maybe-valid-uuid-bit-vector))
        (if (uuid-bit-vector-null-p maybe-valid-uuid-bit-vector)
            (error "Arg must _not_ satisfy `unicly::uuid-bit-vector-null-p'")
            maybe-valid-uuid-bit-vector))))

(defmacro def-make-v5-uuid-extended (make-v5-uuid-suffix v5-uuid-class &optional (no-verify nil))
  ;; (macroexpand-1 '(unicly::def-make-v5-uuid-extended indexable indexable-uuid))
  ;; (macroexpand-1 '(unicly::def-make-v5-uuid-extended indexable indexable-uuid t))
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots v5-uuid-class))
  (let ((v5-fun-name 
         (intern (format nil "MAKE-V5-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix))))))
    `(defun ,v5-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name)
                (optimize (speed 3)))
       (let ((change-obj (unicly::make-v5-uuid namespace name)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v5-uuid-class)))))

(defmacro def-make-v3-uuid-extended (make-v3-uuid-suffix v3-uuid-class &optional (no-verify nil))
  ;; (macroexpand-1 '(def-make-v3-uuid-extended indexed indexable-uuid))
  ;; (macroexpand-1 '(def-make-v3-uuid-extended indexed indexable-uuid t))
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots v3-uuid-class))
  (let ((v3-fun-name 
         (intern (format nil "MAKE-V3-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v3-uuid-suffix))))))
    `(defun ,v3-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name)
                (optimize (speed 3)))
       (let ((change-obj (unicly::make-v3-uuid namespace name)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v3-uuid-class)))))

(defmacro def-make-v4-uuid-extended (make-v4-uuid-suffix v4-uuid-class &optional (no-verify nil))
  ;; (macroexpand-1 '(def-make-v4-uuid-extended indexed indexable-uuid))
  ;; (macroexpand-1 '(def-make-v4-uuid-extended indexed indexable-uuid t))
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots v4-uuid-class))
  (let ((v4-fun-name 
         (intern (format nil "MAKE-V4-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v4-uuid-suffix))))))
    `(defun ,v4-fun-name ()
       (let ((change-obj (unicly::make-v4-uuid)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v4-uuid-class)))))

(defmacro def-make-uuid-from-string-extended (make-extended-suffix extended-class &optional (no-verify nil))
  ;; (macroexpand-1 '(def-make-uuid-from-string-extended indexed indexable-uuid))
  ;; (macroexpand-1 '(def-make-uuid-from-string-extended indexed indexable-uuid t))
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots extended-class))
  (let ((uuid-from-string-fun
         (intern (format nil "MAKE-UUID-FROM-STRING-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-extended-suffix))))))
    `(defun ,uuid-from-string-fun (uuid-hex-string-36)
       (declare (type unicly::string-compat uuid-hex-string-36)
                (inline unicly::%make-uuid-from-string-extended-null-string-error)
                (optimize (speed 3)))
       (when (unicly::%make-uuid-from-string-extended-null-string-error uuid-hex-string-36)
         (locally 
             (declare (type unicly::uuid-string-36 uuid-hex-string-36))
           (let ((change-obj (unicly::make-uuid-from-string uuid-hex-string-36)))
             (declare (type unicly::unique-universal-identifier change-obj))
             (change-class change-obj ',extended-class)))))))

(defmacro def-make-uuid-byte-array-extended (make-extended-suffix extended-class &optional (no-verify nil))
  ;; (macroexpand-1 '(def-make-uuid-byte-array-extended indexed indexable-uuid)) 
  ;; (macroexpand-1 '(def-make-uuid-byte-array-extended indexed indexable-uuid t)) 
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots extended-class))
  (let ((uuid-from-ba-fun
         (intern (format nil "MAKE-UUID-FROM-BYTE-ARRAY-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-extended-suffix))))))
    `(defun ,uuid-from-ba-fun (uuid-byte-array-16)
       (declare (type unicly::uuid-byte-array uuid-byte-array-16)
                (inline unicly::%make-uuid-from-byte-array-extended-null-array-error)
                (optimize (speed 3)))
       (when (unicly::%make-uuid-from-byte-array-extended-null-array-error uuid-byte-array-16)
         (locally 
             (declare (type unicly::uuid-byte-array-16 uuid-byte-array-16))
           (let ((change-obj (unicly::uuid-from-byte-array uuid-byte-array-16)))
             (declare (type unicly::unique-universal-identifier change-obj))
             (change-class change-obj ',extended-class)))))))

(defmacro def-uuid-from-bit-vector-extendable (make-uuid-from-bv-suffix uuid-bv-class &optional (no-verify nil))
  ;; (macroexpand-1 '(def-uuid-from-bit-vector-extendable indexed indexable-uuid))
  ;; (macroexpand-1 '(def-uuid-from-bit-vector-extendable indexed indexable-uuid t))
  (declare (type boolean no-verify))
  (unless no-verify (%verify-valid-subclass-and-slots uuid-bv-class))
  (let ((uuid-from-bv-fun
         (intern (format nil "MAKE-UUID-FROM-BIT-VECTOR-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-uuid-from-bv-suffix))))))
    `(defun ,uuid-from-bv-fun (uuid-bit-vector-128)
       (declare (unicly::uuid-bit-vector-128 uuid-bit-vector-128)
                (inline unicly::%make-uuid-from-bit-vector-extendable-bv-zeroed-error)
                (optimize (speed 3)))
       #-:sbcl (unicly::uuid-bit-vector-128-check-type uuid-bit-vector-128)
       (%make-uuid-from-bit-vector-extendable-bv-zeroed-error uuid-bit-vector-128)
       (let ((change-obj (unicly::uuid-from-bit-vector uuid-bit-vector-128)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',uuid-bv-class)))))

(defmacro def-make-uuid-extend-class-fun (make-extended-suffix extended-class)
  ;; (macroexpand-1 '(def-make-uuid-extend-class-fun indexed indexable-uuid))
  `(progn
     (%verify-valid-subclass-and-slots ',extended-class)
     (unicly::def-make-v3-uuid-extended ,make-extended-suffix           ,extended-class t)
     (unicly::def-make-v5-uuid-extended ,make-extended-suffix           ,extended-class t)
     (unicly::def-make-v4-uuid-extended ,make-extended-suffix           ,extended-class t)
     (unicly::def-make-uuid-from-string-extended ,make-extended-suffix  ,extended-class t)
     (unicly::def-make-uuid-byte-array-extended ,make-extended-suffix   ,extended-class t)
     (unicly::def-uuid-from-bit-vector-extendable ,make-extended-suffix ,extended-class t)
     (values)))

;;; ==============================
;;; EOF
