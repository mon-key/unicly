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
;;; ==============================
;;
;; :TODO macros for 
;; `uuid-copy-uuid' as `def-make-uuid-copy-uuid-extended'
;;
;;; ==============================

(in-package #:unicly)

(defun %verify-valid-uuid-subclass (subclass)
  ;; (%verify-valid-uuid-subclass 'unique-universal-identifier)
  (when (eq subclass 'unicly:unique-universal-identifier)
    (error "Arg SUBCLASS must be a subclass of unicly:unique-universal-identifier"))
  (unless (equal '(T T)
                 (multiple-value-list
                  (subtypep subclass 'unique-universal-identifier)))
    (error "Arg SUBCLASS not `cl:subtypep' the class `unicly:unique-universal-identifier'~% ~
            got: ~S~% type-of: ~S~%" subclass (type-of subclass))))

(defun %verify-class-slots (class)
  ;; (%verify-class-slots 'unique-universal-identifier)
  (let ((obj (make-instance class)))
    (loop for slot in (list 
                       '%uuid_time-low
                       '%uuid_time-mid
                       '%uuid_time-high-and-version 
                       '%uuid_clock-seq-and-reserved
                       '%uuid_clock-seq-low
                       '%uuid_node)
       unless (slot-exists-p obj slot)
       do (error "Class slot ~S does not satisfy `cl:slot-exists-p' for class ~S"
                 slot class)
       end
       unless (zerop (slot-value obj slot))
       do (error "Default value of slot ~S does not satisfy `cl:zero-p' for class ~S"
                 slot class))
    class))

(declaim (inline %make-uuid-from-string-extended-null-string-error))
(defun %make-uuid-from-string-extended-null-string-error (maybe-null-string)
  ;; :TODO We originally declared MAYBE-NULL-STRING as `unicly:string-or-null'
  ;; verify why we might ever expect MAYBE-NULL-STRING to be other than of type
  ;; unicly::uuid-string-36
  (declare (type unicly::uuid-string-36 maybe-null-string)
           (optimize (speed 3)))
  (if (string= maybe-null-string +uuid-null-string+)
      (error "Arg must not be `cl:string=' the constant `unicly::+uuid-null-string+'")
      maybe-null-string))

(declaim (inline %make-uuid-from-byte-array-extended-null-array-error))
(defun %make-uuid-from-byte-array-extended-null-array-error (byte-array)
  ;; (%make-uuid-from-byte-array-extended-null-array-error (uuid-byte-array-16-zeroed))
  (declare (type uuid-byte-array-16 byte-array)
           (inline %uuid-byte-array-null-p)
           (optimize (speed 3)))
  (if (%uuid-byte-array-null-p byte-array)
      (error "Arg must not be an array with all octets `cl:zerop'")
      byte-array))

;; (fundoc '%make-uuid-from-bit-vector-extendable-bv-zeroed-error
;; Return MAYBE-BIT-VECTOR-ZEROED or error if it is `unicly::uuid-bit-vector-null-p'.
;; :EXAMPLE
;; (let ((zero-bits (uuid-bit-vector-128-zeroed)))
;;    (setf (sbit zero-bits 0) 1)
;;    (%make-uuid-from-bit-vector-extendable-bv-zeroed-error zero-bits))
;; Following each fail succesfully:
;; (%make-uuid-from-bit-vector-extendable-bv-zeroed-error (uuid-bit-vector-128-zeroed))
;; (%make-uuid-from-bit-vector-extendable-bv-zeroed-error (make-array 16 :element-type 'bit))
(declaim (inline %make-uuid-from-bit-vector-extendable-bv-zeroed-error))
(defun %make-uuid-from-bit-vector-extendable-bv-zeroed-error (maybe-bit-vector-zeroed)
  (declare (unicly::uuid-bit-vector-128 maybe-bit-vector-zeroed)
           (inline unicly::uuid-bit-vector-null-p)
           (optimize (speed 3)))
  (if (unicly::uuid-bit-vector-null-p maybe-bit-vector-zeroed)
      (error "Arg must _not_ satisfy `unicly::uuid-bit-vector-null-p'")
      maybe-bit-vector-zeroed))

(defun %verify-valid-subclass-and-slots (class-to-verify)
  (%verify-valid-uuid-subclass class-to-verify)
  (%verify-class-slots         class-to-verify))

(defmacro def-make-v5-uuid-extended (make-v5-uuid-suffix v5-uuid-class)
  ;; (macroexpand-1 '(unicly::def-make-v5-uuid-extended indexable uuid-indexable-v5))
  ;;  (%verify-valid-uuid-subclass v5-uuid-class)
  ;; (%verify-class-slots         v5-uuid-class)
  (let ((v5-fun-name 
         (intern (format nil "MAKE-V5-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix))))))
    `(defun ,v5-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name))
       (let ((change-obj (unicly::make-v5-uuid namespace name)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v5-uuid-class)))))

(defmacro def-make-v3-uuid-extended (make-v3-uuid-suffix v3-uuid-class)
  ;; (macroexpand-1 '(def-make-v3-uuid-extended indexable uuid-indexable-v3))
  ;; (%verify-valid-uuid-subclass v3-uuid-class)
  ;; (%verify-class-slots         v3-uuid-class)
  (let ((v3-fun-name 
         (intern (format nil "MAKE-V3-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v3-uuid-suffix))))))
    `(defun ,v3-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name))
       (let ((change-obj (unicly::make-v3-uuid namespace name)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v3-uuid-class)))))

(defmacro def-make-v4-uuid-extended (make-v4-uuid-suffix v4-uuid-class)
  ;; (macroexpand-1 '(def-make-v4-uuid-extended indexable uuid-indexable-v3))
  ;; (%verify-valid-uuid-subclass v4-uuid-class)
  ;; (%verify-class-slots         v4-uuid-class)
  (let ((v4-fun-name 
         (intern (format nil "MAKE-V4-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v4-uuid-suffix))))))
    `(defun ,v4-fun-name ()
       (let ((change-obj (unicly::make-v4-uuid)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v4-uuid-class)))))

(defmacro def-make-uuid-from-string-extended (make-extended-suffix extended-class)
  (let ((uuid-from-string-fun
         (intern (format nil "MAKE-UUID-FROM-STRING-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-extended-suffix))))))
    `(defun ,uuid-from-string-fun (uuid-hex-string-36)
       (declare (type unicly::uuid-hex-string-36 uuid-hex-string-36)
                (inline %make-uuid-from-string-extended-null-string-error))
       (%make-uuid-from-string-extended-null-string-error uuid-hex-string-36)
       (let ((change-obj (make-uuid-from-string uuid-hex-string-36)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',extended-class)))))

(defmacro def-make-uuid-byte-array-extended (make-extended-suffix extended-class)
  ;; (macroexpand-1 '(def-make-uuid-byte-array-extended indexable uuid-indexable-v3))
  (let ((uuid-from-ba-fun
         (intern (format nil "MAKE-UUID-FROM-BYTE-ARRAY-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-extended-suffix))))))
    `(defun ,uuid-from-ba-fun (uuid-byte-array-16)
       (declare (type unicly::uuid-byte-array-16 uuid-byte-array-16)
                (inline unicly::%make-uuid-from-byte-array-extended-null-array-error))
       (unicly::%make-uuid-from-byte-array-extended-null-array-error uuid-byte-array-16)
       (let ((change-obj (unicly::uuid-from-byte-array uuid-byte-array-16)))
         (declare (type unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',extended-class)))))

(defmacro def-uuid-from-bit-vector-extendable (make-uuid-from-bv-suffix uuid-bv-class)
  ;; (macroexpand-1 '(def-uuid-from-bit-vector-extendable indexable uuid-indexable-v3))
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
  ;; (macroexpand-1 '(def-make-uuid-extend-class-fun indexable uuid-indexable-v5))
  (%verify-valid-subclass-and-slots extended-class)
  `(progn
     (unicly::def-make-v3-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-v5-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-v4-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-uuid-from-string-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-uuid-byte-array-extended ,make-extended-suffix ,extended-class)
     (unicly::def-uuid-from-bit-vector-extendable ,make-extended-suffix ,extended-class)
     (values)))

;;; ==============================
;; Alternative forms of macro `def-make-v5-uuid-extended',
;; `def-make-v4-uuid-extended' That are likely faster where we don't need to
;; accomodate specializations on `cl:update-instance-for-different-class'
;; e.g. where we know that the subclass arguments don't contain additional
;; direct-slots which need to be accomodated.
;;

;; NOTE This is likely a faster but won't handle updating the `cl:slot-value's of arg V4-UUID-CLASS
;; (defmacro def-make-v4-uuid-extended (make-v4-uuid-suffix v4-uuid-class)
;;   ;; (macroexpand-1 '(def-make-v4-uuid-extended indexable uuid-indexable-v3))
;;   ;; (%verify-valid-uuid-subclass v4-uuid-class)
;;   ;; (%verify-class-slots         v4-uuid-class)
;;   (let ((v4-fun-name 
;;          (intern (format nil "MAKE-V4-UUID-~A"
;;                          (string-trim '(#\SPACE #\- #\:) (string-upcase make-v4-uuid-suffix))))))
;;     `(defun ,v4-fun-name, ()
;;        (declare (special unicly::*random-state-uuid*)
;;                 (optimize (speed 3)))
;;        (let ((*random-state* (the random-state unicly::*random-state-uuid*)))
;;          (the ,v4-uuid-class
;;            (make-instance ',v4-uuid-class
;;                           :%uuid_time-low (the unicly::uuid-ub32 (random #xFFFFFFFF))
;;                           :%uuid_time-mid (the unicly::uuid-ub16 (random #xFFFF))
;;                           :%uuid_time-high-and-version  
;;                           (the unicly::uuid-ub16 (dpb #b0100 (byte 4 12) (ldb (byte 12 0) (the unicly::uuid-ub16 (random #xFFFF)))))
;;                           :%uuid_clock-seq-and-reserved
;;                           (the unicly::uuid-ub8  (dpb #b0010 (byte 2  6) (ldb (byte  8 0) (the unicly::uuid-ub8 (random #xFF)))))
;;                           :%uuid_clock-seq-low (the unicly::uuid-ub8 (random #xFF))
;;                           :%uuid_node (the unicly::uuid-ub48 (random #xFFFFFFFFFFFF))))))))
;;
;; (defmacro def-make-v5-uuid-extended (make-v5-uuid-suffix v5-uuid-class)
;;   (%verify-valid-uuid-subclass v5-uuid-class)
;;   (%verify-class-slots         v5-uuid-class)
;;   (let ((v5-fun-name 
;;          (intern (format nil "MAKE-V5-UUID-~A"
;;                          (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix)))))
;;         (digested-v5-uuid-fun-name
;;          (intern (format nil "%DIGESTED-V5-UUID-~A"
;;                          (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix))))))
;;     ;;
;;     `(defun ,<DIGESTED-V5-UUID-FUN-NAME> (v5-digest-byte-array)
;;        (declare (type unicly::uuid-byte-array-20 
;;                       v5-digest-byte-array)
;;                 (inline unicly::%uuid_time-low-request
;;                         unicly::%uuid_time-mid-request
;;                         unicly::%uuid_time-high-and-version-request
;;                         unicly::%uuid_clock-seq-and-reserved-request 
;;                         unicly::%uuid_node-request)
;;                 (optimize (speed 3)))
;;        (the ,V5-UUID-CLASS
;;          (make-instance ',V5-UUID-CLASS
;;                         :%uuid_time-low (unicly::%uuid_time-low-request v5-digest-byte-array)
;;                         :%uuid_time-mid (unicly::%uuid_time-mid-request v5-digest-byte-array)
;;                         :%uuid_time-high-and-version (unicly::%uuid_time-high-and-version-request v5-digest-byte-array #x05)
;;                         :%uuid_clock-seq-and-reserved (unicly::%uuid_clock-seq-and-reserved-request v5-digest-byte-array)
;;                         :%uuid_clock-seq-low (the uuid-ub8 (unicly::%uuid_clock-seq-low-request v5-digest-byte-array))
;;                         :%uuid_node (unicly::%uuid_node-request v5-digest-byte-array))))
;;     ;;
;;     `(defun ,V5-FUN-NAME (namespace name)
;;        (declare (type string name)
;;                 (type unicly:unique-universal-identifier namespace)
;;                 (inline unicly::uuid-digest-uuid-instance
;;                         ,DIGESTED-V5-UUID-FUN-NAME)
;;                 (optimize (speed 3)))
;;        (the (values ,V5-UUID-CLASS &optional)
;;          (,DIGESTED-V5-UUID-FUN-NAME
;;           (the unicly::uuid-byte-array-20 
;;             (unicly::uuid-digest-uuid-instance #x05 namespace name)))))))
;;
;;; ==============================


;;; ==============================
;;; EOF
