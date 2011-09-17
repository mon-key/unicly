;;; :FILE-CREATED <Timestamp: #{2011-09-15T19:19:50-04:00Z}#{11374} - by MON>
;;; :FILE unicly/unicly-extend.lisp
;;; ==============================

;;; ==============================
;; :NOTE The inteface defined here is experimental and subject to change!
;;
;; Should you need to define a function which instantiates a uuid for a subclass
;; of unique-universal-identifier I've provided an alternative form of the macro
;; `def-make-v5-uuid-extended' at the bottom of this file which is functional and
;; can be adapted to for use in third-party code but it isn't particular CLOS
;; friendly.
;;
;; There are some issues around subclassing `unique-universal-identifier' b/c of
;; we the way we've chosen to interact with the base class UUID class
;; `unique-universal-identifier'. Basically we treat the slot values of its
;; instances as immutable once instantiated and try to go out of our way to
;; protect those slots. It may have been wiser to define the base class as a
;; structure (which would likely open up other complications) and then move the
;; slot values of an instantiated UUID structure into a CLOS thing. 
;; The original intent behind defining UUIDs as classes was to:
;; implement unique-universal-identifier in a manner similar enough to the
;; class uuid:uuid that existing 3 party code using the uuid would remain
;; familar. If we moved to structure based instance we would likely have to
;; allocate both a class object and a structure object for each UUID created
;; In any event, as it stands we're sticking with the existing class interface.
;; This said, the current interface as defined below is not hooking into the MOP and everything by hand with attempting to 
;;
;; :USAGE
;;
;; (defclass indexable-uuid (unicly:unique-universal-identifier)
;;  ((bit-vector 
;;    :reader bit-vector-of-uuid)
;;   (integer-128
;;    :reader integer-128-of-uuid)))
;;
;; (def-make-uuid-extend-class-fun indexed indexable-uuid)
;;
;; (defmethod update-instance-for-different-class  ((old unicly:unique-universal-identifier)
;;                                                  (new indexable-uuid)
;;                                                  &key)
;;   (with-slots (%uuid_time-low
;;                %uuid_time-mid
;;                %uuid_time-high-and-version 
;;                %uuid_clock-seq-and-reserved
;;                %uuid_clock-seq-low 
;;                %uuid_node)
;;       old
;;     (setf (slot-value new '%uuid_time-low) %uuid_time-low
;;           (slot-value new '%uuid_time-mid) %uuid_time-mid
;;           (slot-value new '%uuid_time-high-and-version) %uuid_time-high-and-version
;;           (slot-value new '%uuid_clock-seq-and-reserved) %uuid_clock-seq-and-reserved
;;           (slot-value new '%uuid_clock-seq-low) %uuid_clock-seq-low
;;           (slot-value new '%uuid_node) %uuid_node
;;           (slot-value new 'bit-vector)  (unicly:uuid-to-bit-vector old)
;;           (slot-value new 'integer-128) (unicly::uuid-bit-vector-to-integer (slot-value new 'bit-vector)))))
;;
;; (make-v5-uuid-indexable *uuid-namespace-dns* "bubba")
;; (make-v3-uuid-indexable *uuid-namespace-dns* "bubba")
;; (make-v4-uuid-indexable)
;;
;;; ==============================
;;
;; :TODO macros for 
;; `uuid-copy-uuid' as `def-make-uuid-copy-uuid-extended'
;; `uuid-from-bit-vector' as `def-make-uuid-from-bit-vector-extended'
;; `uuid-from-byte-array' as `def-make-uuid-from-byte-array-extended'

;;; ==============================

(in-package #:unicly)

(defun %verify-valid-uuid-subclass (subclass)
  ;; (%verify-valid-uuid-subclass 'unique-universal-identifier)
  (when (eq subclass 'unicly:unique-universal-identifier)
    (error "Arg SUBCLASS must be a subclass of unicly:unique-universal-identifier"))
  (unless (equal '(T T)
                 (multiple-value-list
                  (subtypep 'uuid-indexable-v5 'unique-universal-identifier)))
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
  (declare (type (or string-or-null unique-universal-identifier) maybe-null-string)
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

;; (unicly::uuid-to-byte-array (make-v5-uuid-indexable unicly::*uuid-namespace-dns* "bubba"))

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

(defmacro def-make-uuid-extend-class-fun (make-extended-suffix extended-class)
  ;; (macroexpand-1 (def-make-uuid-extend-class-fun indexable uuid-indexable-v5))
  (%verify-valid-subclass-and-slots extended-class)
  `(progn
     (unicly::def-make-v3-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-v5-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-v4-uuid-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-uuid-from-string-extended ,make-extended-suffix ,extended-class)
     (unicly::def-make-uuid-byte-array-extended ,make-extended-suffix ,extended-class)
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
