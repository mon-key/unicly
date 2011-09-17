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
;; protect its slots. It may have been wiser to define the base class as a
;; structure (which would likely open up other complications) and then move the
;; slot values of an instantiated UUID structure into a CLOS thing. 
;; The original intent behind defining UUIDs as classes was to:
;; implement unique-universal-identifier in a manner similar enough to the
;; class uuid:uuid that existing 3 party code using the uuid would remain
;; familar. If we moved to structure based instance we would likely have to
;; allocate both a class object and a structure object for each UUID created
;; In any event, as it stands were sticking with the existing class interface.
;;
;; :USAGE
;;
;; (defclass indexable-v5-uuid (unicly:unique-universal-identifier)
;;  ((bit-vector 
;;    :reader bit-vector-of-uuid)
;;   (integer-128
;;    :reader integer-128-of-uuid)))
;;
;; (def-make-v5-uuid-extended indexed indexable-v5-uuid)
;;
;; (defmethod update-instance-for-different-class  ((old unique-universal-identifier)
;;                                                  (new uuid-indexable-v5)
;;                                                  &key)
;;   (with-slots '(%uuid_time-low
;;                 %uuid_time-mid
;;                 %uuid_time-high-and-version 
;;                 %uuid_clock-seq-and-reserved
;;                 %uuid_clock-seq-low 
;;                 %uuid_node)
;;       old
;;     (setf (slot-value new '%uuid_time-low) old
;;           (slot-value new '%uuid_time-mid) old
;;           (slot-value new '%uuid_time-high-and-version) old
;;           (slot-value new '%uuid_clock-seq-and-reserved) old
;;           (slot-value new '%uuid_clock-seq-low) old
;;           (slot-value new '%uuid_node) old
;;           (slot-value new 'bit-vector) (unicly:uuid-to-bit-vector old)
;;           (slot-value new 'integer-128)
;;           (unicly::uuid-bit-vector-to-integer (slot-value new 'bit-vector)))))
;;
;; (make-v5-uuid-indexable *uuid-namespace-dns* "bubba")
;;
;;; ==============================
;;
;; :TODO macros for 
;; `make-v3-uuid' as `def-make-v3-uuid-extended'
;; `make-v4-uuid' as `def-make-v4-uuid-extended'
;; `make-uuid-from-string' as `def-make-uuid-from-string-extended'
;; `uuid-copy-uuid'
;;; ==============================


(in-package #:unicly)

(defun %verify-valid-uuid-subclass (subclass)
  ;; (%verify-valid-uuid-subclass 'unique-universal-identifier)
  (when (eq subclass 'unicly:unique-universal-identifier)
    (error "Arg SUBCLASS must be a subclass of unicly:unique-universal-identifier"))
  (unless (equal '(T T)
                 (multiple-value-list
                  (subtypep 'uuid-indexable-v5 'unique-universal-identifier)))
    (error "Arg SUBCLASS not `cl:subtypep' the class `unique-universal-identifier'~% ~
            got: ~S~% type-of: ~S~%" sublcass (type-of subclass))))

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

(defun %make-uuid-from-string-extended-null-string-error (maybe-null-string)
  (declare (type (or string-or-null unique-universal-identifier) maybe-null-string)
           (optimize (speed 3)))
  (when (string= maybe-null-string +uuid-null-string+)
    (error "Arg must not be `cl:string=' `unicly:+uuid-null-string+'")))

(defmacro def-make-v5-uuid-extended (make-v5-uuid-suffix v5-uuid-class)
  ;; (macroexpand-1 '(unicly::def-make-v5-uuid-extended indexable uuid-indexable-v5))
  (%verify-valid-uuid-subclass v5-uuid-class)
  (%verify-class-slots         v5-uuid-class)
  (let ((v5-fun-name 
         (intern (format nil "MAKE-V5-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix))))))
    `(defun ,v5-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name))
       (let ((change-obj (unicly::make-v5-uuid namespace name)))
         (declare (unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v5-uuid-class)))))

(defmacro def-make-v3-uuid-extended (make-v3-uuid-suffix v3-uuid-class)
  ;; (macroexpand-1 '(def-make-v3-uuid-extended indexable uuid-indexable-v3))
  (%verify-valid-uuid-subclass v3-uuid-class)
  (%verify-class-slots         v3-uuid-class)
  (let ((v3-fun-name 
         (intern (format nil "MAKE-V3-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v3-uuid-suffix))))))
    `(defun ,v3-fun-name (namespace name)
       (declare (type unicly::unique-universal-identifier namespace)
                (type unicly::string-compat name))
       (let ((change-obj (unicly::make-v3-uuid namespace name)))
         (declare (unicly::unique-universal-identifier change-obj))
         (change-class change-obj ',v3-uuid-class)))))

;; (defun ,<MAKE-UUID-FROM-STRING-FOO> (uuid-or-hex-string-36)
;;  (let ((change-obj (make-uuid-from-string uuid-or-hex-string-36)))

;;; ==============================
;; Alternative form of macro `def-make-v5-uuid-extended' that doesn't so easily
;; accomodate specializations on `cl:update-instance-for-different-class'
;; with this version evaluating (def-make-v5-uuid-extended indexed some-indexed-class)
;; would generate two new functions `%digested-v5-uuid-indexed' and `make-v5-uuid-indexed'
;; With `make-v5-uuid-indexed' remaining as the preferred interface for
;; generating v5 UUID instances of the class `some-indexed-class'
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
;;        (declare (type uuid-byte-array-20 
;;                       v5-digest-byte-array)
;;                 (inline %uuid_time-low-request
;;                         %uuid_time-mid-request
;;                         %uuid_time-high-and-version-request
;;                         %uuid_clock-seq-and-reserved-request 
;;                         %uuid_node-request)
;;                 (optimize (speed 3)))
;;        (the ,V5-UUID-CLASS
;;          (make-instance ',V5-UUID-CLASS
;;                         :%uuid_time-low (%uuid_time-low-request v5-digest-byte-array)
;;                         :%uuid_time-mid (%uuid_time-mid-request v5-digest-byte-array)
;;                         :%uuid_time-high-and-version (%uuid_time-high-and-version-request v5-digest-byte-array #x05)
;;                         :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v5-digest-byte-array)
;;                         :%uuid_clock-seq-low (the uuid-ub8 (%uuid_clock-seq-low-request v5-digest-byte-array))
;;                         :%uuid_node (%uuid_node-request v5-digest-byte-array))))
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
