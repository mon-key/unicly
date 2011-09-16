;;; :FILE-CREATED <Timestamp: #{2011-09-15T19:19:50-04:00Z}#{11374} - by MON>
;;; :FILE unicly/unicly-extend.lisp
;;; ==============================

;; The inteface defined here is experimental!

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

(defmacro def-make-v5-uuid-extended (make-v5-uuid-suffix v5-uuid-class)
  ;; (unicly::def-make-v5-uuid-extended indexable uuid-indexable-v5)
  (%verify-valid-uuid-subclass v5-uuid-class)
  (%verify-class-slots         v5-uuid-class)
  (let ((v5-fun-name 
         (intern (format nil "MAKE-V5-UUID-~A"
                         (string-trim '(#\SPACE #\- #\:) (string-upcase make-v5-uuid-suffix))))))
    `(defun ,v5-fun-name (namespace name)
       (let ((change-obj (make-v5-uuid namespace name)))
         (declare (unique-universal-identifier change-obj))
         (change-class change-obj ',v5-uuid-class)))))

;; `(defun ,<DIGESTED-V5-UUID-FUN-NAME> (v5-digest-byte-array)
;;    (declare (type uuid-byte-array-20 
;;                   v5-digest-byte-array)
;;             (inline %uuid_time-low-request
;;                     %uuid_time-mid-request
;;                     %uuid_time-high-and-version-request
;;                     %uuid_clock-seq-and-reserved-request 
;;                     %uuid_node-request)
;;             (optimize (speed 3)))
;;    (the ,<V5-UUID-CLASS>
;;      (make-instance ',<V5-UUID-CLASS>
;;                     :%uuid_time-low (%uuid_time-low-request v5-digest-byte-array)
;;                     :%uuid_time-mid (%uuid_time-mid-request v5-digest-byte-array)
;;                     :%uuid_time-high-and-version (%uuid_time-high-and-version-request v5-digest-byte-array #x05)
;;                     :%uuid_clock-seq-and-reserved (%uuid_clock-seq-and-reserved-request v5-digest-byte-array)
;;                     :%uuid_clock-seq-low (the uuid-ub8 (%uuid_clock-seq-low-request v5-digest-byte-array))
;;                     :%uuid_node (%uuid_node-request v5-digest-byte-array))))

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


;;; ==============================
;;; EOF
