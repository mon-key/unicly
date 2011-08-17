;;; :FILE-CREATED <Timestamp: #{2011-08-17T16:19:17-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-uuid-version.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

;; %uuid-uuid-version-if

(declaim (inline %uuid-uuid-version-if))
(defun %uuid-uuid-version-if (uuid-time-high-and-version uuid)
  ;; :TEST (signals succesfully)
  ;; (let ((v4uuid (make-v4-uuid)))
  ;;   (setf (slot-value v4uuid '%uuid_time-high-and-version) #xFFFF) 
  ;;   (%uuid-uuid-version-if (slot-value v4uuid '%uuid_time-high-and-version) v4uuid))
  (declare (unique-universal-identifier uuid)
           (uuid-ub16 uuid-time-high-and-version)
           (optimize (speed 3)))
  (when (ldb-test (byte 1 15) uuid-time-high-and-version)
    (error 'uuid-bit-48-error :uuid-bit-48-error-datum uuid)))

;;; ==============================
;; ,---- RFC4122 4.1.3. Subsection "Version"
;; | The version number is in the most significant 4 bits of the time
;; | stamp (bits 4 through 7 of the time_hi_and_version field).
;; | 
;; |    15    14    13    12 
;; |  Msb0  Msb1  Msb2  Msb3   Version  Description
;; |     0     0     0     1        1     The time-based version specified in this document.
;; |     0     0     1     0        2     DCE Security version, with embedded POSIX UIDs.
;; |     0     0     1     1        3     The name-based MD5
;; |     0     1     0     0        4     The randomly or pseudo-randomly generated version
;; |     0     1     0     1        5     The name-based SHA-1
;; |     ^--bit-48
;; `----
;;
;; :TODO Currently not detecting v1 or v2 UUIDs at all.
;; uuid-version-bit-vector
(declaim (inline uuid-version-uuid))
(defun uuid-version-uuid (uuid)
  ;; :WAS (declare  (unique-universal-identifier uuid)
  (declare ((or unique-universal-identifier uuid-bit-vector-128) uuid)
           (inline %unique-universal-identifier-null-p)
           (optimize (speed 3)))
  ;; :NOTE !EXPERIMENTAL!
  ;; Its entirely possible for a bit-vector of length 128 to be
  ;; passed and for that that b-v to be contained of a bit-field that
  ;; independent of its 48 bit in no other way resembles a UUID
  (when (uuid-bit-vector-128-p uuid)
    (return-from uuid-version-uuid (uuid-version-bit-vector uuid)))
  ;;
  (locally 
      (declare (unique-universal-identifier uuid))
    (when (%unique-universal-identifier-null-p uuid)
      (return-from uuid-version-uuid (values 0 'null-uuid)))
    (let ((uuid-thav (if (slot-boundp uuid '%uuid_time-high-and-version)
                         (slot-value uuid '%uuid_time-high-and-version)
                         (error 'uuid-simple-error ;; 'uuid-slot-unbound-error
                          :format-control "slot %UUID_TIME-HIGH-AND-VERSION is not ~
                                              `cl:slot-boundp' in uuid object"))))
      (declare (uuid-ub16 uuid-thav))
      (%uuid-uuid-version-if uuid-thav uuid)
      (or (and (ldb-test (byte 1 13) uuid-thav)
               (ldb-test (byte 1 12) uuid-thav)
               3)
          (and (ldb-test (byte 1 14) uuid-thav)
               (or (and (ldb-test (byte 1 12) uuid-thav) 5)
                   (and (not (ldb-test (byte 1 13) uuid-thav)) 4)
                   (error 'uuid-simple-error
                          :format-control "something wrong with UUID bit field~% got: ~S"
                          :format-arguments (list uuid-thav))))))))

;; :TODO use `uuid-string-parse-integer' to get the version from a uuid-string-32
;; :SEE The notes at `make-uuid-from-string-if' and `make-uuid-from-string'.
;; (defun uuid-string-36-version (uuis-hex-string-36) (...))
;; (defun uuid-string-32-version (uuis-hex-string-32) (...))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
