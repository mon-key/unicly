;;; :FILE-CREATED <Timestamp: #{2011-09-20T17:59:09-04:00Z}#{11382} - by MON>
;;; :FILE unicly/unicly-null-check.lisp
;;; ==============================

;; helper functions for intercepting around
;; `*uuid-allow-null-like-namespace-args*' `*uuid-allow-empty-string-name-args*'
;; and prior to `uuid-digest-uuid-instance'.


(in-package #:unicly)
;; *package*

;; *uuid-allow-null-like-namespace-args*
(declaim (inline %verify-non-null-namespace-arg))
(defun %verify-non-null-namespace-arg (namespace-arg)
  ;; (values uuid-byte-array-16 &optional)
  (declare (type unique-universal-identifier namespace-arg)
           (inline uuid-byte-array-null-p)
           (optimize (speed 3)))
  ;; (if *uuid-allow-null-like-namespace-args*
  ;;     (the (values uuid-byte-array-16 &optional)
  ;;       (uuid-get-namespace-bytes namespace-arg))
  (let ((name-bytes (the (values uuid-byte-array-16 &optional)
                      (uuid-get-namespace-bytes namespace-arg))))
    (declare (uuid-byte-array-16 name-bytes))
    (when *uuid-allow-null-like-namespace-args*
      (return-from %verify-non-null-namespace-arg name-bytes))
    (if (uuid-byte-array-null-p name-bytes)
        (error "Declining to generate an uuid with NAMESPACE-ARG.~%~%~
                    Current value of `unicly::*uuid-allow-null-like-namespace-args*' null \(the default\),~%~
                    but got object with slot-values equivalent to object returned by `unicly:make-null-uuid'~%~
                    and which does not otherwise satisfy `unicly:unique-universal-identifier-null-p'.~%~%~
                    If you intended to use the null-uuid as NAMESPACE-ARG ~%~
                    pass the object returned from `unicly:make-null-uuid' instead.~%~% got: ~S~% type-of: ~S~%"
               namespace-arg (type-of namespace-arg))
        name-bytes)))

;; (type-of (%uuid-string-to-octets "bubba"))
;; (mon:type-expand-all 'uuid-byte-array)
;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
;; (%uuid-byte-array-null-p (uuid-get-namespace-bytes 
;;
;; (fundoc '%verify-non-empty-name-arg
;; If value of `unicly::*uuid-allow-empty-string-name-args*' is T retrun NAME-ARG.
;; If value of NAME-ARG is of type `unicly::string-not-empty' retrun NAME-ARG,
;; if not and error is signaled.
;; :EXAMPLE
;; (%verify-non-empty-name-arg "bubba")
;; (let ((*uuid-allow-empty-string-name-args* t))
;;   (%verify-non-empty-name-arg ""))
;; (null (ignore-errors (%verify-non-empty-name-arg "")))
(declaim (inline %verify-non-empty-name-arg))
(defun %verify-non-empty-name-arg (name-arg)
  ;; (values uuid-byte-array &optional)
  (declare (type string-compat name-arg)
           (inline 
             #+:sbcl %uuid-string-to-octets
             %string-not-empty-p)
           (optimize (speed 3)))
  (the (values uuid-byte-array &optional)
    (%uuid-string-to-octets 
     (the (values string-compat &optional)
       (if *uuid-allow-empty-string-name-args*
           name-arg
           (if (%string-not-empty-p name-arg)
               name-arg
               (error "Declining to generate an uuid with NAME-ARG.~%~%~
                   Got string of type `unicly::string-empty' with current value of~%~
                  `unicly::*uuid-allow-empty-string-name-args*' null \(the default\).~%~%~
                  If you intend\(ed\) to pass the empty-string to a uuid function which requires~%~
                  both a NAME and NAMESPACE argument, consider dynamically binding the value of~%~
                  `unicly::*uuid-allow-empty-string-name-args*' T within a wrapper function.~%")))))))

;; (fundoc 'verify-sane-namespace-and-name
;; (verify-sane-namespace-and-name (make-v4-uuid) "bubba")
;;
;; (let ((*uuid-allow-empty-string-name-args* t))
;;   (verify-sane-namespace-and-name (make-v4-uuid) ""))
;;
;; (null (ignore-errors (verify-sane-namespace-and-name (make-v4-uuid) "")))
;;
;; (let ((*uuid-allow-null-like-namespace-args* t))
;;   (verify-sane-namespace-and-name (make-instance 'unique-universal-identifier) "bubba"))
;;
;; (let ((*uuid-allow-null-like-namespace-args* t)
;;       (*uuid-allow-empty-string-name-args*   t))
;;   (verify-sane-namespace-and-name (make-instance 'unique-universal-identifier) ""))
(defun verify-sane-namespace-and-name (namespace name)
  ;; (values uuid-byte-array uuid-byte-array-16 &optional)
  (declare (type unique-universal-identifier namespace)
           (type string-compat name)
           (inline %verify-non-null-namespace-arg
                   %verify-non-empty-name-arg)
           (optimize (speed 3)))
  ;; :NOTE do the empty-string check first b/c its cheaper
  (let ((empty-string-chk (the (values uuid-byte-array &optional)
                            (%verify-non-empty-name-arg name)))
        (null-uuid-chk    (the (values uuid-byte-array-16 &optional)
                            (%verify-non-null-namespace-arg namespace))))
    (values null-uuid-chk empty-string-chk)))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
