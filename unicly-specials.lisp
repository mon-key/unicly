;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:00:53-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-specials.lisp
;;; ==============================


(in-package #:unicly)
;; *package*


;;; ==============================
;;; :UNICLY-VARIABLES
;;; ==============================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *random-state-uuid* (make-random-state t))

(defvar *uuid-null-uuid* nil)

;;; ==============================
;; :NOTE Following are bound to their RFC4122 Appendix C. defaults at loadtime in:
;; :FILE unicly-loadtime-bind.lisp

(defparameter *uuid-namespace-dns* 
  ;; (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  nil) 

(defparameter *uuid-namespace-url* 
  ;; (make-uuid-from-string "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
  nil) 

(defparameter *uuid-namespace-oid*  
  ;; (make-uuid-from-string "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
  nil) 

(defparameter *uuid-namespace-x500*
  ;; (make-uuid-from-string "6ba7b814-9dad-11d1-80b4-00c04fd430c8")
  nil) 
)

;; (defparameter *uuid-v1-imitation-seed* (make-v4-uuid))
;; (uuid-version-uuid *uuid-namespace-dns*) (make-v5-uuid *uuid-namespace-oid*)

(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; (defconst +uuid-null-string+  "00000000-0000-0000-0000-000000000000")
(defconst +uuid-null-string+
  ;; (type-of +uuid-null-string+)   => (SIMPLE-ARRAY CHARACTER (36))
  ;; (constantp +uuid-null-string+) => T
  (make-array 36 
              :element-type 'character
              :initial-contents "00000000-0000-0000-0000-000000000000"))

;;; ==============================
;; :NOTED intercepted at `uuid-digest-uuid-instance'.
;;
;; "When value is null `make-v3-uuid' and `make-v5-uuid' will error when their NAMESPACE
;; cause `make-v3-uuid' and `make-v5-uuid' to error when their argumement is an
;; instance with all of its slot-values satisfying `cl:zerop' but which doesn't
;; satisfy `unique-universal-identifier-null-p'. 
;;
;; :NOTE RFC 4122 probably didn't have CLOS in mind w/r/t the null-uuid and as
;; such when either of the following type of object is allowed as a NAMESPACE
;; argument it may produce unexpected results and with unexpected consequences
;; which may not be easily detected after the fact:
;; 
;;  (unique-universal-identifier-null-p (make-instance 'unique-universal-identifier))
;;  (unique-universal-identifier-null-p (make-instance 'unique-universal-identifier-null))
;;  (unique-universal-identifier-null-p (make-instance 'mop-frobbed-subclass-of-unique-universal-identifier))
;;
;; Their is a performance impact when this argument is nil the default b/c we
;; will have to check the slot-values of each namespace arg for each evaluation
;; of `make-v5-uuid' / `make-v3-uuid'.  A reasonable way to avoid this impact is
;; to cache the NAMESPACEs common to a class you control and dynamically bind
;; this variable inside a wrapper function e.g.
;;
;;  (make-v5-uuid-with-cached-namespace (name)
;;    (let ((*uuid-allow-null-like-namespace-args* t))
;;      (make-v5-uuid <CACHED-NAMESPACE> name)))
;; 
;; Note however that we are already doing this for the stuff in
;; unicly-extend.lisp because third parties are more liable to unknowingly
;; instantiate their sub-classes outside the constraints of the Unicly API.
;;
;;  
(defvar *uuid-allow-null-like-namespace-args* nil)

;;; ==============================

;; (vardoc '*uuid-allow-empty-string-name-args*
;; "When value is null `make-v3-uuid' and `make-v5-uuid' will error when their NAME
;;  argumement is not of type `unicly::string-not-empty'."
;;
(defvar *uuid-allow-empty-string-name-args* nil)


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
