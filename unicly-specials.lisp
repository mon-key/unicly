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


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
