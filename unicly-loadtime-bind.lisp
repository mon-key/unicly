;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:43:39-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-loadtime-bind.lisp
;;; ==============================


(in-package #:unicly)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (setf *uuid-namespace-dns*  (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))

  (setf *uuid-namespace-url*  (make-uuid-from-string "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))

  (setf *uuid-namespace-oid*  (make-uuid-from-string "6ba7b812-9dad-11d1-80b4-00c04fd430c8"))

  (setf *uuid-namespace-x500* (make-uuid-from-string "6ba7b814-9dad-11d1-80b4-00c04fd430c8"))
  
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
