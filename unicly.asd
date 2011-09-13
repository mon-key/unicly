;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:41:41-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly.asd
;;; ==============================

;; ,----
;; | "I am sick to death of knee-jerk anti-LOOPism and I am beginning to
;; |  irrationally regard it as a plot to disable me as a programmer by
;; |  excommunicating my useful tools."
;; |
;; |     :SOURCE "Knee-jerk Anti-LOOPism and other E-mail Phenomena" p 17 
;; `---- :SEE http://ccs.mit.edu/papers/CCSWP150.html



(defpackage #:unicly-asd (:use #:common-lisp #:asdf))

(in-package #:unicly-asd)

(defvar *author-maintainer-contact* (format nil "MON KEY -- ~A"
                                 (map 'string
                                      'code-char
                                      #(109 111 110 107 101 121 64 115 97 110 100 112 
                                        102 114 97 109 105 110 103 46 99 111 109))))

(defsystem #:unicly
  :name "unicly"
  :licence "MIT"
  :version "2011.08.31"
  ;; :maintainer "MON KEY"
  :maintainer #.*author-maintainer-contact*
  :description "UUID Generation per RFC 4122"
  :long-description "UUID implementation for Common Lisp as per RFC 4122"
  :serial t
  :depends-on (:ironclad 
               :split-sequence
               #-:sbcl :flexi-streams
               #+:sbcl :sb-unicode)
  :components ((:file "package")
               (:file "unicly-specials")
               (:file "unicly-bridge")
               (:file "unicly-utils")
               (:file "unicly-macros")
               (:file "unicly-types")
               (:file "unicly-class")
               (:file "unicly-conditions")
               (:file "unicly-integers")
               (:file "unicly-byte-arrays")
               (:file "unicly-bit-vectors")
               (:file "unicly")
               (:file "unicly-string-uuid")
               (:file "unicly-uuid-version")
               (:file "unicly-hash-table")
               (:file "unicly-io")
               (:file "unicly-docs" )
               ;; (:file "unicly-compat")
               ;; (:file "unicly-deprecated")
               ))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :unicly))))
  (pushnew :unicly cl:*features*)
  (let ((chk-if (probe-file (make-pathname 
                             :directory `(,@(pathname-directory (truename (asdf:system-source-directory :unicly))))
                             :name "unicly-loadtime-bind" :type "lisp"))))
    (and chk-if (load  chk-if))))


;;; ==============================
;;; EOF
