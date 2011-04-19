;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:41:41-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly.asd
;;; ==============================

(defpackage #:unicly-asd (:use #:common-lisp #:asdf))

(in-package #:unicly-asd)

(defsystem #:unicly
  :name "unicly"
  :licence "LLGPL"
  :version "04-18-2011"
  :maintainer "MON KEY"
  :description "UUID Generation"
  :long-description "Lisp implementation of RFC 4122"
  :serial t
  :depends-on (:ironclad 
               :split-sequence
               #-sbcl :flexi-streams)
  :components ((:file "package")
               (:file "unicly-specials")
               (:file "unicly-utils")
               (:file "unicly-types")
               (:file "unicly-class")
               (:file "unicly"      )
               (:file "unicly-docs" )
               ;; (:file "unicly-deprecated")
               ))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :unicly))))
  (pushnew :unicly cl:*features*)
  (let ((chk-if (probe-file (make-pathname 
                             :directory `(,@(pathname-directory (truename (asdf:system-source-directory :unicly))))
                             :name "unicly-loadtime-bind" :type "lisp"))))
    (and chk-if (load  chk-if)))
  )

;;; ==============================
;;; EOF
