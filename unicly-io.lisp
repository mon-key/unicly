;;; :FILE-CREATED <Timestamp: #{2011-08-18T21:12:14-04:00Z}#{11334} - by MON>
;;; :FILE unicly/unicly-io.lisp
;;; ==============================

(in-package #:unicly)
;; *package*

;;; ==============================
;; :TODO `deserialize-uuid'... 
;; :NOTE Should there be a generic function which dispatches on the UUID's
;; representation , e.g. uuid-bit-vector-128, uuid-byte-array-20array-16,
;; unique-universal-identifier, uuid-string-32, uuid-string-36?
;; :NOTE Consider renaming this to `serialize-uuid-byte-array' and calling the
;; G-F in body.
(defun uuid-serialize-byte-array (uuid-or-byte-array-16 stream)
  (declare ((or uuid-byte-array-16 unique-universal-identifier) uuid-or-byte-array-16)
           (type stream stream)
           (optimize (speed 3)))
  (loop 
     with uuid-bytes = (the uuid-byte-array-16
                         (if (uuid-byte-array-16-p uuid-or-byte-array-16) 
                             uuid-or-byte-array-16
                             (uuid-get-namespace-bytes uuid-or-byte-array-16)))
     for i from 0 below 16
     do (write-byte (aref uuid-bytes i) stream)))

(defun uuid-serialize-bit-vector (bv-or-uuid stream)
  (declare ((or uuid-bit-vector-128 unique-universal-identifier) bv-or-uuid))
  (loop 
     with ensure-bv = (if (unique-universal-identifier-p bv-or-uuid)
                          (uuid-to-bit-vector bv-or-uuid)
                          bv-or-uuid)
     ;; for bit-idx from 128 0 below 128 
     for bit-idx downfrom 127 to 0
     do (write-byte (aref ensure-bv  bit-idx)  stream)))

(defun uuid-write-bit-vector (bv-or-uuid bv-path &key (if-exists :supersede) 
                                                      (if-does-not-exist :create))
  ;; (uuid-write-bit-vector *stream-uuid-bv* (make-pathname :directory '(:absolute  "tmp") :name "bitstream-test"))
  (declare ((or uuid-bit-vector-128 unique-universal-identifier) bv-or-uuid)
           ((or pathname string) bv-path))
  (with-open-file (out bv-path
                       :direction :output
                       ;; :direction :input
                       :if-exists if-exists 
                       :if-does-not-exist if-does-not-exist
                       :element-type '(unsigned-byte 8))
    (uuid-serialize-bit-vector bv-or-uuid out)
    bv-path))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
