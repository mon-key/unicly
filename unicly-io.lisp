;;; :FILE-CREATED <Timestamp: #{2011-08-18T21:12:14-04:00Z}#{11334} - by MON>
;;; :FILE unicly/unicly-io.lisp
;;; ==============================

(in-package #:unicly)
;; *package*

(declaim (inline uuid-valid-stream-p))
(defun uuid-valid-stream-p (maybe-stream)
  (declare (optimize (speed 3)))
  (if (and (streamp maybe-stream)
           (open-stream-p (the stream maybe-stream)))
      (values t (the stream maybe-stream))
      (values nil maybe-stream)))

(defun uuid-valid-stream-verify-io-type (maybe-valid-io-stream &key direction)
  (declare ((member :input :output) direction)
           (inline uuid-valid-stream-p)
           (optimize (speed 3)))
  (multiple-value-bind (chk stream-val) (uuid-valid-stream-p maybe-valid-io-stream)
    (declare (boolean chk))
    (unless (if (eql direction :output)
                (and chk (output-stream-p (the stream stream-val)))
                (and chk (input-stream-p  (the stream  stream-val))))
      (error ":FUNCTION `UUID-VALID-STREAM-VERIFY-IO-TYPE' ~
              -- arg IN-STREAM failed check for either `uuid-valid-stream-p' or `cl:~(~A~)-stream-p'~%~T~
              got-val: ~S~%~Ttype-of: ~S~%"
             direction stream-val (type-of stream-val))))
  (the stream maybe-valid-io-stream))

(defun uuid-valid-stream-verify-for-output (out-stream)
  (declare (optimize (speed 3)))
  (the (values stream &optional)
    (uuid-valid-stream-verify-io-type out-stream :direction :output)))

(defun uuid-valid-stream-verify-for-input (in-stream)
  (declare (optimize (speed 3)))
  (the (values stream &optional)
    (uuid-valid-stream-verify-io-type in-stream :direction :input)))

;;; ==============================
;; :TODO `deserialize-uuid'... 
;; :NOTE Should there be a generic function which dispatches on the UUID's
;; representation , e.g. uuid-bit-vector-128, uuid-byte-array-20array-16,
;; unique-universal-identifier, uuid-string-32, uuid-string-36?
;; :NOTE Consider renaming this to `serialize-uuid-byte-array' and calling the
;; G-F in body.
(defun uuid-serialize-byte-array-bytes (uuid-or-byte-array-16 stream)
  (declare ((or uuid-byte-array-16 unique-universal-identifier) uuid-or-byte-array-16)
           (type stream stream)
           (optimize (speed 3)))
  (let ((ba16
         (the uuid-byte-array-16
           (if (uuid-byte-array-16-p uuid-or-byte-array-16) 
               uuid-or-byte-array-16
               (uuid-get-namespace-bytes uuid-or-byte-array-16)))))
    (declare (uuid-byte-array-16 ba16))
    (loop 
       for byte-idx from 0 below 16
       do (write-byte (aref ba16 byte-idx) stream))))

(defun uuid-serialize-bit-vector-bits (bv-or-uuid stream)
  (declare ((or uuid-bit-vector-128 unique-universal-identifier) bv-or-uuid))
  (loop 
     with ensure-bv = (if (unique-universal-identifier-p bv-or-uuid)
                          (uuid-to-bit-vector bv-or-uuid)
                          bv-or-uuid)
     ;; for bit-idx downfrom 127 to 0
     for bit-idx from  0 below 128 
     do (write-byte (aref ensure-bv  bit-idx)  stream)))

(defun uuid-deserialize-bit-vector-bits (stream)
  ;; :TODO Test (subtypep (stream-element-type stream) 'uuid-ub8)
  ;; :TODO Should peek at stream to test if we are at end of file
  ;; (error 'end-of-file :stream stream)
  ;; Should check if there are pututatively enough bytes remaining in stream with:
  ;; (>= (- file-length file-position) 128)
  ;; This won't be 100% reliable but will at least let us bail early for trivial cases.
  (let* ((stream-type (stream-element-type stream))
         (stream-subtypep (subtypep stream-type 'uuid-ub8)))
    (format t "stream-type: ~S~%stream-subtypep: ~S~%" stream-type stream-subtypep))
  (loop 
     with bv = (uuid-bit-vector-128-zeroed)
     for cnt from 0 below 128 
     for byte-read = (read-byte stream nil 'EOF)
     if (eql byte-read 'EOF)
     do (loop-finish)
     ;; (unless (typep byte-read 'bit) 
     ;;   (error "UUID-DESERIALIZE-BIT-VECTOR-BITS -- CL:READ-BYTE read object not of type CL:BIT~%~Tgot: ~S~%~Ttype-of: ~S~%"
     ;;          byte-read (type-of byte-read)))
     do (setf (sbit bv cnt) byte-read)
     finally (return bv)))

;; Write bits of BV-OR-UUID to OUTPUT-PATHNAME return OUTPUT-PATHNAME.
;; :EXAMPLE
;;  (uuid-write-bit-vector-bits (make-v4-uuid) (make-pathname :directory '(:absolute  "tmp") :name "bitstream-test"))
;;
(defun uuid-write-bit-vector-bits (bv-or-uuid output-pathname &key (if-exists :supersede) 
                                                      (if-does-not-exist :create))
  (declare ((or uuid-bit-vector-128 unique-universal-identifier) bv-or-uuid)
           ((or pathname string) output-pathname))
  (with-open-file (bv-out output-pathname
                       :direction :output
                       :if-exists if-exists 
                       :if-does-not-exist if-does-not-exist
                       :element-type 'uuid-ub8)
    (uuid-serialize-bit-vector-bits bv-or-uuid bv-out)
    output-pathname))


;; with-open-stream

;; (fundoc 'uuid-read-bit-vector-bits
;; Read the bits of a UUID's bit-vector representation from INPUT-PATHNAME return
;; an object of type `uuid-bit-vector-128'.
;; INPUT-PATHNAME names an existing file with element-type `uuid-ub8'.
;; :EXAMPLE
;;  (let* ((tmp (make-pathname :directory '(:absolute  "tmp") 
;;                             :name "bitstream-test"))
;;         (v4     (uuid-to-bit-vector (make-v4-uuid)))
;;         (v4-io  (uuid-read-bit-vector-bits (uuid-write-bit-vector-bits v4 tmp))))
;;    (uuid-bit-vector-eql v4 v4-io))
(defun uuid-read-bit-vector-bits (input-pathname &key (if-does-not-exist :error))
  (declare ((or pathname string) input-pathname))
  (with-open-file (bv-in input-pathname
                         :direction :input
                         :if-does-not-exist if-does-not-exist
                         :element-type 'uuid-ub8)
    (uuid-deserialize-bit-vector-bits bv-in)))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
