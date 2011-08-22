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

;; :EXAMPLE
;; (let ((os (make-string-output-stream)))
;;   (uuid-valid-stream-verify-octet-stream-for-output os))
;;
;; (let ((is (make-string-input-stream "bubba")))
;;   (uuid-valid-stream-verify-octet-stream-for-input is))
(defun uuid-valid-stream-verify-io-octet-type (maybe-octet-stream &key direction)
  (declare ((member :input :output) direction))
  (let* ((chk-stream 
          (ecase direction
            (:output (uuid-valid-stream-verify-for-output maybe-octet-stream))
            (:input  (uuid-valid-stream-verify-for-input maybe-octet-stream))))
         (elt-type    (stream-element-type (the stream chk-stream))))
    (multiple-value-bind (t1 t2) (subtypep elt-type 'uuid-ub8)
      (declare (boolean t1 t2))
      (unless (and t1 t2)
        (error "UUID-VALID-STREAM-VERIFY-IO-OCTET-TYPE -- arg MAYBE-OCTET-STREAM ~
                has `cl:stream-element-type' not `cl:subtypep' of `uuid-ub8'~%~Twith-stream: ~S~%~Twith-type: ~S~%"
               chk-stream elt-type))))
  (the stream maybe-octet-stream))

(defun uuid-valid-stream-verify-octet-stream-for-output (maybe-output-octet-stream)
  (uuid-valid-stream-verify-io-octet-type maybe-output-octet-stream :direction :output))

(defun uuid-valid-stream-verify-octet-stream-for-input (maybe-input-octet-stream)
  (uuid-valid-stream-verify-io-octet-type maybe-input-octet-stream :direction :input))

;;; ==============================
;; :TODO `deserialize-uuid'... 
;; :NOTE Should there be a generic function which dispatches on the UUID's
;; representation , e.g. uuid-bit-vector-128, uuid-byte-array-20array-16,
;; unique-universal-identifier, uuid-string-32, uuid-string-36?
;; :NOTE Consider renaming this to `serialize-uuid-byte-array' and calling the
;; G-F in body.
(defun uuid-serialize-byte-array-bytes (uuid-or-byte-array-16 stream-out)
  (declare ((or uuid-byte-array-16 unique-universal-identifier) uuid-or-byte-array-16)
           (type stream stream-out)
           (optimize (speed 3)))
  (uuid-valid-stream-verify-octet-stream-for-output stream-out)
  (let ((ba16
         (the uuid-byte-array-16
           (if (uuid-byte-array-16-p uuid-or-byte-array-16) 
               uuid-or-byte-array-16
               (uuid-get-namespace-bytes uuid-or-byte-array-16)))))
    (declare (uuid-byte-array-16 ba16))
    ;; (loop 
    ;;    for byte-idx from 0 below 16
    ;;    do (write-byte (aref ba16 byte-idx) stream))
    (write-sequence ba16 stream-out :start 0 :end 16)))
;; *print-array* 
(defun uuid-deserialize-byte-array-bytes (stream-in)
  (uuid-valid-stream-verify-octet-stream-for-input stream-in)
  ;; :NOTE Following idiom does not suitably catch EOF.
  ;; (let ((bv-return (uuid-bit-vector-128-zeroed)))
  ;;    (read-sequence bv-return stream :start 0 :end 127)
  ;;    bv-return))
  (loop 
     with ba16 = (uuid-byte-array-16-zeroed)
     for ba16-idx from 0 below 16
     for byte-read = (read-byte stream-in nil 'EOF)
     if (eql byte-read 'EOF)
     do (error 'end-of-file :stream stream-in)
     end
     ;; unless (typep byte-read 'bit) ;; catches new line just prior to EOF...
     ;; do (error "UUID-DESERIALIZE-BIT-VECTOR-BITS -- CL:READ-BYTE read object not of type CL:BIT~%~Tgot: ~S~%~Ttype-of: ~S~%"
     ;; byte-read (type-of byte-read))
     do (setf (aref ba16 ba16-idx) byte-read)
     finally (return ba16)))

(defun uuid-serialize-bit-vector-bits (bv-or-uuid stream-out)
  (declare ((or uuid-bit-vector-128 unique-universal-identifier) bv-or-uuid)
           (type stream stream-out))
  (uuid-valid-stream-verify-octet-stream-for-output stream-out)
  (let ((bv-128  (the uuid-bit-vector-128
                   (if (unique-universal-identifier-p bv-or-uuid)
                       (uuid-to-bit-vector bv-or-uuid)
                       bv-or-uuid))))
    (declare (uuid-bit-vector-128 bv-128))
    ;; (loop 
    ;;    ;; for bit-idx downfrom 127 to 0
    ;;    for bit-idx from  0 below 128 
    ;;    do (write-byte (sbit bv-128 bit-idx) stream-out))
    (write-sequence bv-128 stream-out :start 0 :end 128)))

;; :TODO Test (subtypep (stream-element-type stream) 'uuid-ub8)
;; :TODO Should peek at stream to test if we are at end of file
;; (error 'end-of-file :stream stream)
;; Should check if there are pututatively enough bytes remaining in stream with:
;; (>= (- file-length file-position) 128)
;; This won't be 100% reliable but will at least let us bail early for trivial cases.
;;
;; (let* ((stream-type (stream-element-type stream))
;;        (stream-subtypep (subtypep stream-type 'uuid-ub8)))
;;   (format t "stream-type: ~S~%stream-subtypep: ~S~%" stream-type stream-subtypep))
(defun uuid-deserialize-bit-vector-bits (stream-in)
  (uuid-valid-stream-verify-octet-stream-for-input stream-in)
  ;; :NOTE Following idiom with `cl:read-sequence' does not suitably catch EOF.
  ;; (let ((bv-return (uuid-bit-vector-128-zeroed)))
  ;;    (read-sequence bv-return stream :start 0 :end 127)
  ;;    bv-return))
  ;;
  ;; Following is the equivalent using `cl:do' instead of `cl:loop'
  ;; (let ((bv (uuid-bit-vector-128-zeroed)))
  ;;   (do ((code (read-byte s nil 'EOF) (read-byte s nil 'EOF))
  ;;        (cnt 0 (1+ cnt)))
  ;;       ((if (or (eql code 'EOF) (> cnt 128 )) 
  ;;            t
  ;;            (unless (typep code 'bit)
  ;;              (error "UUID-DESERIALIZE-BIT-VECTOR-BITS -- CL:READ-BYTE read object not of type CL:BIT~%~Tgot: ~S~%~Ttype-of: ~S~%"
  ;;                     code (type-of code))))
  ;;        (if (= cnt 128)
  ;;            (uuid-bit-vector-eql w-uuid-bv bv)
  ;;            ;; (values nil (cons code cnt))))
  ;;            (error 'end-of-file :stream stream-in)))
  ;;     (setf (sbit bv cnt) code)))
  ;;
  (loop 
     with bv = (uuid-bit-vector-128-zeroed)
     for cnt from 0 below 128 
     for byte-read = (read-byte stream-in nil 'EOF)
     if (eql byte-read 'EOF)
     do (error 'end-of-file :stream stream-in)
     end
     unless (typep byte-read 'bit) ;; catches new line just prior to EOF...
     do (error "UUID-DESERIALIZE-BIT-VECTOR-BITS -- CL:READ-BYTE read object not of type CL:BIT~%~Tgot: ~S~%~Ttype-of: ~S~%"
               byte-read (type-of byte-read))
     do (setf (sbit bv cnt) byte-read)
     finally (return bv)))

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
