;;; :FILE-CREATED <Timestamp: #{2011-08-17T15:28:02-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-integers.lisp
;;; ==============================


;;; ==============================
;; :NOTE ironclad utility functions
;;
;; ironclad:ub16ref/le buffer index => value
;; ironclad:ub32ref/le buffer index => value
;; ironclad:ub64ref/le buffer index => value
;;
;; This family of functions accesses an unsigned 16-bit, 32-bit or 64-bit value
;; stored in little-endian order starting at index in array. 
;; array must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)). These functions are SETFable.
;;
;; ironclad:ub16ref/be buffer index => value
;; ironclad:ub32ref/be buffer index => value
;; ironclad:ub64ref/be buffer index => value
;;
;; As the above, only the value is stored in big-endian order.
;;; ==============================


(in-package #:unicly)


;;; ==============================
;;; :PASTE-AUTHOR nyef -- Alistair Bridgewater
;;; :PASTE-TITLE Informing loop of integer size -- how to do it idiomatically?
;;; :PASTE 120426 :PASTE-URL (URL `http://paste.lisp.org/+2KX6/1')
(defun uuid-request-integer (array offset length &key little-endian sign-extend)
  (let ((value (loop
                  for i from 0 below length
                  for octet = (aref array (+ offset
                                             (if little-endian
                                                 i
                                                 (- length i 1))))
                  sum (ash octet (* i 8)))))
    (if (and sign-extend
             (logbitp (1- (* length 8)) value))
        (logior (lognot (1- (ash 1 (1- (* length 8))))) value)
        value)))
;;
(define-compiler-macro uuid-request-integer (&whole form array offset length &key little-endian sign-extend)
  ;; :NOTE the 4 is an (unsigned-byte 32) which isn't a fixnum on x86-32
  (if (and (member length '(1 2 4)) 
           (member little-endian '(t nil))
           (member sign-extend '(t nil)))
      `(let* (,@(loop
                   for i from 0 below length
                   for var in '(byte-0 byte-1 byte-2 byte-3)
                   collect `(,var (aref ,array (+ ,offset
                                                  ,(if little-endian
                                                       i
                                                       (- length i 1))))))
              (value ,(elt '(#1=byte-0
                             #2=(dpb byte-1 (byte 8 8) #1#)
                             #3=(dpb byte-2 (byte 8 16) #2#)
                             (dpb byte-3 (byte 8 24) #3#))
                           (1- length))))
         ,(if sign-extend
              `(if (logbitp ,(1- (* length 8)) value)
                   (logior ,(lognot (1- (ash 1 (1- (* length 8))))) value)
                   value)
              'value))
      form))

(declaim (inline uuid-disassemble-ub48))
(defun uuid-disassemble-ub48 (u48)
  (declare (uuid-ub48 u48)
           (optimize (speed 3)))
  (let ((b1 nil) (b2 nil) (b3 nil) (b4 nil) (b5 nil) (b6 nil))
    ;; :NOTE The setf/the junk may be ugly, but its certainly faster.
    (setf b1 (ldb (byte 8 40)  u48))
    (setf b2 (ldb (byte 8 32) u48))
    (setf u48 (mask-field (byte 32 0) (the uuid-ub48 u48)))
    (setf b3 (ldb (byte 8 24) (the uuid-ub32 u48)))
    (setf u48 (mask-field (byte 24 0) (the uuid-ub32 u48)))
    (setf b4 (ldb (byte 8 16) (the uuid-ub24 u48)))
    (setf b5 (ldb (byte 8  8) (the uuid-ub24 u48)))
    (setf b6 (ldb (byte 8  0) (the uuid-ub24 u48)))
    (locally 
        (declare (uuid-ub8 b1 b2 b3 b4 b5 b6))
      (values b1 b2 b3 b4 b5 b6))))

;;; ==============================
;; :SOURCE Zach Beane's usenet-legend/io.lisp 
;; `uuid-disassemble-ub32' :WAS `disassemble-u32'
;; `uuid-assemble-ub32' :WAS `assemble-u32'
(declaim (inline uuid-disassemble-ub32))
(defun uuid-disassemble-ub32 (u32)
  (declare (type uuid-ub32 u32)
           (optimize (speed 3)))
  (let ((b1 (ldb (byte 8 24) u32))
        (b2 (ldb (byte 8 16) u32))
        (b3 (ldb (byte 8  8) u32))
        (b4 (ldb (byte 8  0) u32)))
    (declare (uuid-ub8 b1 b2 b3 b4))
    (values b1 b2 b3 b4)))

(declaim (inline uuid-disassemble-ub16))
(defun uuid-disassemble-ub16 (u16)
  (declare (type uuid-ub16 u16)
           (optimize (speed 3)))
  (let ((b1 (ldb (byte 8 8) u16))
        (b2 (ldb (byte 8 0) u16)))
    (declare (uuid-ub8 b1 b2))
    (values b1 b2)))

(declaim (inline %uuid_byte-array-16-ub8-reqeust))
(defun %uuid_byte-array-16-ub8-reqeust (byte-array offset)
  ;; Only intended to be used in requests for octet values of
  ;; `uuid-byte-array-16's e.g. `uuid-from-byte-array'
  (declare (uuid-byte-array-16 byte-array)
           ((integer 8 9) offset)
           (optimize (speed 3)))
  (the uuid-ub8 (uuid-request-integer byte-array offset 1)))

(declaim (inline uuid-assemble-ub48))
(defun uuid-assemble-ub48 (b1 b2 b3 b4 b5 b6)
  (declare (type uuid-ub8 b1 b2 b3 b4 b5 b6)
           (optimize (speed 3)))
  (logand #xFFFFFFFFFFFF
          (logior (ash b1 40)
                  (ash b2 32)
                  (ash b3 24)
                  (ash b4 16)
                  (ash b5 8)
                  (ash b6 0))))

(declaim (inline uuid-assemble-ub32))
(defun uuid-assemble-ub32 (b1 b2 b3 b4)
  (declare (type uuid-ub8 b1 b2 b3 b4)
           (optimize speed))
  (logand #xFFFFFFFF
          (logior (ash b1 24)
                  (ash b2 16)
                  (ash b3 8)
                  (ash b4 0))))

;; (uuid-disassemble-ub32  #xFFFFFFFF)
;; 255, 255, 255, 255

;(declare (inline uuid-assemble-ub16))
(defun uuid-assemble-ub16 (b1 b2) 
  (declare (type uuid-ub8 b1 b2)
           (optimize (speed 3)))
  (logand #xFFFF
          (logior (ash b1 8)
                  (ash b2 0))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
