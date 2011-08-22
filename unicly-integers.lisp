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
;; :SOURCE Zach Beane's usenet-legend/io.lisp :WAS `disassemble-u32'
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

;; (uuid-u48-from-bytes (b5 b4 b3 b2 b1 b0)
;; (declare optimize

;;; ==============================
;; (defun uuid-ub32-from-bytes (b3 b2 b1 b0)
;;   ;; (declare (optimize (speed 3))
;;   ;; 	   (uuid-ub8 b3 b2 b1 b0))
;;   (logxor (ash b3 24)
;; 	     (ash b2 16)
;; 	     (ash b1 8)
;;      b0))

;; (defun uuid-ub16-from-bytes (b1 b0)
;; (declare (uuid-ub8 b3 b2 b1 b0)
;; 	   (optimize (speed 3)))
;; (the uuid-ub16 (logxor (ash b1 8) b0))
;; (logxor (ash 255 8) 255)

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
