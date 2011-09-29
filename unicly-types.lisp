;;; :FILE-CREATED <Timestamp: #{2011-04-11T11:51:10-04:00Z}#{11151} - by MON>
;;; :FILE unicly/unicly-types.lisp
;;; ==============================


(in-package #:unicly)
;; *package*

;; simple-type
(deftype uuid-version-int ()
  '(mod 6))

;; simple-type
;; (funcall #'(lambda (x) (declare (uuid-v3-4-or-5-int x) (optimize (speed 3)))  x) 6)
(deftype uuid-v3-4-or-5-int ()
  '(and uuid-version-int (integer 3 5)))

;; simple-type
;; (funcall #'(lambda (x) (declare (uuid-v3-or-5-int x) (optimize (speed 3)))  x) 6)
;; (typep 3 'uuid-v3-or-5-int)
;; (typep 5 'uuid-v3-or-5-int)
;; (and (null (typep 4 'uuid-v3-or-5-int)))
(deftype uuid-v3-or-5-int ()
  '(and uuid-v3-4-or-5-int (not (integer 4 4))))

;; simple-type
;; (funcall #'(lambda (x) (declare (uuid-ub48 x) (optimize (speed 3)))  x) #XFFFFFFFFFFFF0)
(deftype uuid-unsigned-byte-size (byte-size)
  `(unsigned-byte ,byte-size))
;; uuid-ub<N> uuid-ub8
(def-uuid-unsigned-byte-size 128)
(def-uuid-unsigned-byte-size  64)
(def-uuid-unsigned-byte-size  48)
(def-uuid-unsigned-byte-size  32)
(def-uuid-unsigned-byte-size  24)
(def-uuid-unsigned-byte-size  16)
(def-uuid-unsigned-byte-size   8)

;; simple-type
(deftype uuid-unsigned-byte-integer-length (integer)
  `(mod ,integer))
;; uuid-ub<N>-integer-length uuid-ub128-integer-length
(def-uuid-unsigned-byte-integer-length 128)
(def-uuid-unsigned-byte-integer-length  64)
(def-uuid-unsigned-byte-integer-length  49)
(def-uuid-unsigned-byte-integer-length  33)
(def-uuid-unsigned-byte-integer-length  25)
(def-uuid-unsigned-byte-integer-length   9)

;;; ==============================
;; 
;; (deftype uuid-fixnum-bit-width ()
;;   #-sbcl '(integer 0 #.(integer-length most-positive-fixnum))
;;   #+sbcl '(integer 0 #.sb-vm:n-positive-fixnum-bits))

;; (deftype uuid-bignum-bit-width ()
;;   #-sbcl '(integer #.(integer-length most-positive-fixnum) *)
;;   ;; :NOTE x86-32 only
;;   ;; #+sbcl '(integer #.sb-vm:n-positive-fixnum-bits #.(- (expt 2 24) 2))
;;   ;; not following range is 1 under sb-bignum::maximum-bignum-length for congruency with the x86-32 above.
;;   ;; I'm assuming this is correct for other procs/architectures...
;;   #+sbcl '(integer #.sb-vm:n-positive-fixnum-bits #.(1- sb-bignum::maximum-bignum-length)))

;;; ==============================
;; :NOTE (upgraded-array-element-type (type-of (make-array 128 :element-type 'bit :initial-element 0))) => T
(deftype uuid-bit-vector (&optional size)
  (let ((sz (or size '*)))
    `(simple-bit-vector ,sz)))
;; uuid-bit-vector-<N> uuid-bit-vector-128
(def-uuid-bit-vector-N-type 128)
(def-uuid-bit-vector-N-type  48)
(def-uuid-bit-vector-N-type  32)
(def-uuid-bit-vector-N-type  16)
(def-uuid-bit-vector-N-type   8)

;; simple-type
;; (funcall #'(lambda (x) (declare (uuid-bit-vector-128-length x) (optimize (speed 3)))  x) 129)
(deftype uuid-bit-vector-length (size)
  `(integer ,size ,size))
;; uuid-bit-vector-<N>-length
(def-uuid-bit-vector-length-type 128)
(def-uuid-bit-vector-length-type  48)
(def-uuid-bit-vector-length-type  32)
(def-uuid-bit-vector-length-type  16)
(def-uuid-bit-vector-length-type   8)

;; simple-type
;; (funcall (lambda (x) (declare (uuid-bit-vector-valid-length x) (optimize (speed 3))) x) 129)
(deftype uuid-bit-vector-valid-length ()
  '(or 
    uuid-bit-vector-128-length
    uuid-bit-vector-48-length
    uuid-bit-vector-32-length
    uuid-bit-vector-16-length
    uuid-bit-vector-8-length))

;; complex-type
;; (funcall (lambda (x) (declare (uuid-bit-vector-valid-bit-offset x) (optimize  (speed 3) (safety  2))) x) 18)
(deftype uuid-bit-vector-valid-bit-offset ()
  '(member 0 32 48 64 72 80))

;; complex-type
;; (funcall (lambda (x) (declare (uuid-bit-vector-valid-bit-width x) (optimize  (speed 3))) x) 18)
(deftype uuid-bit-vector-valid-bit-width ()
  '(or 
    uuid-bit-vector-48-length
    uuid-bit-vector-32-length
    uuid-bit-vector-16-length
    uuid-bit-vector-8-length))

;; simple-type
;; (funcall (lambda (x) (declare ((uuid-bit-vector-index 3) x) (optimize (speed 3))) x) 4)
(deftype uuid-bit-vector-index (index-range)
  `(mod ,index-range))

;; complex-type
;; (funcall (lambda (x) (declare (uuid-bit-vector-null  x) (optimize (speed 3))) x)
;;          (uuid-to-bit-vector (make-v4-uuid)))
(deftype uuid-bit-vector-null ()
  '(and uuid-bit-vector-128
    (satisfies %uuid-bit-vector-null-p)))

;; :NOTE This is entirely equivalent to the type `ironclad::simple-octet-vector'
;; So long as Unicly has an ironclad dependency maybe we should just use that instead? :)
;; complex-type 
(deftype uuid-byte-array (&optional size)
  (let ((sz (or size '*)))
    `(simple-array uuid-ub8 (,sz))))

;; (mon:type-expand-all 'uuid-byte-array-16)
;; uuid-byte-array-<N>
(def-uuid-byte-array-length 20)
(def-uuid-byte-array-length 16)

;; complex-type
(deftype uuid-byte-array-null ()
  ;; expands to: (simple-array (unsigned-byte 8) (16)))
  '(and uuid-byte-array-16 (satisfies %uuid-byte-array-null-p)))

;; complex-type
(deftype uuid-simple-vector-5 ()
  '(simple-vector 5))

(deftype uuid-simple-string-vector-5 ()
  #-:lispworks '(simple-array simple-string (5))
  #+:lispworks 'uuid-simple-vector-5)

;; complex-type
(deftype uuid-string-32 ()
  #-:lispworks '(array character (32))
  #+:lispworks '(simple-string-n-length-compat 36))

;; complex-type
(deftype uuid-string-36 ()
  #-:lispworks '(array character (36))
  #+:lispworks '(string-n-length-compat 36))
 
;; complex-type
;; currently unused :SEE `uuid-get-bytes' in unicly/unicly-deprecated.lisp
(deftype uuid-byte-string ()
  #-:lispworks '(simple-array character (16))
  #+:lispworks '(simple-string-n-length-compat 16))

;;; ==============================
;; :NOTE These notes are w/r/t method `uuid-print-bytes-to-string' specialized
;; on `unique-universal-identifier'. Currently the internal string variable is 
;; an object of type:
;;  (AND (BASE-STRING 32) (NOT SIMPLE-ARRAY))
;; That decision likely originally had something to do with some notion i had re
;; `ironclad:byte-array-to-hex-string' and `simple-base-string's. 
;; In any event, if we tweak `uuid-print-bytes-to-string' we need to be mindful
;; that underlying callers/methods may check `uuid-hex-string-<N>' and might
;; fail if they don't find the correct array-element-type...
;; 2011-08-20
;; <mon_key> http://paste.lisp.org/+2NT1
;; <pkhuong> mon_key: see clhs 15.1.2.1  [17:22]
;; <mon_key> pkhuong: Thanks. So the initial #\NUL base-char is permissible
;; 	  because the initally allocated array has base-char as its
;; 	  upgraded-array-element-type ?  [17:27]
;; <mon_key> e.g. (upgraded-array-element-type (array-element-type (make-array 16
;; 	  :element-type 'standard-char)))  [17:29]
;; <pkhuong> mon_key: no, it's permissible because it's undefined what happens if
;; 	  you manage to observe it.
;; <mon_key> Schrodingers cat?  [17:33]
;; <Xach> mon_key: aref on an uninitialized array before setf of the element in
;;        question is undefined
;;; ==============================
;; complex-type
(deftype uuid-hex-string-32 ()
  '(and uuid-string-32 (satisfies string-all-hex-char-p)))

;; :NOTE The satisfies check for `uuid-hex-string-36-p' is somewhat costly and
;; need only be evaluated once...
;; Forms which rely on declaring `uuid-hex-string-12', `uuid-hex-string-8',
;; `uuid-hex-string-4', and `uuid-hex-string-2' do so as in-body
;; micro-optimizations and should have a prior declarartion of
;; `uuid-hex-string-36' earlier in the call chain such that, if/when necessary,
;; some later form in its body will then coerce a subseq of the declared
;; `uuid-hex-string-36' to a simple-string and therafter we only need to declare
;; a uuid-hex-string-length for subseqs of the uuid-hex-string-36.
(deftype uuid-hex-string-36 ()
  '(and uuid-string-36 (satisfies uuid-hex-string-36-p)))

(deftype uuid-hex-string-length (string-length)
  #-:lispworks `(simple-array character (,string-length))
  #+:lispworks `(or (simple-string-n-length-compat ,string-length)
                    (string-n-length-compat ,string-length)))

(def-uuid-uuid-hex-string-length 12)
(def-uuid-uuid-hex-string-length  8)
(def-uuid-uuid-hex-string-length  4)
(def-uuid-uuid-hex-string-length  2)



;;; ==============================
;;; :UUID-TYPE-PREDICATES
;;; ==============================
(declaim (inline uuid-bit-vector-128-p
                 uuid-bit-vector-128-check-type
                 uuid-bit-vector-48-p
                 uuid-bit-vector-32-p
                 uuid-bit-vector-16-p
                 uuid-bit-vector-8-p
                 uuid-string-32-p
                 uuid-string-36-p
                 uuid-string-36-check-type
                 uuid-byte-array-p
                 uuid-byte-array-16-p
                 uuid-byte-array-16-check-type
                 uuid-byte-array-20-p
                 uuid-byte-string-p
                 uuid-simple-vector-5-p
                 string-with-fill-pointer-p
                 string-with-fill-pointer-check-type
                 ))
;; uuid-bit-vector-<N>-p uuid-bit-vector-<N>-check-type
(def-uuid-predicate-and-type-check-definer uuid-bit-vector-128)
(def-uuid-predicate-and-type-check-definer uuid-bit-vector-48)
(def-uuid-predicate-and-type-check-definer uuid-bit-vector-32)
(def-uuid-predicate-and-type-check-definer uuid-bit-vector-16)
(def-uuid-predicate-and-type-check-definer uuid-string-32)
(def-uuid-predicate-and-type-check-definer uuid-string-36)
(def-uuid-predicate-and-type-check-definer uuid-byte-array)
(def-uuid-predicate-and-type-check-definer uuid-byte-array-16)
(def-uuid-predicate-and-type-check-definer uuid-byte-array-20)
(def-uuid-predicate-and-type-check-definer uuid-byte-string)
(def-uuid-predicate-and-type-check-definer uuid-simple-vector-5)
(def-uuid-predicate-and-type-check-definer string-with-fill-pointer)

;;; :NOTE Following definition is unused on the assumption that it is guaranteed
;;;  that the following always returns (SIMPLE-BIT-VECTOR 0) on all implementations:
;;;  (type-of (make-array 0 :element-type 'bit :initial-element 0))
(defun uuid-verify-bit-vector-simplicity (putative-simple-bit-vector)
  (declare (bit-vector putative-simple-bit-vector)
           (optimize (speed 3)))
  (and (eql (array-element-type putative-simple-bit-vector) 'bit)
       (null (adjustable-array-p putative-simple-bit-vector))
       (null (array-has-fill-pointer-p putative-simple-bit-vector))))

(declaim (inline %uuid-byte-array-null-p))
(defun %uuid-byte-array-null-p (byte-array-maybe-null)
  ;; (%uuid-byte-array-null-p (uuid-byte-array-16-zeroed))
  ;; (%uuid-byte-array-null-p (make-array 16 :element-type 'uuid-ub8 :initial-element 1))
  (declare (inline uuid-byte-array-16-check-type)
           (optimize (speed 3)))
  (uuid-byte-array-16-check-type byte-array-maybe-null)
  (locally 
      (declare (uuid-byte-array-16 byte-array-maybe-null))
    (loop for x across byte-array-maybe-null always (zerop x))))
;;
;; (uuid-byte-array-16-check-type (make-array 20 :element-type 'uuid-ub8 :initial-element 1))
;; (%uuid-byte-array-null-p (make-array 20 :element-type 'uuid-ub8 :initial-element 0))

(declaim (inline uuid-byte-array-null-p))
(defun uuid-byte-array-null-p (byte-array-maybe-null)
  ;; (uuid-byte-array-null-p (make-array 20 :element-type 'uuid-ub8 :initial-element 1))
  (declare (inline %uuid-byte-array-null-p)
           (optimize (speed 3)))
  (typep byte-array-maybe-null 'uuid-byte-array-null))

;; uuid-hex-string-32 has a satisfies of `string-all-hex-char-p'
(defun uuid-hex-string-32-p (maybe-uuid-hex-string-32)
  (typep maybe-uuid-hex-string-32 'uuid-hex-string-32))

;; uuid-hex-string-36

(declaim (inline uuid-delimited-string-36-p))
(defun uuid-delimited-string-36-p (maybe-delim-string-36)
  (declare (inline uuid-string-36-p)
           (optimize (speed 3)))
  (unless (uuid-string-36-p maybe-delim-string-36)
    (return-from uuid-delimited-string-36-p (the boolean nil)))
  (labels ((char-dash-p (maybe-dash)
             (declare (char-compat maybe-dash))
             (char= #\- maybe-dash))
           (delimit-seq (string-seq offset length)
             (declare (uuid-string-36 string-seq)
                      ((mod 25) offset)
                      ((mod 13) length))
             (let ((rtn-sub (make-array length :element-type 'character))
                   (sub-end (+ offset length)))
               (declare ((mod 37) sub-end))
               (loop 
                  for source-idx from offset below sub-end 
                  for dest-idx from 0 below length
                  do (setf (char rtn-sub dest-idx)
                           (char string-seq source-idx))
                  finally (return (the simple-string-compat rtn-sub)))))
           (delimited-subseqs (string-for-subs)
             (declare (uuid-string-36 string-for-subs))
             (the uuid-simple-string-vector-5
               #+:lispworks 
               (make-array 5 
                           :element-type 'system:simple-augmented-string
                           :initial-contents (list 
                                              (the uuid-hex-string-8  (delimit-seq string-for-subs 0  8))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 9  4))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 14 4))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 19 4))
                                              (the uuid-hex-string-12 (delimit-seq string-for-subs 24 12))))
               #-:lispworks
               (make-array 5 
                           :element-type 'simple-string 
                           :initial-contents (list 
                                              (the uuid-hex-string-8  (delimit-seq string-for-subs 0  8))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 9  4))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 14 4))
                                              (the uuid-hex-string-4  (delimit-seq string-for-subs 19 4))
                                              (the uuid-hex-string-12 (delimit-seq string-for-subs 24 12)))))))
    (declare (uuid-string-36 maybe-delim-string-36))
    (loop 
       initially  (unless (loop
                             for char across maybe-delim-string-36
                             thereis (char= #\- char))
                    (loop-finish))
       for delim-char across maybe-delim-string-36
       for idx from 0 below 36  
       when (char-dash-p delim-char)
       ;; At end we want (+ <CNT> <PSNS>) => 66 e.g. (+ 4  23 18 13 8) => 66 
       ;; The maximum upper bounds of cnt is 36  -- an (unsigned-byte 6)
       ;; The maximum upper bounds of cnt is 630 -- an (unsigned-byte 10)
       ;; These can only occur if MAYBE-DELIM-STRING-36 is cl:string= 
       ;; the return-value of (make-string 36 :initial-element #\-)
       count delim-char into cnt of-type (unsigned-byte 6)
       and sum idx into psns of-type (unsigned-byte 10)
       finally (return 
                 (if (and (= cnt 4)
                          (= psns 62))
                     (values (the boolean t)
                             (the uuid-simple-string-vector-5 ;; uuid-simple-vector-5
                               (delimited-subseqs maybe-delim-string-36)))
                     (the boolean nil))))))

(declaim (inline %uuid-hex-string-36-null-string-p))
(defun %uuid-hex-string-36-null-string-p (split-vec)
  (declare (uuid-simple-vector-5 split-vec)
           (optimize (speed 3)))
  (uuid-simple-vector-5-check-type split-vec)
  (labels 
      ((all-zero-char-p (split-string)
         (declare (simple-string-compat split-string))
         (loop 
            for maybe-zero across split-string
            always (char= #\0 maybe-zero)))
       (all-strings-zero-p ()
         (loop 
            for split of-type simple-string-compat across split-vec
            always (all-zero-char-p split))))
    (all-strings-zero-p)))
;;
;; (%uuid-hex-string-36-null-string-p #(0 0 0 0))
;; (deftype uuid-hex-string-36-zeroed ()
;;   '(satisfies %uuid-hex-string-36-null-string-p))

(declaim (inline uuid-hex-string-36-p))
(defun uuid-hex-string-36-p (maybe-uuid-hex-string-36)
  (declare (inline string-all-hex-char-p
                   uuid-string-36-p
                   %uuid-hex-string-36-null-string-p
                   uuid-delimited-string-36-p)
           (optimize (speed 3)))
  (when (uuid-string-36-p maybe-uuid-hex-string-36)
    (multiple-value-bind (is-36 split-36) (uuid-delimited-string-36-p (the uuid-string-36 maybe-uuid-hex-string-36))
      (when is-36
        (locally 
            (declare (type uuid-simple-string-vector-5 split-36))
          (when (loop 
                   ;; for split of-type simple-string-compat across (the uuid-simple-string-vector-5 split-36)
                   for split of-type simple-string-compat across split-36
                   always (string-all-hex-char-p split))
            (if (%uuid-hex-string-36-null-string-p  split-36)
                (values (the boolean t) (the function #'make-null-uuid)) ;; #'make-null-uuid);; (the function #'make-null-uuid))
                (values (the boolean t) (the uuid-simple-vector-5 split-36)))))))))

;; (typep #'make-null-uuid 'compiled-function)
;; (uuid-hex-string-36-p (uuid-princ-to-string (make-v4-uuid)))
;; (type-of (svref #("e3115c49" "6e13" "4d21" "9a37" "a1af250a8f88") 0))
;; (simple-array-chacter (8)) 8 4 4 4 12)


;;; ==============================
;;; :REPLACED-WITH-MACROLOGY
;;; ==============================

;;; ==============================
;; (deftype uuid-ub128 ()
;;  '(uuid-unsigned-byte-size 128))
;; (deftype uuid-ub64 ()
;;  '(uuid-unsigned-byte-size 64))
;; (deftype uuid-ub48 ()
;;  '(uuid-unsigned-byte-size 48))
;; (deftype uuid-ub32 ()
;;  '(uuid-unsigned-byte-size 32))
;; (deftype uuid-ub24 ()
;;   '(uuid-unsigned-byte-size 24))
;; (deftype uuid-ub16 ()
;;   '(uuid-unsigned-byte-size 16))
;; (deftype uuid-ub8 ()
;;   '(uuid-unsigned-byte-size 8))
;;; ==============================
;;; ==============================
;; (deftype uuid-ub128-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 129))
;; (deftype uuid-ub64-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 65))
;; (deftype uuid-ub48-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 49))
;; (deftype uuid-ub32-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 33))
;; (deftype uuid-ub24-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 25))
;; (deftype uuid-ub16-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 17))
;; (deftype uuid-ub8-integer-length ()
;;   '(uuid-unsigned-byte-integer-length 9))
;;; ==============================
;;; ==============================
;; UUID v5 SHA1 returns an array of type: (simple-array (unsigned-byte 8) (20))
;; (deftype uuid-byte-array-20 ()
;;   ;; expands to: (simple-array (unsigned-byte 8) (20))
;;   '(uuid-byte-array 20))
;;
;; UUID v3 MD5 returns an array of type: (simple-array (unsigned-byte 8) (16))
;; (deftype uuid-byte-array-16 ()
;;   ;; expands to: (simple-array (unsigned-byte 8) (16)))
;;   '(uuid-byte-array 16))
;;; ==============================
;;; ==============================
;; (deftype uuid-bit-vector-128 ()
;;   '(uuid-bit-vector-sized 128))
;; (deftype uuid-bit-vector-48 ()
;;   '(uuid-bit-vector-sized 48))
;; (deftype uuid-bit-vector-32 ()
;;   '(uuid-bit-vector-sized 32))
;; (deftype uuid-bit-vector-16 ()
;;   '(uuid-bit-vector-sized 16))
;; (deftype uuid-bit-vector-8 ()
;;   '(uuid-bit-vector-sized 8))
;;; ==============================
;;; ==============================
;; (deftype uuid-bit-vector-128-length ()
;;   '(uuid-bit-vector-length 128))
;; (deftype uuid-bit-vector-48-length ()
;;   '(uuid-bit-vector-length 48))
;; (deftype uuid-bit-vector-32-length ()
;;   '(uuid-bit-vector-length 32))
;; (deftype uuid-bit-vector-16-length ()
;;   '(uuid-bit-vector-length 16))
;; (deftype uuid-bit-vector-8-length ()
;;   '(uuid-bit-vector-length 8))
;;; ==============================
;;; ==============================
;; (deftype uuid-hex-string-12 ()
;;   `(uuid-hex-string-length 12))
;; (deftype uuid-hex-string-8 ()
;;    `(uuid-hex-string-length 8))
;; (deftype uuid-hex-string-4 ()
;;    `(uuid-hex-string-length 4))
;; (deftype uuid-hex-string-2 ()
;;   `(uuid-hex-string-length 2))
;;; ==============================
;;
;;
;;; ==============================
;; (defun uuid-string-32-p (maybe-uuid-string-32)
;;   (typep maybe-uuid-string-32 'uuid-string-32))
;; (declaim (inline uuid-string-36-p))
;; (defun uuid-string-36-p (maybe-uuid-string-36)
;;   (declare (optimize (speed 3)))
;;   (typep maybe-uuid-string-36 'uuid-string-36))
;; (defun uuid-byte-array-p (maybe-uuid-byte-array)
;;   (typep maybe-uuid-byte-array 'uuid-byte-array))
;; (defun uuid-byte-array-16-p (maybe-uuid-byte-array-16)
;;   (typep maybe-uuid-byte-array-16 'uuid-byte-array-16))
;; (defun uuid-byte-array-20-p (maybe-uuid-byte-array-20)
;;   (typep maybe-uuid-byte-array-20 'uuid-byte-array-20))
;; (defun uuid-byte-string-p (maybe-uuid-byte-string)
;;   (typep maybe-uuid-byte-string 'uuid-byte-string))
;; (defun uuid-bit-vector-128-p (maybe-uuid-bit-vector-128)
;;   (typep maybe-uuid-bit-vector-128 'uuid-bit-vector-128))
;;
;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
