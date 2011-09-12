;;; :FILE-CREATED <Timestamp: #{2011-04-02T23:16:03-04:00Z}#{11136} - by MON>
;;; :FILE unicly/unicly-utils.lisp
;;; ==============================


(in-package #:unicly)
;; *package*


;;; ==============================
;; Following are normally loaded from my utils system, 
;; but you prob. don't want all of that :)
;; string-all-hex-char-p string-not-null-or-empty-p hexadecimal-char-p
;; string-or-null hexadecimal-char not-null string-not-null-or-empty
;; string-not-empty string-empty string-or-null *hexadecimal-chars*
;; stream-or-boolean stream-or-boolean-or-string-with-fill-pointer
;; string-with-fill-pointer vector-with-fill-pointer-p
;;
;; type-specifier-p doc-set fundoc vardoc typedoc
;;; ==============================
;;
;;; ==============================
;; I'm at a bit of a loss about what to do with LispWorks strings...
;; Advice/Patches welcome!
;; Following are some of my notes as I tried to track down differences across
;; implementations:
;;
;; lw:sb-char
;; lw:sg-char
;; lw:stchar
;; lw:base-string-p
;; lw:base-character
;; lw:base-char-p
;; lw:8-bit-string
;; lw:16-bit-string
;; lw:general-string
;; lw:general-string-p
;; lw:simple-char
;; lw:simple-char-p
;; lw:simple-base-string-p
;; lw:simple-text-string
;; lw:simple-text-string-p
;;
;; SBCL> (type-of (make-array 0 :element-type nil))
;; => (simple-array nil (0))
;;
;; SBCL> (array-element-type (make-array 0 :element-type nil))
;; => NIL
;;
;; SBCL> (upgraded-array-element-type  (array-element-type (make-array 0 :element-type nil)))
;; => NIL
;;
;; LW> (array-element-type (make-array 0))
;; => T
;;
;; LW> (upgraded-array-element-type  (array-element-type (make-array 0 :element-type 'null)))
;; => T
;;
;; LW> (make-array 36 :element-type 'null)
;;
;; SBCL> (make-array 36 :element-type 'null)
;; => #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;
;; LW> (make-array 36 :element-type 'null)
;; => #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
;;     NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;;
;; (array-dimension (make-array 0) 0)
;; (make-string 0 :initial-element #\0)  (make-array 0) 0)
;; (type-of (make-string 0)) '(array character (0)))
;; (type-of (make-string 0))
;;
;;
;; Is `simple-string-p' LispWorks compatible?
;; lispworks> (type-of (make-array 36 :element-type 'character :initial-contents "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))
;; ;=>  system:simple-augmented-string
;;
;; lispworks> (type-of (make-array 36 :element-type 'lw:simple-char :initial-contents "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))
;; ;=>  simple-text-string
;;
;; lispworks> (type-of (aref (make-array 36 :element-type 'lw:simple-char :initial-contents "6ba7b810-9dad-11d1-80b4-00c04fd430c8") 0))
;; ;=>  character
;;
;; (deftype simple-string-compat ()
;; #+:lispworks 'system:simple-augmented-string)
;; #-:lisworks 'simple-string)
;;
;;; ==============================


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hexadecimal-chars* 
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))

;; :SOURCE flexi-streams/mapping.lisp
(deftype char-compat ()
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks 'character
  #+:lispworks '(or character lw:simple-char base-char standard-char))  

(deftype string-compat ()
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks 'string
  #+:lispworks '(or string simple-string lw:text-string simple-base-string system:simple-augmented-string simple-text-string))

(deftype hexadecimal-char () 
  ;; #-:lispworks `(and standard-char (member ,@*hexadecimal-chars*))
  ;; #+:lispworks `(and char-compat  (member ,@*hexadecimal-chars*)))
  `(and char-compat  (member ,@*hexadecimal-chars*)))

(deftype not-null ()
  '(not null))

(deftype string-empty ()
  #-:lispworks '(and string-compat
                 (or 
                  (array char-compat (0)) ;; <- (vector character 0)
                  (array nil (0)) ;; <- (vector nil 0) ;; :NOTE This is not be a valid type-spec on LispWorks
                  (array base-char (0)))) ;; <- (base-string 0)
  #+:lispworks '(and string-compat 
                 (satisfies %lw-string-zerop)))

(defun %lw-string-zerop (string)
  (declare 
   (string-compat string)
   (optimize (speed 3)))
  (zerop (length string)))

(deftype string-not-empty ()
  '(and string-compat (not string-empty)))

(deftype string-or-null () 
  #-:lispworks '(or 
                 null
                 (array char-compat (*)) ;; (vector character)
                 (array nil (*))         ;; (vector nil)
                 (array base-char (*)))
  #+:lispworks (or null string-empty))

(deftype string-not-null-or-empty ()
  '(and not-null string-not-empty))

(declaim (inline hexadecimal-char-p))
(defun hexadecimal-char-p (maybe-hex-char)
  (declare (optimize (speed 3)))
  (typep maybe-hex-char 'hexadecimal-char))

(declaim (inline string-not-null-or-empty-p))
(defun string-not-null-or-empty-p (str)
  (declare (optimize (speed 3)))
  (typep str 'string-not-null-or-empty))

(declaim (inline string-all-hex-char-p))
(defun string-all-hex-char-p (maybe-hex-string)
  (declare (string-or-null maybe-hex-string)
           (inline string-not-null-or-empty-p
                   hexadecimal-char-p)
           (optimize (speed 3)))
  (and maybe-hex-string ;; allow null and bail
       (string-not-null-or-empty-p (the string-compat maybe-hex-string))
       (or (simple-string-p (the string-compat maybe-hex-string))
           (setf maybe-hex-string (copy-seq maybe-hex-string)))
       (loop 
          for chk-hex across (the simple-string maybe-hex-string)
          always (hexadecimal-char-p chk-hex))))

;; :SOURCE sbcl/src/code/stream.lisp
(declaim (inline vector-with-fill-pointer))
(defun vector-with-fill-pointer-p (object)
  (and (vectorp object)
       (array-has-fill-pointer-p object)))

;; :SOURCE sbcl/src/code/stream.lisp
;; :EXAMPLE~%
;; (typep (make-array 6 :element-type 'character :initial-contents "string" :fill-pointer t)
;;        'string-with-fill-pointer)
;; :SEE-ALSO `string-with-fill-pointer-p', `string-with-fill-pointer-check-type'
(deftype string-with-fill-pointer ()
  #-:lispworks `(and (or (vector character) 
                         (vector base-char))
                     (satisfies vector-with-fill-pointer-p))
  #+:lispworks `(and (vector char-compat)
                     (satisfies vector-with-fill-pointer-p)))

(deftype stream-or-boolean ()
  '(or stream boolean))

(deftype stream-or-boolean-or-string-with-fill-pointer ()
  '(or stream-or-boolean string-with-fill-pointer))

;; PCL p. 342
;; (defun swap-bytes (code)
;;   (assert (<= code #xffff))
;;   (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
;;   code)


;;; ==============================
;;; documentation fun
;;; ==============================  

;;; :SOURCE mcclim/Apps/Scigraph/dwim/extensions.lisp
;;; Which noted: "A somewhat consful implementation, but entirely portable."
(defun type-specifier-p (object)
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))

(defun doc-set (name object-type string args);&rest args)
  (declare (type symbol name) 
           ((member variable type function) object-type)
           ((or null string-compat) string))
  (let ((doc-or-null 
         (if (null string)
             string
             (apply #'format nil `(,string ,@args)))))
    (ecase object-type
          (function
           (setf (documentation (fdefinition name) object-type) 
                 (setf (documentation name object-type) doc-or-null)))
          (variable 
           (locally (declare (special name))
             (setf (documentation name object-type) doc-or-null)))
          (type 
           (setf (documentation name object-type) doc-or-null)))))

(defun fundoc (name &optional string &rest args)
  (declare (type symbol name) ((or null string) string))
  (doc-set name 'function string args))

(defun vardoc (name &optional string &rest args)
  (declare (type symbol name)
           (special name) 
           ((or null string-compat) string))
  (doc-set name 'variable string args))

(defun typedoc (name &optional string &rest args)
  (declare (type symbol name) 
           ((or null string-compat) string))
  (when (type-specifier-p name)
    (doc-set name 'type string args)))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
