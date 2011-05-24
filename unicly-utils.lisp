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
;;
;;; ==============================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hexadecimal-chars* 
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))

(deftype hexadecimal-char () 
  `(and standard-char (member ,@*hexadecimal-chars*)))

(deftype not-null ()
  '(not null))

(deftype string-not-null-or-empty ()
  '(and not-null string-not-empty))

(deftype string-not-empty ()
  '(and string (not string-empty)))

(deftype string-empty ()
  '(and string
    (or 
     (array character (0)) ;; <- (vector character 0)
     (array nil (0))	      ;; <- (vector nil 0)
     (array base-char (0))))) ;; <- (base-string 0)

(deftype string-or-null () 
  '(or 
    null
    (array character (*))   ;; (vector character)
    (array nil (*))         ;; (vector nil)
    (array base-char (*))))

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
       (string-not-null-or-empty-p (the string maybe-hex-string))
       (or (simple-string-p (the string maybe-hex-string))
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
(deftype string-with-fill-pointer ()
  `(and (or (vector character) (vector base-char))
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
           ((or null string) string))
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
           ((or null string) string))
  (doc-set name 'variable string args))

(defun typedoc (name &optional string &rest args)
  (declare (type symbol name) 
           ((or null string) string))
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
