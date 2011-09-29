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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hexadecimal-chars* 
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))

(deftype hexadecimal-char () 
  #-:lispworks `(and standard-char (member ,@*hexadecimal-chars*))
  #+:lispworks `(and char-compat   (member ,@*hexadecimal-chars*)))

(deftype not-null ()
  '(not null))

(deftype string-empty ()
  #-:lispworks '(and string-compat
                 (or 
                  (array character (0)) ;; <- (vector character 0)
                  (array nil (0)) ;; <- (vector nil 0) ;; :NOTE This is not be a valid type-spec on LispWorks
                  (array base-char (0)))) ;; <- (base-string 0)
  #+:lispworks '(and (or string-compat  simple-string-compat)
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
                 (array character (*)) ;; (vector character)
                 (array nil (*))       ;; (vector nil)
                 (array base-char (*)))
  #+:lispworks (or null string-empty))

(deftype string-not-null-or-empty ()
  '(and not-null string-not-empty))

(declaim (inline %string-not-empty-p))
(defun %string-not-empty-p (maybe-not-null-or-empty-string)
  (declare (optimize (speed 3)))
  (typep maybe-not-null-or-empty-string 'string-not-empty))

(declaim (inline simple-string-compat-p))
(defun simple-string-compat-p (maybe-simple-string-compat)
  (declare (optimize (speed 3)))
  #+:lispworks 
  (typep maybe-simple-string-compat 'simple-string-compat)
  #-:lispworks 
  (simple-string-p maybe-simple-string-compat))

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
           (inline simple-string-compat-p
                   %string-not-empty-p
                   hexadecimal-char-p)
           (optimize (speed 3)))
  (when (null maybe-hex-string)
    (return-from string-all-hex-char-p nil))
  (the boolean
    (and (locally 
             (declare (string-compat maybe-hex-string))
           (%string-not-empty-p maybe-hex-string)
           (or (simple-string-compat-p maybe-hex-string)
               (setf maybe-hex-string (copy-seq maybe-hex-string))))
         (locally
             (declare (simple-string-compat maybe-hex-string))
           (loop 
              for chk-hex across maybe-hex-string
              always (hexadecimal-char-p chk-hex))))))

;; (loop 
;;    for chk-hex across +uuid-null-string+ of-type simple-string-compat
;;    always (hexadecimal-char-p chk-hex))

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
  #+:lispworks `(and (or simple-string-compat
                         string-compat)
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

(defun doc-set (name object-type string args) ;&rest args)
  (declare (type (or standard-method standard-generic-function (and symbol (not-null)))  name) 
           (type (member variable type function generic method) object-type)
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
       (setf (documentation name object-type) doc-or-null))
      (method
       (setf (documentation name t) doc-or-null))
      (generic
       (setf (documentation name t) doc-or-null)))))

(defun fundoc (name &optional string &rest args)
  (declare (type symbol name) ((or null string) string))
  (doc-set name 'function string args))

(defun generic-doc (function-designator &optional doc-string &rest args)
  (when (and doc-string
             (typep function-designator 'standard-generic-function))
    (doc-set function-designator 'generic doc-string args)))

(defun method-doc (generic-function-designator qualifiers specializers &optional doc-string &rest args)
  (when doc-string
    (let ((found-method (find-method generic-function-designator qualifiers specializers nil)))
      (when (and found-method (typep found-method 'standard-method))
        (doc-set found-method 'method doc-string args)))))

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
