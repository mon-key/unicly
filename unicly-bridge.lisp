;;; :FILE-CREATED <Timestamp: #{2011-09-13T13:15:02-04:00Z}#{11372} - by MON>
;;; :FILE unicly/unicly-bridge.lisp
;;; ==============================

;;  Compatibility functions for cross-implementation support.
;; :NOTE :SEE  unicly/LISPWORKS for notes and pending problems w/r/t LispWorks compatibility.

;;; ==============================


(in-package #:unicly)

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :SB-UNICODE *features*)
    (warn "~&~%Feature :SB-UNICODE not present in `cl:*features*'~%~
               Using :flexi-streams as fallback for :UTF-8 string/char frobbing~%"))
  (unless (eql sb-impl::*default-external-format* :UTF-8)
    (warn "~&~%Value of SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* not :UTF-8~%~%~
               Current default external-format is: ~S~%~@
               Unicly explicity assumes that all string arguments to the functions:~%~% ~
               UNICLY:MAKE-V3-UUID and UNICLY:MAKE-V5-UUID~%~@
               are contained of character data encoded as UTF-8 \(or a compatible subset\)~%~%~
               Assuming your current SBCL is :UTF-8 compatible and that currrent OS~%~
               is reasonably capable of supporting UTF-8 character encodings, you may wish~%~
               to consider altering the value of SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* in your~%~
               user-init file, e.g. by setting its value to :UTF-8 in your ~~/.sbclrc~%~@
               If altering SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* is not an option,~%~
               please take care to ensure that string arguments to Unicly functions:~%~% ~
               UNICLY:MAKE-V3-UUID and UNICLY:MAKE-V5-UUID~%~@
               are properly encoded as UTF-8 \(or a compatible subset\)~%"
          sb-impl::*default-external-format*)))

;; :SOURCE flexi-streams/mapping.lisp
(deftype char-compat ()
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks 'character
  #+:lispworks '(or base-char character lw:simple-char))

(deftype string-compat ()
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks 'string
  #+:lispworks '(or cl:string
                    cl:base-string
                    system:augmented-string 
                    lispworks:text-string))

(deftype simple-string-compat ()
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks 'simple-string
  #+:lispworks '(or cl:simple-string
                    cl:simple-base-string
                    system:simple-augmented-string
                    lispworks:simple-text-string))

(deftype string-n-length-compat (size)
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks `(string ,size)
  #+:lispworks 
  `(or 
    (cl:base-string ,size)
    (system:augmented-string ,size)
    (lispworks:text-string ,size)))

(deftype simple-string-n-length-compat (size)
  ;; "Convenience shortcut to paper over differences between LispWorks and other Lisps."
  #-:lispworks `(simple-string ,size)
  #+:lispworks `(or (cl:simple-base-string ,size)
                    (system:simple-augmented-string ,size)
                    (lispworks:simple-text-string ,size)))

;; (defun %uuid-string-to-octets (name-arg &key (start 0) end (external-format #+clisp charset:utf-8 #-clisp :UTF-8)) 
#+:sbcl (declaim (inline %uuid-string-to-octets))
(defun %uuid-string-to-octets (name-arg)   
  ;; NAME-ARG is a string to convert to octets. 
  ;; Characters of NAME-ARG should be encoded in UTF-8 or a subset thereof.
  ;; Convenience function for converting the NAME argument to make-v3-uuid and
  ;; make-v5-uuid prior to getting its digest with `uuid-digest-uuid-instance'
  ;; Papers over difference between various implementations of `string-to-octets':
  ;;  SBCL:          `sb-ext:string-to-octets'
  ;;  Clisp:         `ext:convert-string-to-bytes'
  ;;  Flexi-Streams: `flex:string-to-octets'
  ;; NOTE When the string arg to SBCL's `sb-ext:string-to-octets' is comprised of only
  ;; characters is in the constrained range of ASCII chars [0,255] SBCL may be a
  ;; good bit slower than its Babel equivalent. It appears that much of the
  ;; slowness is around the detection of and adjustment for differences in OS
  ;; line-ending conversions w/r/t CRLF LF CR e.g. chars #\return and #\newline.
  ;; Nikodemus posted some new SBCL code examples around May 2011 which would
  ;; allows for SBCL string-to-octets timings to be as good or better as
  ;; Babel's.  AFAICT as of 2011-09-28 the changes required for the speedup have
  ;; yet to be synchronised with upstream.
  ;; :SEE (URL `http://paste.lisp.org/display/122405')
  ;; :SEE (URL `http://paste.lisp.org/display/122395')
  (declare (type string-compat name-arg)
           (optimize (speed 3)))
  ;; #+:allegro (coerce (excl:string-to-octets string) 'list)
  #+:clisp 
  (the (simple-array (unsigned-byte 8) (*))  
    (ext:convert-string-to-bytes name-arg CHARSET:UTF-8))
  #-(or :clisp (and :sbcl :sb-unicode))
  (the (simple-array (unsigned-byte 8) (*))  
    (flexi-streams:string-to-octets name-arg :external-format :UTF-8))
  #+(and :sbcl :sb-unicode)
  (the (values (simple-array (unsigned-byte 8) (*)) &optional)
    (sb-ext:string-to-octets name-arg :external-format :UTF-8)))

;; :NOTE we could declare convert-byte-array to be an array of length 16 or 20
;; If we know that uuid-octets-to-string is only called internally.
;;
;; (defun %uuid-octets-to-string (convert-byte-array &key (start 0) end (external-format #+clisp charset:utf-8 #-clisp :UTF-8)) 
(defun %uuid-octets-to-string (convert-byte-array)
  (declare ((simple-array (unsigned-byte 8) (*)) convert-byte-array))
  (the simple-string-compat
    #+:clisp 
    (ext:convert-string-from-bytes CHARSET:UTF-8) ; :start start :end end)
    #-(or :clisp (and :sbcl :sb-unicode))
    (flexi-streams:octets-to-string convert-byte-array :external-format :UTF-8) ; :start start :end end)
    #+(and :sbcl :sb-unicode)
    (sb-ext:octets-to-string convert-byte-array :external-format :UTF-8) ; :start start :end end)))
    ))

;;; ==============================
;; CLISP
;; Encodings can also be used to convert directly between strings and their
;; corresponding byte vector representation according to that encoding.
;;
;; (EXT:CONVERT-STRING-FROM-BYTES vector encoding &KEY :START :END)
;;     converts the subsequence of vector (a (VECTOR (UNSIGNED-BYTE 8))) from start
;;     to end to a STRING, according to the given encoding, and returns the
;;     resulting string.
;; (EXT:CONVERT-STRING-TO-BYTES string encoding &KEY :START :END)
;;     converts the subsequence of string from start to end to a (VECTOR
;;     (UNSIGNED-BYTE 8)), according to the given encoding, and returns the
;;     resulting byte vector.
;; :SEE (URL `http://www.clisp.org/impnotes.html/#string-byte')
;;
;; #+sbcl (sb-ext:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8)
;; #(225 184 187 102 225 184 137 225 184 178 195 173 195 175 60 195 178 98 71 194
;;   166 62 71 225 184 156 195 174 225 185 137 195 173 64 66 51 195 129 195 187 63
;;   225 184 185 60 109 195 190 225 184 169 195 186 39 195 129 225 185 146 194 172
;;   38 93 225 184 142)
;; #+clisp (ext:convert-string-to-bytes "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" CHARSET:UTF-8)
;; #(225 184 187 102 225 184 137 225 184 178 195 173 195 175 60 195 178 98 71 194
;;   166 62 71 225 184 156 195 174 225 185 137 195 173 64 66 51 195 129 195 187 63
;;   225 184 185 60 109 195 190 225 184 169 195 186 39 195 129 225 185 146 194 172
;;   38 93 225 184 142)
;;
;; SBCL> (type-of (sb-ext:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8))
;; => (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (66))
;;
;; LISPW> (type-of (flexi-streams:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8)) 
;; =>  '(simple-array (unsigned-byte 8) (66)))
;;
;; CLISP> (type-of (ext:convert-string-to-bytes "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" CHARSET:UTF-8))
;; => (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (66))
;;
;; SBCL> (type-of
;;  (sb-ext:octets-to-string
;;   (sb-ext:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8)
;;   :external-format :UTF-8))
;; => (SIMPLE-ARRAY CHARACTER (36))
;;
;; CLISP> (type-of 
;;         (ext:convert-string-from-bytes 
;;          (ext:convert-string-to-bytes "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" charset:utf-8)
;;          charset:utf-8))
;; => (simple-base-string 36)
;;
;; SBCL> (subtypep  '(simple-base-string 36) '(SIMPLE-ARRAY CHARACTER (36)))
;; => NIL, T
;;
;; CLISP> (subtypep  '(simple-base-string 36) '(SIMPLE-ARRAY CHARACTER (36)))
;; => T, T
;;
;; SBCL> (type-of 
;;        (flex:octets-to-string
;;         (flex:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8)
;;         :external-format :UTF-8))
;; => (SIMPLE-ARRAY CHARACTER (36))
;;
;;
;; LispW> (type-of 
;;        (flex:octets-to-string
;;         (flex:string-to-octets "ḻfḉḲíï<òbG¦>GḜîṉí@B3Áû?ḹ<mþḩú'ÁṒ¬&]Ḏ" :external-format :UTF-8)
;;         :external-format :UTF-8))
;; => SIMPLE-TEXT-STRING

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
