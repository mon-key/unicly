;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:17:12-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-docs.lisp
;;; ==============================


(in-package #:unicly)
;; *package*


;;; ==============================
;;; :SPECIALS-UUID-DOCUMENTATION
;;; ============================

(vardoc '*random-state-uuid*
        "A random-state objet for use when generating UUIDv4 UUIDs.~%~@
:EXAMPLE~%
 *random-state-uuid*~%
 \(random-state-p  *random-state-uuid*\)~%~@
:SEE-ALSO `cl:random-state', `cl:random-state-p'.~%►►►")

;;; ==============================
(vardoc '*uuid-namespace-dns*
"A DNS namespace as provided with RFC4122 Appendix C. \"Some Name Space IDs\".~%~@
It is suggested that this may be used as the NAMESPACE arg when generating v3
and v5 UUIDs where NAME is a string designating a FQDN \(Fully-Qualified Domain Name\).~%~@
:EXAMPLE~%
 \(unique-universal-identifier-p *uuid-namespace-dns*\)~%~@
The default value of this namespace is generated from the hexadecimal integers
provided for `NameSpace_DNS` page 29 of the appendix.~%~@
These are:
#x6ba7b810                    ;; uuid-ub32
#x9dad                        ;; uuid-ub16
#x11d1                        ;; uuid-ub16
#x80                          ;; uuid-ub8
#xb4                          ;; uuid-ub8
#x00 #xc0 #x4f #xd4 #x30 #xc8 ;; uuid-ub48~%~@
With the string representation: \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%►►►")
 
(vardoc '*uuid-namespace-url*
"An URL namespace as provided with RFC4122 Appendix C. \"Some Name Space IDs\".~%~@
It is suggested that this may be used as the NAMESPACE arg when generating v3
and v5 UUIDs where NAME is a string designating an URL \(Universal Resource Locator\).~%~@
:EXAMPLE~%
 \(unique-universal-identifier-p *uuid-namespace-url*\)~%~@
The default value for this namespace is generated from the hexadecimal integers
provided with RFC4122 Appendix C. - \"Some- Name Space IDs\" page 29.~%~@
These are:
#x6ba7b811                        ;; uuid-ub32
#x9dad                            ;; uuid-ub16
#x11d1                            ;; uuid-ub16
#x80                              ;; uuid-ub8 
#xb4                              ;; uuid-ub8 
#x00 #xc0, #x4f, #xd4, #x30, #xc8 ;; uuid-ub48~%~@
With the string representation: \"6ba7b811-9dad-11d1-80b4-00c04fd430c8\"~%~@
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%►►►")

(vardoc '*uuid-namespace-oid*
"An OID namespace as provided with RFC4122 Appendix C. \"Some Name Space IDs\".~%~@
It is suggested that this may be used as the NAMESPACE arg when generating v3
and v5 UUIDs where NAME is a string designating an ISO OID .~%~@
:EXAMPLE~%
 \(unique-universal-identifier-p *uuid-namespace-oid*\)~%~@
The default value for this namespace is generated from the hexadecimal integers
provided with RFC4122 Appendix C. - \"Some- Name Space IDs\" page 29.~%~@
#x6ba7b812                        ;; uuid-ub32
#x9dad                            ;; uuid-ub16
#x11d1                            ;; uuid-ub16
#x80                              ;; uuid-ub8 
#xb4                              ;; uuid-ub8 
#x00 #xc0, #x4f, #xd4, #x30, #xc8 ;; uuid-ub48~%~@
With the string representaton: \"6ba7b812-9dad-11d1-80b4-00c04fd430c8\"~%~@
:SEE (URL `http://www.techabulary.com/o/oid/')~%~@
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%►►►")

(vardoc '*uuid-namespace-x500*
"An x500 namespace as provided with RFC4122 Appendix C. \"Some Name Space IDs\".~%~@
It is suggested that this may be used as the NAMESPACE arg when generating v3
and v5 UUIDs where NAME is a string designating an an X.500 Distinguished Name 
in DER \(Distinguished Encoding Rules\) or a text output format.~%~@
:EXAMPLE~%~@
 \(unique-universal-identifier-p *uuid-namespace-x500*\)~%~@
The default value for this namespace is generated from the hexadecimal integers
provided with RFC4122 Appendix C. - \"Some- Name Space IDs\" page 29.~%~@
#x6ba7b812                        ;; uuid-ub32
#x9dad                            ;; uuid-ub16
#x11d1                            ;; uuid-ub16
#x80                              ;; uuid-ub8 
#xb4                              ;; uuid-ub8 
#x00 #xc0, #x4f, #xd4, #x30, #xc8 ;; uuid-ub48~%~@
With the string representaton: \"6ba7b812-9dad-11d1-80b4-00c04fd430c8\"~%~@
:SEE \(URL `http://en.wikipedia.org/wiki/X.500'\)~%
:SEE \(URL `http://luca.ntop.org/Teaching/Appunti/asn1.html'\) esp. Section 6. ~%~@
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%►►►")



;;; ==============================
;;; :UUID-TYPES-DOCUMENTATION
;;; ==============================

(typedoc 'uuid-ub48
"An object of type: \(unsigned-byte 48\)~%~@
Octets:         6
Bits:          48
Hex value:     #xFFFFFFFFFFFF
Decimal value: 281474976710655
Octal value:   #o7777777777777777
:Binary value: #b111111111111111111111111111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-ub48', `uuid-ub32', `uuid-ub16', `uuid-ub8'.~%►►►")

(typedoc 'uuid-ub32
"An object of type: \(unsigned-byte 32\)~%~@
Octets:         4
Bits:          32 
Hex value:     #xFFFFFFFF
Decimal value: 4294967295
Octal value:   #o37777777777
Binary value:  #b11111111111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-ub48', `uuid-ub32', `uuid-ub16', `uuid-ub8'.~%►►►")

(typedoc 'uuid-ub16
"An object of type: \(unsigned-byte 16\)~%~@
Octets:          2
Bits:           16 
Hex value:      #xFFFF
Decimal value:  65535
Octal value:    #o177777
Binary value:   #b1111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-ub48', `uuid-ub32', `uuid-ub16', `uuid-ub8'.~%►►►")

(typedoc 'uuid-ub8
         "An object of type: \(unsigned-byte 8\)~%~@
Octets:         1
Bits:           8
Hex value:      #xFF
Decimal value:  255
Octal value:    #o377
Binary value:   #b11111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-ub48', `uuid-ub32', `uuid-ub16', `uuid-ub8'.~%►►►")

(typedoc 'uuid-integer-length
         "An object of this type is an integer of type \(unsigned-byte 3\) in the set {1 2 4 6}.~%~@
These represent valid `cl:integer-length's capable of representing the numeric
value in a fully intialized instance of the class `unique-universal-identifier'.
:EXAMPLE~%
 \(mapcar #'\(lambda \(x\) \(cons \(typep x 'uuid-integer-length\) x\)\) '\(0 1 2 3 4 5 6 7\)\)~%
:SEE-ALSO `uuid-number-to-byte-array'.~%►►►")

(typedoc 'uuid-string-36
"An object with the type signature: \(array character \(32\)\)
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-byte-string', `uuid-byte-array-16', `uuid-string-32',
`uuid-string-36', `uuid-hex-string-32', `uuid-hex-string-36',
`uuid-hex-string-32-p', `uuid-hex-string-36-p',.~%►►►")

(typedoc 'uuid-string-32
"An object with the type signature: \(array character \(36\)\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-byte-string', `uuid-byte-array-16', `uuid-string-32',
`uuid-string-36', `uuid-hex-string-32', `uuid-hex-string-36',
`uuid-hex-string-32-p', `uuid-hex-string-36-p'.~%►►►")

(typedoc 'uuid-hex-string-36
"An object of type \(array character \(36\)\) satsfying `uuid-hex-string-36-p'.~%~@
:EXAMPLE~%
 (type ~%~@
 \(typep \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\" 'uuid-hex-string-36\)~%
 \(typep \"6BA7B810-9DAD-11D1-80B4-00C04FD430C8\" 'uuid-hex-string-36-p\)~%~@
;; Following fails successfully:~%
 \(typep \"6BA7B810--9DAD-11D1-80B4-00C04FD430C8\ 'uuid-hex-string-36-p\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'uuid-hex-string-32
"An object with the type signature:~%
 \(array character \(32\)\)~%~@
And satisfies `mon:string-all-hex-char-p'.~%~@
:EXAMPLE~%
 \(typep \(uuid-print-bytes nil *uuid-namespace-dns*\) 'uuid-hex-string-32\)~%
;; Following fails successfully:~%
 \(typep \(print-object *uuid-namespace-dns* nil\) 'uuid-hex-string-32\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'uuid-byte-string
 "An object of with type signature: \(simple-array character \(16\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-get-bytes \(uuid-print-bytes nil *uuid-namespace-dns*\)\) 'uuid-byte-string\)~%~@
:SEE-ALSO `uuid-byte-array-16', `uuid-hex-string-36'.~%►►►")

(typedoc 'uuid-bit-vector-128
"An object of type \(simple-bit-vector 128\) capable of representing all 128
bits of a `unique-universal-identifier'~%~@
:EXAMPLE~%
 \(typep  \(uuid-bit-vector-zeroed\) 'uuid-bit-vector-128\)~%
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'uuid-byte-array-16
 "An object which has the type signature: \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-to-byte-array *uuid-namespace-dns*\) 'uuid-byte-array-16\)~%~@
:SEE-ALSO `uuid-hex-string-36'.~%►►►")

(typedoc 'uuid-byte-array-20
 "An object which has the type signature: \(simple-array \(unsigned-byte 8\) \(20\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-string-to-sha1-byte-array \"bubba\"\) 'uuid-byte-array-20\)~%~@
:SEE-ALSO `uuid-hex-string-36'.~%►►►")


;;; ==============================
;;; :UUID-TYPE-PREDICATE-DOCUMENTATION
;;; ==============================

(fundoc 'uuid-string-32-p
"Whether object is of type `uuid-string-32'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-string-36-p
"Whether object is of type `uuid-string-36'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-hex-string-36-p
"Whether MAYBE-UUID-HEX-STRING-36 is a valid string representation of a UUID.~%~@
Return T when MAYBE-UUID-HEX-STRING-36 satisfies the following constraints:~%
 - Its type is '\(array character 36\)~%
 - When split to list of strings on #\\- delimter the list is of length is 5~%
 - The length of each string in the list matches the pattern 8-4-4-4-12~%
 - each string in the list satisfies `mon:string-all-hex-char-p'~%
:EXAMPLE~% 
\(uuid-hex-string-36-p \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)~%
 \(uuid-hex-string-36-p \"6BA7B810-9DAD-11D1-80B4-00C04FD430C8\"\)~%~@
;; Following fails successfully:~%
 \(uuid-hex-string-36-p \"6BA7B810--9DAD-11D1-80B4-00C04FD430C8\"\)~%~@
:SEE-ALSO `uuid-hex-string-32-p', `uuid-hex-string-32'
`mon:split-string-on-chars'.~%►►►")

(fundoc 'uuid-hex-string-32-p
"Whether object is of type `uuid-hex-string-32'.~%~@
Return T when object has the type signature:~%
 \(array character \(32\)\)~%~@
and satisfies `mon:string-all-hex-char-p'.~%~@
:EXAMPLE~%
 \(uuid-hex-string-32-p \(uuid-print-bytes nil *uuid-namespace-dns*\)\)~%
;; Following fails successfully:~%
 \(uuid-hex-string-32-p \(print-object *uuid-namespace-dns* nil\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%►►►")

(fundoc 'uuid-byte-array-16-p
"Whether MAYBE-UUID-BYTE-ARRAY-16 is of type `uuid-byte-array-16'.~%~@
Return T when MAYBE-UUID-BYTE-ARRAY-16 has the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-array-16-p \(uuid-to-byte-array *uuid-namespace-dns*\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%►►►")

(fundoc 'uuid-byte-array-20-p
"Whether MAYBE-UUID-BYTE-ARRAY-16 is of type `uuid-byte-array-20'.~%~@
Return T when MAYBE-UUID-BYTE-ARRAY-16 has the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(20\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-array-20-p \(uuid-string-to-sha1-byte-array \"bubba\"\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%►►►")

(fundoc 'uuid-byte-string-p
"Whether object is of type `uuid-byte-string'.~%~@
Return T when object has the type signature:~%
 \(simple-array character \(16\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-string-p \(uuid-get-bytes \(uuid-print-bytes nil *uuid-namespace-dns*\)\)\)~%~@
:SEE-ALSO `uuid-hex-string-36-p', `uuid-byte-array-16-p'.~%►►►")

(fundoc 'uuid-bit-vector-v3-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-bit-vector-version', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%►►►")

(fundoc 'uuid-bit-vector-v4-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-bit-vector-version', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%►►►")

(fundoc 'uuid-bit-vector-v5-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-bit-vector-version', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%►►►")


;;; ==============================
;;; UUID-FUNCTIONS-DOCUMENTATION
;;; ==============================

(fundoc 'uuid-bit-vector-version
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-bit-vector-version', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%►►►")

(fundoc '%verify-slot-boundp-and-type
"Check that VERIFY-UUID has all slots `cl:slot-boundp' and of the correct type.~%~@
Signal either a `cl:simple-condition' or `cl:type-error' error if not.~%~@
Helper function for `uuid-copy-uuid'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-copy-uuid
"Copy slot-value's of UUID-INSTANCE to new instance and return new instance.~%~@
UUID-INSTANCE should satisfy `unique-universal-identifier-p', an error is
signaled if not.~%~@
:EXAMPLE~%
 \(let* \(\(source *uuid-namespace-dns*\)
        \(clone \(uuid-copy-uuid source\)\)\)
   \(not \(or \(eq     clone source\)
            \(eql    clone source\)
            \(equal  clone source\)
            \(equalp clone source\)\)\)\)~%~@
:NOTE The base slot-values of an instance of `unique-universal-identifier' class
are constants and satisfy `cl:constantp':~%
 \(constantp \(slot-value *uuid-namespace-dns* '%uuid_time-low\)\)~%~@
Therefor, the newly returned copy will not share mutable structure with
its source UUID-INSTANCE.~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-bit-vector-zeroed
"Return a bit vector of 128 elements with all eltements zeroed.~%~@
:EXAMPLE~%
 \(uuid-bit-vector-zeroed\)~%
 \(typep \(uuid-bit-vector-zeroed\) 'uuid-bit-vector-128\)~%~@
:SEE-ALSO `uuid-bit-vector-128', `uuid-deposit-octet-to-bit-vector',
`uuid-byte-array-to-bit-vector', `make-null-uuid'.~%►►►")

(fundoc 'uuid-request-integer
  "Decode an integer of LENGTH octets from ARRAY starting at OFFSET.~%~@
The number represented by BYTE-ARRAY may be any positive integer representable
in 48 bits.~%~@
The OFFSET is effectively :start and 
LENGTH is :end where :end => \(+ offset length\)
x86-32 OSs are LITTLE-ENDIAN but network-byte-order is big-endian...~%~@
:EXAMPLE~%
  \(uuid-request-integer \(uuid-number-to-byte-array 281474976710654\) 0 6\)~%
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-disassemble-ub48
"Return U48 as six integer values of type uuid-ub8 as if by `cl:values'.~%~@
U48 is an integer of type `uuid-ub48'. corresponding to the `%uuid_node' slot of
an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(multiple-value-list
  \(uuid-disassemble-ub48
   \(slot-value \(make-v5-uuid *uuid-namespace-dns* \"buuba\"\) '%uuid_node\)\)\)
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%►►►")

(fundoc 'uuid-disassemble-ub32
"Return U32 as four integer values of type `uuid-ub8' as if by `cl:values'.~%~@
U32 is an integer of type `uuid-ub32' corresponding to the `%uuid_time-low' slot
of an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(slot-value  \(make-v4-uuid\) '%UUID_TIME-LOW\)~%
 \(uuid-disassemble-ub32 \(slot-value \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(equal \(multiple-value-list \(uuid-disassemble-ub16 13953\)\) '\(54 129\)\)~%~@
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%►►►")

(fundoc 'uuid-disassemble-ub16
"Return two uuid-ub8 octets of U16 as if by `cl:values'.~%~@
U16 is an integer of type `uuid-ub16' corresponding to either the
`%uuid_time-high-and-version' or `%uuid_time-mid' slots of an instance of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(uuid-disassemble-ub16 
  \(slot-value \(make-v5-uuid *uuid-namespace-dns* \"buuba\"\) '%uuid_time-mid\)\)~%
 \(multiple-value-list \(uuid-disassemble-ub16 13953\)\)~%
 \(eq \(nth-value 0 \(uuid-disassemble-ub16 13953\)\) 54\)~%
 \(eq \(nth-value 0 \(uuid-disassemble-ub16 13953\)\) 129\)~%~@
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%►►►")

(fundoc 'uuid-get-namespace-bytes
  "Convert UUID to a byte-array.~%~@
Arg UUID should be an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
Return value is an array of type `uuid-byte-array-16' with the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
It will satisfy the predicate `uuid-byte-array-16-p'.~%~@
:EXAMPLE~%
 \(uuid-get-namespace-bytes *uuid-namespace-dns*\)~%~@
:SEE-ALSO `uuid-from-byte-array'.~%►►►")


(fundoc 'make-hash-table-uuid
        "Return a hash-table specialized to hold UUIDs as keys.~%~@
On SBCL this this is a hash-table with `uuid-eql' as `cl:hash-table-test'.~%~@
Keyword :SYNCHRONIZED when non-nile indicates hash-table may have multiple concurrent readers.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-ext:with-locked-hash-table'.~%►►►")

(fundoc 'sxhash-uuid
        "Return the `sxhash' of UUID's representation as an object of type `uuid-bit-vector-128'.~%~@
UUID is an instance of the class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(sxhash-uuid \(make-v4-uuid\)\)~%
 \(sxhash-uuid \(make-v3 uuid *uuid-namespace-dns* \"bubba\"\)\)~%
 \(sxhash-uuid \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)~%
 \(let \(\(v4-instance \(make-v4-uuid\)\)
       \(v5-instance \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
       \(v3-instance \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
   \(and \(eq \(sxhash-uuid v5-instance\)
            \(sxhash-uuid \(uuid-copy-uuid v5-instance\)\)\)
        \(eq \(sxhash-uuid v3-instance\)
            \(sxhash-uuid \(uuid-copy-uuid v3-instance\)\)\)
        \(eq \(sxhash-uuid v4-instance\)
            \(sxhash-uuid \(uuid-copy-uuid v4-instance\)\)\)\)\)~%~@
:SEE-ALSO `uuid-eql', `uuid-copy-uuid', `uuid-to-bit-vector', `sb-int:bit-vector-='.~%►►►")

(fundoc 'serialize-uuid
"Serialize UUID to STREAM.~%~@
Bytes of UUID are written to STREAM.~%~@
Stream should have an :element-type '\(unsigned-byte 8\).~%~@
:EXAMPLE~%
 \(let \(\(file \(make-pathname :directory '\(:absolute \"tmp\"\)
                            :name \"temp-bytes\"\)\)
       \(w-uuid \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
       \(gthr '\(\)\)\)
   \(with-open-file \(s file
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :direction :output
                      :element-type 'uuid-ub8)
     \(serialize-uuid w-uuid s\)\)
  ;; :NOTE basis for deserializing to file \(defun deserialize-uuid \(file\) ;
   \(with-open-file \(stream file :element-type 'uuid-ub8\)
     \(do \(\(code \(read-byte stream nil :eof\) \(read-byte stream nil :eof\)\)\)
         \(\(eql code :eof\)\)
       \(push code gthr\)\)\)
   \(and gthr
        \(setf gthr \(uuid-from-byte-array \(make-array 16
                                                     :element-type 'uuid-ub8
                                                     :initial-contents \(nreverse gthr\)\)\)\)\)
   \(unwind-protect 
        \(list \(uuid-eql w-uuid gthr\)
              gthr
              w-uuid\)
     \(delete-file file\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_time-low-request
"Step three of RFC4122 Section 4.3:~%~@
 - Set octets zero through 3 of the time_low field to octets zero through 3 of the digest hash.~%~@
:EXAMPLE~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v4-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_time-mid-request
"Step four of RFC4122 Section 4.3:~%~@
 - Set octets zero and one of the time_mid field to octets 4 and 5 of the digest hash.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_time-high-and-version-request
"Steps five and six of RFC4122 Section 4.3:~%~@
Step five:~%
 - Set octets zero and one of the time_hi_and_version field to octets 6 and 7 of
   the digest hash.~%~@
Step six:~%
 - Set the four most significant bits \(bits 12 through 15\) of the
   time_hi_and_version field to the appropriate 4-bit version number per the
   table below.~%~@
                                
 Msb0  Msb1  Msb2  Msb3   Version
    0     1     0     1        5 
    0     0     1     1        3 
    0     1     0     0        4  ; :NOTE Not relevant to this operation.

 MSB                                LSB
  0  0  1  1  0  0 0 0 0 0 1 1 1 0 0 1 ; v3
  0  1  0  1  0  0 0 0 0 0 1 1 1 0 0 1 ; v5
  0  1  0  0  0  0 0 0 0 0 1 1 1 0 0 1 ; v4
 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
 !---------!

UUID v3 bit-field
12345 => #b0011000000111001 \(length \"0011000000111001\"\) => 16
#b0011000000111001
  !--!

UUID v5 bit-field
20759 => #b0101000100010111 \(length \"0101000100010111\"\) => 16
#b0101000100010111
  !--!

UUID v4 bit-field
:NOTE Per RFC 4.1.3 bit 48 in a uuid-bit-vector-128 should always be 0.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_clock-seq-and-reserved-request
"Steps seven and eight of RFC4122 Section 4.3:~%~@
Step seven:~%
 - Set the clock_seq_hi_and_reserved field to octet 8 of the digest hash.~%~@
Step eight:~%
 - Set the two most significant bits (bits 6 and 7) of the clock_seq_hi_and_reserved
   to zero and one, respectively. IOW, set bit 6 to 0 - Set bit 7 to 1~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_clock-seq-low-request
"Step nine of RFC4122 Section 4.3:~%
 - Set the clock_seq_low field to octet 9 of the hash.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%uuid_node-request
"Step ten or RFC4122 Section 4.3:~%
 - Set octets zero through five of the node field to octets 10 through 15 of the digest hash.~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc '%verify-version-3-or-5
"Verify that arg VERSION is either the integer 3 or 5.~%~@
Signal an error if not.
Helper function for `digested-v3or5-uuid'.~%~@
:EXAMPLE~%
  \(%verify-version-3-or-5 3\)~%
  \(%verify-version-3-or-5 5\)~%~@
;; Following fails successfully:~%
  \(%verify-version-3-or-5 6\)~%~@
:SEE-ALSO `%verify-digest-version'.~%►►►")

(fundoc '%verify-digest-version
"Return one of the symbols :MD5 or :SHA1 depending on value of CHK-VERSION.
When CHK-VERSION is 3 return :MD5~%~@
When CHK-VERSION is 5 return :SHA1~%~@
CHK-VERSION should satisfy `%verify-version-3-or-5', an error is signaled if not. ~%~@
Helper function for `uuid-digest-uuid-string'~%~@
:EXAMPLE~%
  \(%verify-digest-version 3\)~%
  \(%verify-digest-version 5\)~%
;; Following fails successfully:~%
  \(%verify-digest-version 6\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-digest-uuid-instance
"This is step two of RFC4122 Section 4.3:
 - Compute the digest hash of the name space ID concatenated with the name.~%~@
DIGEST-VERSION is an interger value either (3 or 5) indicateing the type of hash
to digest where 3 indicates :MD5 for a v3 UUID and 5 indicates :SHA1 for a v5
UUID. It should satisfy `%verify-digest-version', and error is signaled if
not.~%~@
Arg UUID-NAMESPACE-INSTANCE is an instance of class
`unique-universal-identifier' indicating the namespace of NAME.
NAME is a string of arbitrary length and should be encoded as UTF-8 \(or subset of\).~%~@
When DIGEST-VERSION is 3 return value is an object of type `uuid-byte-array-16'
suitable for use as the V3-DIGEST-BYTE-ARRAY argument to `digested-v3-uuid'.
When DIGEST-VERSION is 5 return value is an object of type `uuid-byte-array-20'
suitable for use as the V5-DIGEST-BYTE-ARRAY argument to `digested-v5-uuid'.
:EXAMPLE~%
 \(uuid-digest-uuid-instance 3 *uuid-namespace-dns* \"bubba\"\)~%
 \(uuid-digest-uuid-instance 5 *uuid-namespace-dns* \"bubba\"\)~%
 \(uuid-byte-array-16-p \(uuid-digest-uuid-instance 3 *uuid-namespace-dns* \"bubba\"\)\)~%
 \(uuid-byte-array-20-p \(uuid-digest-uuid-instance 5 *uuid-namespace-dns* \"bubba\"\)\)~%~@
:SEE-ALSO `%uuid-digest-uuid-instance-md5', `%uuid-digest-uuid-instance-sha1'.~%►►►")

;; uuid-get-namespace-bytes
;; sb-ext:string-to-octets
;; flexi-streams:string-to-octets

(fundoc 'digested-v3or5-uuid ; ######
"Helper function to format UUIDv3 and UUIDv5 hashes according to UUID-VERSION.~%~@
DIGEST-BYTE-ARRAY is an argument suitable as the BYTE-ARRAY arg to `uuid-load-bytes'.~%~@
DIGEST-BYTE-ARRAY is as per the the return value of `digested-v3or5-uuid'
UUID-VERSION should satisfy `%verify-version-3-or-5', an error is signaled if not.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'make-v4-uuid ; ######
  "Return a version 4 \(random\) UUID.~%~@
Return value is an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
A v4 UUID is one generatied UUIDs from truly-random or pseudo-random numbers.~%~@
 ,---- RFC4122 Section 4.4. \"Creating UUIDs from Truly-Random/Pseudo-Random Numbers\"
 | 
 |   The algorithm is as follows:
 |
 |  1)  Set the two most significant bits (bits 6 and 7) of the
 |     clock_seq_hi_and_reserved to zero and one, respectively.
 |     `-> Slot `%uuid_clock-seq-and-reserved' 
 |     :NOTE Steps 7 and 8 for v3 and v5 with `%uuid_clock-seq-and-reserved-request' 
 |
 |  2) Set the four most significant bits (bits 12 through 15) of the
 |     time_hi_and_version field to the 4-bit version number from Section 4.1.3.
 |     `-> Slot `%uuid_time-high-and-version' e.g. #*0100 
 |     :NOTE Correspond with step 6 for v3 and v5 which sets top bits 
 |     #*01010 for SHA1 or #*0011 for MD5 with `%uuid_time-high-and-version-request'
 |     
 |  3) Set all the other bits to randomly (or pseudo-randomly) chosen
 |     values.
 |     `-> Slots `%uuid_time-low', `%uuid_time-mid', `%uuid-clock-seq-low', `%uuid_node'
 `----~%~@
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-null-uuid',
`*random-state-uuid*', `cl:random'.~%►►►")

(fundoc 'make-v3-uuid ; ######
"Generate an RFC4122 version 3 (named based MD5) UUID for NAME in NAMESPACE.~%~@
Return value is an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
NAMESPACE is a UUID namespace object.~%~@
NAME is a string.~%~@
:EXAMPLE~%~@
 \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'make-v5-uuid ; ######
  "Generates an RFC4122 version 5 (nambe based SHA1) UUID with NAME in NAMESPACE.~%~@
Return value is an instance of the class UNIQUE-UNIVERSAL-IDENTIFIER.~%~@
NAMESPACE is a UUID namespace object.~%~@
NAME is a string.~%~@
:EXAMPLE~%
 \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-as-urn-string ; ######
  "Print UUID as a URN \(Universal Resource Name\) to STREAM.~%~@
Return value has the format:~%
 urn:uuid:<UUID>~%~@
UUID is an instance of `unique-universal-identifier' class.~%~@
STREAM is an output-stream of type `stream-or-boolean-or-string-with-fill-pointer'.~%~@
:EXAMPLE~%
 \(string= \(uuid-as-urn-string nil \(make-null-uuid\)\) \"urn:uuid:00000000-0000-0000-0000-000000000000\"\)~%~@
:NOTE Per RFC4122 Section 3. \"Namespace Registration Template\" 
 ,----
 | The hexadecimal values \"a\" through \"f\" are output as
 | lower case characters and are case insensitive on input.
 `----~%~@
:SEE-ALSO `make-uuid-from-string', `uuid-print-bytes',
`uuid-print-bytes-to-string', `uuid-princ-to-string',
`uuid-string-parse-integer'.~%►►►")

(fundoc 'make-null-uuid ; ######
"Generate a NULL UUID.~%~@
Per RFC4122 Setion 4.1.7. \"Nil UUID\": 
 The nil UUID is special form of UUID specified to have all 128 bits set to zero.~%~@
Return value is an instance of `unique-universal-identifier' with all slots defaulted to 0.~%~@
Return value has the format:~%
 00000000-0000-0000-0000-000000000000~%~@
:EXAMPLE~%
 \(make-null-uuid\)~%~@
:SEE-ALSO `uuid-bit-vector-zeroed'.~%►►►")

(fundoc 'uuid-from-byte-array ; ######
 "Convert BYTE-ARRAY to a UUID.~%~@
Return value is an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
BYTE-ARRAY is a byte-array as returned by `uuid-to-byte-array' it should be of
type `uuid-to-byte-array' and satisfy the predicate
`uuid-to-byte-array-p':~%
:EXAMPLE~%~@
 \(uuid-from-byte-array \(uuid-to-byte-array \(make-uuid-from-string \"6ba7b814-9dad-11d1-80b4-00c04fd430c8\"\)\)\)
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-string-to-sha1-byte-array
"Return string as a SHA1 byte-array as if by `ironclad:make-digest'.~%~@
Arg STRING is a string and may contain UTF-8 characters.~%~@
:EXAMPLE~%
 \(let \(\(target-str \(mon-test:make-random-string 16\)\)\)
   \(values \(string-to-sha1-byte-array target-str\) target-str\)\)~@
:NOTE we can compare the output of `string-to-sha1-byte-array' with output
Emacs lisp' `sha1-binary':
 CL>  \(string-to-sha1-byte-array \"bubba\"\)
        => #\(32 193 148 189 4 164 89 163 52 78 106 202 121 61 200 118 132 25 134 11\)
 elisp> \(vconcat \(sha1-binary \"bubba\"\)\)
         => [32 193 148 189 4 164 89 163 52 78 106 202 121 61 200 118 132 25 134 11]~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'make-uuid-from-string-if
"Helper function for `make-uuid-from-string'.~%~@
If UUID-HEX-STRING-36-IF satisfies `uuid-hex-string-36-p' return it.
If the constraint fails signal a `mon:simple-error-mon' condition.~%~@
:EXAMPLE~%~@
 \(make-uuid-from-string-if \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)~%
 \(make-uuid-from-string-if \"6ba7b810-9dad--11d1-80b4-00c04fd430c8\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'uuid-string-parse-integer
"Helper macro for `make-uuid-from-string'~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(uuid-string-parse-integer \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\" 0 8 uuid-ub32\)\)~%~@
 \(let \(\(uuid-str \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)
       \(parse-specs '\(\(0   8 uuid-ub32\)
                      \(9  13 uuid-ub16\)
                      \(14 18 uuid-ub16\)
                      \(19 21 uuid-ub8\)
                      \(21 23 uuid-ub8\)
                      \(24 36 uuid-ub48\)\)\)
       \(gthr '\(\)\)\)
   \(dolist \(m parse-specs \(setf gthr \(nreverse gthr\)\)\)
     \(push \(apply #'uuid-string-parse-integer m\) gthr\)\)\)~%~@
:SEE-ALSO `make-uuid-from-string-if'.~%►►►")

(fundoc 'make-uuid-from-string ; ######
 "Create an instance of class UNIQUE-UNIVERSAL-IDENTIFIER from UUID-OR-HEX-STRING-36.~%~@
UUID-OR-HEX-STRING-36 is an object of type UNIQUE-UNIVERSAL-IDENTIFIER or UUID-OR-HEX-STRING-36.
It should satisfy one of `unique-universal-identifier-p' or
`uuid-hex-string-36-p', an error is signaled if not.~%~@
:EXAMPLE~%
 \(make-uuid-from-string \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)~%
 \(make-uuid-from-string *uuid-namespace-dns*\)~%
 \(make-uuid-from-string \(uuid-princ-to-string *uuid-namespace-dns*\)\)~%
 \(equal \(make-uuid-from-string *uuid-namespace-dns*\)
        \(make-uuid-from-string \(uuid-princ-to-string *uuid-namespace-dns*\)\)\)~%~@
;; :Following successfully signals an error:~%
 \(make-uuid-from-string \"Q6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")


;;; ==============================
;;; :DEPRECATED-DOCS
;;; ==============================

#+nil
(fundoc 'uuid-number-to-byte-array
        "Return NUMBER as if by `cl:values' a byte-array and the count of its elements.~%~@
byte-array is in big-endian format with LSB as first elt and MSB as last elt.~%~@
:EXAMPLE~%~@
 \(slot-value *uuid-namespace-dns* '%uuid_node\)
 => 825973027016~%
 \(uuid-number-to-byte-array \(slot-value *uuid-namespace-dns* '%uuid_node\)\)
 => #\(192 79 212 48 200\)
      LSB...........MSB~%
 (elt #(192 79 212 48 200) 4) => MSB~%
 (elt #(192 79 212 48 200) 0) => LSB~%
 (uuid-request-integer (uuid-number-to-byte-array 825973027016) 0 5)~%
 => 825973027016~%~@
;; Roundtrip it:~%
 (multiple-value-bind (ba len) (uuid-number-to-byte-array 825973027016)
   (uuid-request-integer ba 0 len))
 => 825973027016~%~@
;; Verify it works on 3 octet numbers:
 \(let \(\(rnd-trip-17 #x1FFFF\)
       \(rslt nil\)\)
   \(setf rslt
         \(multiple-value-bind \(arr len\) \(uuid-number-to-byte-array rnd-trip-17\)
           \(request-integer  arr 0 len\)\)\)
   \(setf rslt `\(,\(eql rnd-trip-17 rslt\) ,rslt\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

#+nil
(fundoc 'uuid-get-bytes-for-integer
"Return the number of octets required to represent UNSIGNED-INTEGER.~%~@
:EXAMPLE~%~@
 \(uuid-get-bytes-for-integer 281474976710655\)~%
 \(uuid-get-bytes-for-integer #b11111111111111111\)~%~@
;; Fails on integers larger than 48 bits:
 \(uuid-get-bytes-for-integer 281474976710656\)~%~@
:NOTE In teh following macro expansion, a 3 octet integer will return 4:~%
 \(macroexpand-1 '\(uuid-get-bytes-for-integer 0\)\)~%~@
E.g. the following integer is in the third octet:~%~@
 #b11111111111111111
    2        1      ~%
 \(typep #b11111111111111111 '\(unsigned-byte 17\)\)~%~@
However, RFC4122 doesn't specify that any sub-value of the UUID will be 3
octet integer so we play it safe and bump that puppy up to four.~%~@
:SEE-ALSO `<XREF>'.~%►►►")



#+nil
(fundoc 'uuid-digest-uuid-string ; ######
  "Helper function producing an ironclad digest.~%~@
Reutrn value is an object of type `uuid-byte-array-16' and will satisfy the
precicate `uuid-byte-array-16-p'.~%~@
DIGEST-VERSION is a symbol suitable for uase as an argument to
`ironclad:make-digest'. It is is either :MD5 or :SHA1, and should satisfy
`%verify-digest-version' an error is signaled if not.~%~@
UUID is an instance of unique-universal-identifier class.
Used for the generation of UUIDv3 and UUIDv5 UUID by `make-v5-uuid' and `make-v3-uuid'.
:EXAMPLE~%~@
 \(uuid-digest-uuid-string 5 \(uuid-get-bytes \(uuid-print-bytes nil *uuid-namespace-dns*\)\) \"bubba\"\)
:SEE-ALSO `<XREF>'.~%►►►")

#+nil
(fundoc 'uuid-get-bytes ; ######
            "Convert UUID-STRING to a string of characters.~%~@
UUID-STRING is a is a string as returned by `uuid-print-bytes'.~%~@
Return value is constructed from the `cl:code-char' of each number in UUID-STRING.~%~@
Return value has is of type `uuid-byte-string' with the type signature:~%
 \(simple-array character \(16\)\)~%~@
And will satisfy the predicate `uuid-byte-string-p'.~%~@
Helper function for `make-v3-uuid' and `make-v5-uuid'.~%~@
:EXAMPLE~%
 \(uuid-get-bytes 
  \(uuid-print-bytes nil \(make-uuid-from-string \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)\)\)~%
\(uuid-get-bytes \"5E320838715730398383652D96705A7D\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

#+nil
(fundoc '%uuid-get-bytes-if ; ######
"Helper function for `uuid-get-bytes'.~%~@
Verify that arg CHK-UUID-STR is of type `uuid-hex-string-32'.~%~@
Signal an error if not.~%~@
:EXAMPLE~%~@
 \(%uuid-get-bytes-if \"6ba7b8109dad11d180b400c04fd430c8\"\)~%
 \(%uuid-get-bytes-if \"6BA7B8109DAD11D180B400C04FD430C8\"\)~%
 \(%uuid-get-bytes-if \"6ba7b8109dad11d180b400c04fd430c8-Q\"\)~%~@
:SEE-ALSO `uuid-hex-string-32-p'.~%►►►")

#+nil
(fundoc 'uuid-load-bytes ; ######
 "Helper function.~%~@
Load as if by `cl:dpb' the bytes of BYTE-ARRAY.~%~@
Return bytes set as integer values.~%~@
keyword BYTE-SIZE is a byte width to set. Default is 8.~%~@
keyword START is the position in BYTE-ARRAY to begin setting bytes from. Default is 0.~%~@
END is the position to stop setting bytes.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

#+nil
(fundoc 'uuid-to-byte-array ; ######
  "Convert UUID to a byte-array.~%~@
Arg UUID should be an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
Return value is an array of type `uuid-byte-array-16' with the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
It will satisfy the predicate `uuid-byte-array-16-p'.
:EXAMPLE~%~@
 \(uuid-to-byte-array *uuid-namespace-dns*\)~%~@
:SEE-ALSO `uuid-from-byte-array'.~%►►►")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
