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
:SEE-ALSO `cl:random-state', `cl:random-state-p'.~%")

(vardoc '*uuid-allow-empty-string-name-args*
"When value is null `make-v3-uuid' and `make-v5-uuid' will error when their NAME
argumement is not of type `unicly::string-not-empty'.~%~@
:SEE-ALSO `*uuid-allow-null-like-namespace-args*',
`unicly::verify-sane-namespace-and-name', `unicly::%verify-non-empty-name-arg',
`unicly::%verify-non-null-namespace-arg'.~%")

(vardoc '*uuid-allow-null-like-namespace-args*
"When value is null, `make-v3-uuid' and `make-v5-uuid' will error when their
NAMESPACE argumement is an instance of the class `unique-universal-identifier'
with all of its slot-values satisfying `cl:zerop' but which doesn't satisfy
`unique-universal-identifier-null-p'.~%~@
:NOTE RFC 4122 probably didn't have CLOS in mind w/r/t the null-uuid and as
such when either of the following type of object is allowed as a NAMESPACE
argument it may produce unexpected results and with unexpected consequences
which may not be easily detected after the fact:~%
 \(unique-universal-identifier-null-p \(make-instance 'unique-universal-identifier\)\)
 \(unique-universal-identifier-null-p \(make-instance 'unique-universal-identifier-null\)\)
 \(unique-universal-identifier-null-p \(make-instance 'mop-frobbed-subclass-of-unique-universal-identifier\)\)
Their may be a performance impact when this argument is nil \(the default\) b/c we
will have to check the slot-values of each namespace arg for each evaluation
of `make-v5-uuid' / `make-v3-uuid'.~%~@
A reasonable way to avoid this impact is to cache the NAMESPACEs common to a
class you control and dynamically bind this variable inside a wrapper function:~%
 \(make-v5-uuid-with-cached-namespace \(name\)
   \(let \(\(*uuid-allow-null-like-namespace-args* t\)\)
     \(make-v5-uuid <CACHED-NAMESPACE> name\)\)\)~%~@
:SEE-ALSO `*uuid-allow-empty-string-name-args*', `unicly::verify-sane-namespace-and-name',
`unicly::%verify-non-empty-name-arg',
`unicly::%verify-non-null-namespace-arg'.~%")

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
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%")
 
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
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%")

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
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%")

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
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v4-uuid'.~%")

(vardoc '*uuid-null-uuid*
         "The default null-uuid per RFC4122 Section 4.1.7 \"Nil UUID\".~%
The value of this variable is initialized at loadtime with
`unicly::%make-null-uuid-loadtime'.~%~@
User code should not neither direclty access nor set the value of this
variable and should instead always use `unicly:make-null-uuid'!~%~@
It is an an object of type `unicly::unique-universal-identifier-null'.~%~@
The value of this variable should always evalute to the same object as there
are methods specialized it, e.g.:~%~@
 \(uuid-eql *uuid-null-uuid* *uuid-null-uuid*\)~%
 \(uuid-eql *uuid-null-uuid* \(make-null-uuid\)\)~%
 \(uuid-eql \(make-null-uuid\) *uuid-null-uuid*\)~%
 \(uuid-eql *uuid-null-uuid* \(make-instance 'unicly::unique-universal-identifier-null\)\)~%
 \(uuid-eql *uuid-null-uuid* \(make-instance 'unicly::unique-universal-identifier\)\)~%
:NOTE The value of this variable should always return t for the following:~%
 \(and \(slot-exists-p *uuid-null-uuid* '%uuid_null\)
      \(slot-value *uuid-null-uuid* '%uuid_null\)\)~%~@
:SEE-ALSO `<XREF>'.~%")


;;; ==============================
;;; :UUID-TYPES-DOCUMENTATION
;;; ==============================

(typedoc 'uuid-unsigned-byte-size
"An object of type: \(unsigned-byte <SIZE>\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-ub128', `uuid-ub64', `uuid-ub48', `uuid-ub32', `uuid-ub16',
`uuid-ub8'.~%")

(typedoc 'uuid-ub128
"An object of type: \(unsigned-byte 128\)~%~@
Octets:        16
Bits:          128
Hex value:     #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
Decimal value: 340282366920938463463374607431768211455
Octal value:   #o3777777777777777777777777777777777777777777~%~@
An object of this type is capable of representing the combined the integer value
of the slots of class `unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO 
`uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48', `uuid-ub32',
`uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-ub64
"An object of type: \(unsigned-byte 64\)~%~@
An object of this type is capable of representing any integer value
of a  slot of class `unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48',
`uuid-ub32', `uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-ub48
"An object of type: \(unsigned-byte 48\)~%~@
Octets:         6
Bits:          48
Hex value:     #xFFFFFFFFFFFF
Decimal value: 281474976710655
Octal value:   #o7777777777777777
:Binary value: #b111111111111111111111111111111111111111111111111~%~@
Capable of representing the integer value of slot `%uuid_node' of class
`unique-universal-identifier'.~%~@
An object of this type is capable of representing any integer value
of a  slot of class `unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48',
`uuid-ub32', `uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-ub32
"An object of type: \(unsigned-byte 32\)~%~@
Octets:         4
Bits:          32 
Hex value:     #xFFFFFFFF
Decimal value: 4294967295
Octal value:   #o37777777777
Binary value:  #b11111111111111111111111111111111~%~@
Capable of representing the integer value of slot `%uuid_time-low' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48',
`uuid-ub32', `uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-ub16
"An object of type: \(unsigned-byte 16\)~%~@
Octets:          2
Bits:           16 
Hex value:      #xFFFF
Decimal value:  65535
Octal value:    #o177777
Binary value:   #b1111111111111111~%~@
Capable of representing the integer value of slots `%uuid_time-mid' and
`%uuid_time-high-and-version' of class `unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48',
`uuid-ub32', `uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-ub8
         "An object of type: \(unsigned-byte 8\)~%~@
Octets:         1
Bits:           8
Hex value:      #xFF
Decimal value:  255
Octal value:    #o377
Binary value:   #b11111111~%~@
Capable of representing integer of any any element in an object of type
`uuid-byte-array' and the and the integer value of slots
`%uuid_clock-seq-and-reserved' `%uuid_clock-seq-low' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-unsigned-byte-size', `uuid-ub128', `uuid-ub64', `uuid-ub48',
`uuid-ub32', `uuid-ub16', `uuid-ub8'.~%")

(typedoc 'uuid-string-36
"An object with the type signature: \(array character \(32\)\)
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-byte-string', `uuid-byte-array-16', `uuid-string-32',
`uuid-string-36', `uuid-hex-string-32', `uuid-hex-string-36',
`uuid-hex-string-32-p', `uuid-hex-string-36-p',.~%")

(typedoc 'uuid-string-32
"An object with the type signature: \(array character \(36\)\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-byte-string', `uuid-byte-array-16', `uuid-string-32',
`uuid-string-36', `uuid-hex-string-32', `uuid-hex-string-36',
`uuid-hex-string-32-p', `uuid-hex-string-36-p'.~%")

(typedoc 'uuid-hex-string-36
"An object of type \(array character \(36\)\) satsfying `uuid-hex-string-36-p'.~%~@
:EXAMPLE~%
 (type ~%~@
 \(typep \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\" 'uuid-hex-string-36\)~%
 \(typep \"6BA7B810-9DAD-11D1-80B4-00C04FD430C8\" 'uuid-hex-string-36-p\)~%~@
;; Following fails successfully:~%
 \(typep \"6BA7B810--9DAD-11D1-80B4-00C04FD430C8\ 'uuid-hex-string-36-p\)~%~@
:SEE-ALSO `<XREF>'.~%")

(typedoc 'uuid-hex-string-32
"An object with the type signature:~%
 \(array character \(32\)\)~%~@
And satisfies `mon:string-all-hex-char-p'.~%~@
:EXAMPLE~%
 \(typep \(uuid-print-bytes nil *uuid-namespace-dns*\) 'uuid-hex-string-32\)~%
;; Following fails successfully:~%
 \(typep \(print-object *uuid-namespace-dns* nil\) 'uuid-hex-string-32\)~%~@
:SEE-ALSO `<XREF>'.~%")

(typedoc 'uuid-byte-string
 "An object of with type signature: \(simple-array character \(16\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-get-bytes \(uuid-print-bytes nil *uuid-namespace-dns*\)\) 'uuid-byte-string\)~%~@
:SEE-ALSO `uuid-byte-array-16', `uuid-hex-string-36'.~%")

(typedoc 'uuid-bit-vector
"An object of type \(simple-bit-vector <LENGTH>\).
Objects of this type are capable of representing as bit-vectors the integer
values of slots of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector\)~%~@
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

;; uuid-bit-vector-128

(typedoc 'uuid-bit-vector-128
"An object of type \(simple-bit-vector 128\) capable of representing all 128
bits of a `unique-universal-identifier'~%~@
Objects of this type have a length of type `uuid-bit-vector-128-length' and may
be used to represent the integer value of the combined slots of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-128\)~%~@
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

(typedoc 'uuid-bit-vector-48
"An object of type \(simple-bit-vector 48\) capable of representing 48
bits of a `unique-universal-identifier'~%~@
Objects of this type have a length of type `uuid-bit-vector-48-length', and may
be used to represent the integer values of the slot `%uuid_node' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  (subseq \(uuid-bit-vector-128-zeroed\) 0 48) 'uuid-bit-vector-48\)~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-128\)~%
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

(typedoc 'uuid-bit-vector-32
"An object of type \(simple-bit-vector 32\) capable of representing 32
bits of a `unique-universal-identifier'~%~@
Objects of this type have a length of type `uuid-bit-vector-32-length', and may
be used to represent the integer values of the slot `%uuid_time-low' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  (subseq \(uuid-bit-vector-128-zeroed\) 0 32) 'uuid-bit-vector-32\)~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-32\)~%~@
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

(typedoc 'uuid-bit-vector-16
"An object of type \(simple-bit-vector 16\) capable of representing 16
bits of a `unique-universal-identifier'~%~@
Objects of this type have a length of type `uuid-bit-vector-16-length', and may
be used to represent the integer values of the slots `%uuid_time-mid' and
`%uuid_time-mid' of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  (subseq \(uuid-bit-vector-128-zeroed\) 0 16) 'uuid-bit-vector-16\)~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-128\)~%~@
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

(typedoc 'uuid-bit-vector-8
"An object of type \(simple-bit-vector 128\) capable of representing 8
bits of a `unique-universal-identifier'~%~@
Objects of this type have a length of type `uuid-bit-vector-8-length', and may
be used to represent an `uuid-ub8' value of any sub-sequence of bits in a
`uuid-bit-vector-128' and specifically those integer values of the slots
`%uuid_clock-seq-and-reserved' and `%uuid_clock-seq-low' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep  (subseq \(uuid-bit-vector-128-zeroed\) 0 8) 'uuid-bit-vector-8\)~%
 \(typep  \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-128\)~%~@
:SEE-ALSO `uuid-bit-vector', `uuid-bit-vector-128', `uuid-bit-vector-48',
`uuid-bit-vector-32', `uuid-bit-vector-16', `uuid-bit-vector-8'.~%")

(typedoc 'uuid-bit-vector-length
"An object of type (integer <SIZE> <SIZE>).~%~@
:EXAMPLE~%
 \(typep 16 '\(uuid-bit-vector-length 16\)\) => T~%
 \(typep 17 '\(uuid-bit-vector-length 16\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-bit-vector-128-length
"An object of type \(uuid-bit-vector-length 128\).~%~@
Objects of this type correspond with the length of a simple-bit-vector of type
`uuid-bit-vector-128' which are used to represent the integer value of the
combined slots of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep 128 '\(uuid-bit-vector-128-length\)) => T~%
 \(typep 127 '\(uuid-bit-vector-127-length\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-bit-vector-48-length
"An object of type \(uuid-bit-vector-length 48\).~%~@
Objects of this type correspond with the length of a simple-bit-vector of type
`uuid-bit-vector-48' which are used to represent the integer values of the slot
`%uuid_node' of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep 48 '\(uuid-bit-vector-48-length\)) => T~%
 \(typep 47 '\(uuid-bit-vector-48-length\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-bit-vector-32-length
"An object of type \(uuid-bit-vector-length 32\).~%~@
Objects of this type correspond with the length of a simple-bit-vector of type
`uuid-bit-vector-32' which are used to represent the integer values of the slot
`%uuid_time-low' of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep 32 '\(uuid-bit-vector-32-length\)) => T~%
 \(typep 31 '\(uuid-bit-vector-31-length\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-bit-vector-16-length
"An object of type \(uuid-bit-vector-length 16\).~%~@
Objects of this type correspond with the length of a simple-bit-vector of type
`uuid-bit-vector-16' which are used to represent the integer values of the slots
`%uuid_time-mid' and `%uuid_time-high-and-version' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep 16 '\(uuid-bit-vector-16-length\)) => T~%
 \(typep 17 '\(uuid-bit-vector-16-length\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-bit-vector-8-length
"An object of type \(uuid-bit-vector-length 8\).~%~@
Objects of this type correspond with the length of a simple-bit-vector of type
`uuid-bit-vector-8' which are used to represent the integer values of the slots
`%uuid_clock-seq-low'  and `%uuid_clock-seq-and-reserved' of class
`unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(typep 8 '\(uuid-bit-vector-8-length\)) => T~%
 \(typep 8 '\(uuid-bit-vector-8-length\)\) => NIL~%~@
:SEE-ALSO `uuid-bit-vector-length', `uuid-bit-vector-128-length',
`uuid-bit-vector-48-length', `uuid-bit-vector-32-length',
`uuid-bit-vector-16-length', `uuid-bit-vector-8-length'.~%")

(typedoc 'uuid-byte-array-16
 "An object which has the type signature: \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-to-byte-array *uuid-namespace-dns*\) 'uuid-byte-array-16\)~%~@
:SEE-ALSO `uuid-hex-string-36'.~%")

(typedoc 'uuid-byte-array-20
 "An object which has the type signature: \(simple-array \(unsigned-byte 8\) \(20\)\)~%~@
:EXAMPLE~%
 \(typep \(uuid-string-to-sha1-byte-array \"bubba\"\) 'uuid-byte-array-20\)~%~@
:SEE-ALSO `uuid-hex-string-36'.~%")

(typedoc 'uuid-byte-array-null
        "An object of type `unicly:uuid-byte-array-16' with each element `cl:zerop'.~%~@
:EXAMPLE~%~@
 \(typep \(uuid-byte-array-16-zeroed\) 'uuid-byte-array-null\)~%
 \(typep \(make-array 16 :element-type 'uuid-ub8 :initial-element 0\) 'uuid-byte-array-null\)~%
 \(not \(typep \(make-array 16 :element-type 'uuid-ub8 :initial-element 1\) 'uuid-byte-array-null\)\)~%
 \(not \(typep \(uuid-bit-vector-128-zeroed\) 'uuid-byte-array-null\)\)~%
:SEE-ALSO `uuid-byte-array-16-zeroed', `uuid-byte-array-16-p'.~%")

(typedoc 'uuid-bit-vector-null
"An object of type `unicly:uuid-bit-vector-128' and satisfying
`unicly:uuid-bit-vector-eql'.~%~@
:EXAMPLE~%
  \(typep \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-null\)~%
  \(typep \(make-array 128 :element-type 'bit :initial-element 1\) 'uuid-bit-vector-null\)~%
  \(not \(typep \(uuid-byte-array-16-zeroed\) 'uuid-bit-vector-null\)\)~%~@
:SEE-ALSO `uuid-bit-vector-128', `uuid-bit-vector-128-zeroed',
`uuid-byte-array-16-zeroed', `uuid-byte-array-null'.~%")

(typedoc 'uuid-simple-vector-5
        "Objects of this type are passed as the cl:nth-value 1 by funcitions which
frob objects of type `uuid-hex-string-36'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-delimited-string-36-p', `uuid-hex-string-36-p', 
`%uuid-hex-string-36-null-string-p', 
 `make-uuid-from-string-if', `make-uuid-from-string'.~%")

(typedoc 'uuid-hex-string-length
"A simple-array with element-type  character of length STRING-LENGTH ~%~@
:EXAMPLE~%~@
 \(typep \(svref #\(\"e3115c49\" \"6e13\" \"4d21\" \"9a37\" \"a1af250a8f88\"\) 4\) '\(uuid-hex-string-length 12\)\)~%~@
:SEE-ALSO `uuid-hex-string-length', `uuid-hex-string-12', `uuid-hex-string-8',
`uuid-hex-string-4', `uuid-hex-string-2'.~%")

(typedoc 'uuid-hex-string-12
"An object of type `uuid-hex-string-length' with length 12.~%~@
:EXAMPLE~%~@
 \(typep \(svref #\(\"e3115c49\" \"6e13\" \"4d21\" \"9a37\" \"a1af250a8f88\"\) 4\) 'uuid-hex-string-12\)~%~@
:SEE-ALSO `uuid-hex-string-length', `uuid-hex-string-12', `uuid-hex-string-8',
`uuid-hex-string-4', `uuid-hex-string-2'.~%")

(typedoc 'uuid-hex-string-8
"An object of type `uuid-hex-string-length' with length 8. ~%~@
:EXAMPLE~%~@
 \(typep \(svref #\(\"e3115c49\" \"6e13\" \"4d21\" \"9a37\" \"a1af250a8f88\"\) 0\) 'uuid-hex-string-8\)~%~@
:SEE-ALSO `uuid-hex-string-length', `uuid-hex-string-12', `uuid-hex-string-8',
`uuid-hex-string-4', `uuid-hex-string-2'.~%")

(typedoc 'uuid-hex-string-4
"An object of type `uuid-hex-string-length' with length 4.~%~@
:EXAMPLE~%~@
 \(typep \(svref #\(\"e3115c49\" \"6e13\" \"4d21\" \"9a37\" \"a1af250a8f88\"\) 1\) 'uuid-hex-string-4\)~%~@
:SEE-ALSO `uuid-hex-string-length', `uuid-hex-string-12', `uuid-hex-string-8',
`uuid-hex-string-4', `uuid-hex-string-2'.~%")

(typedoc 'uuid-hex-string-2
"An object of type `uuid-hex-string-length' with length   ~%~@
:EXAMPLE~%~@
 \(typep \(subseq \(svref #\(\"e3115c49\" \"6e13\" \"4d21\" \"9a37\" \"a1af250a8f88\"\) 3\) 0 2\) 'uuid-hex-string-2\)~%~@
:SEE-ALSO `uuid-hex-string-length', `uuid-hex-string-12', `uuid-hex-string-8',
`uuid-hex-string-4', `uuid-hex-string-2'.~%")

(typedoc 'uuid-version-int 
         "An integer value in the range [0,5].~%~@
Range represents the possible return values for `unicly:uuid-version-uuid' when
an object is a UUID as per return value of:~%
 `unicly:make-v3-uuid', `unicly:make-v4-uuid', `unicly:make-v5-uuid'~%~@
and exclusive of return value for `unicly:make-null-uuid'.~%~@
:EXAMPLE~%
 \(loop 
    for x from 0 below 6
    for y = \(typep x 'uuid-version-int\)
    always y\)~%~@
:SEE-ALSO `unicly::uuid-v3-or-5-int', `unicly::uuid-v3-4-or-5-int', `unicly::uuid-version-bit-vector'.~%")

(typedoc 'uuid-v3-or-5-int
         "An integer value either 3 or 5.~%~@
Range represents the possible return values for `unicly:uuid-version-uuid' when
an object is a UUID as per return value of:~%
 `make-v3-uuid'  `make-v5-uuid'~%~@
:EXAMPLE~%
 \(equal \(subseq \(mapcar #'\(lambda \(x\) \(typep x 'uuid-v3-or-5-int\)\)
                        \(loop for x from 2 below 7 collect x\)\)
                1 4\)
        \(list T NIL T\)\)~%
 \(equal \(mapcar #'\(lambda \(x\) \(typep \(uuid-version-uuid x\) 'uuid-v3-or-5-int\)\)
                \(list \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)
                      \(make-v4-uuid\)
                      \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
        \(list T NIL T\)\)~%~@
:SEE-ALSO `unicly::uuid-version-int', `unicly::uuid-v3-4-or-5-int', `unicly::uuid-version-bit-vector'.~%")

(typedoc 'uuid-v3-4-or-5-int
         "An integer value in the range [3,5].~%~@
Range represents the possible return values for `unicly:uuid-version-uuid' when
an object is a UUID as per return value of:~%
 `make-v3-uuid' `make-v4-uuid' `make-v5-uuid'~%~@
:EXAMPLE~%
 \(equal \(subseq \(mapcar #'\(lambda \(x\) \(typep x 'uuid-v3-4-or-5-int\)\)
                        \(loop for x from 2 below 7 collect x\)\)
                1 4\)
        \(list T T T\)\)~%~@
 \(equal \(mapcar #'\(lambda \(x\) \(typep \(uuid-version-uuid x\) 'uuid-v3-4-or-5-int\)\)
                \(list \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)
                      \(make-v4-uuid\)
                      \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
  \(list T T T\)\)~%~@
:SEE-ALSO `unicly::uuid-version-int', `unicly::uuid-v3-or-5-int', `unicly::uuid-version-bit-vector'.~%")


;;; ==============================
;;; :UUID-MACRO-DOCUMENTATION
;;; ==============================
;; For whaterver reason Clisp doesn't like evaluating fundoc for macros...
#-clisp
(progn
(fundoc 'def-uuid-type-definer
        "Convenience macro for use by macros which define uuid sub-type specifiers.~%~@
Arg PARENT-TYPE is an unquoted symbol naming a uuid-type specifer which
accepts a positive integer value as an argument.~%~@
FORMAT-STRING is a format-string used to generate the symbol-name of the type to be defined.
The types defined with by expanders of this macro have an integer value as
part of theier symbol name, as such format-string should contain the
format-control flag \"~~D\" in an appropriate position.~%~@
LENGTH-ARG an positive integer value. It is used as the format-argument to
FORMAT-STRING and as the argument to its PARENT-TYPE.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-type-definer uuid-bit-vector \"UUID-BIT-VECTOR-~~D\" 128\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length', `def-uuid-byte-array-length'.~%")

(fundoc 'def-uuid-unsigned-byte-size
"Convenience macro for defining subtypes of type `uuid-unsigned-byte-size'.~%~@
Arg SIZE-BYTES is an positive integer value indicating the size of an unsigned-byte.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-unsigned-byte-size 128\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length', `def-uuid-byte-array-length'.~%")

(fundoc 'def-uuid-bit-vector-N-type
        "Convenience macro for defining subtypes of type `uuid-bit-vector'.~%~@
Arg BV-LENGTH-TYPE is an positive integer value indicating the number of bits in a uuid-bit-vector.~%~@
:EXAMPLE~%
  \(macroexpand-1 '\(def-uuid-bit-vector-N-type 16\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length', `def-uuid-byte-array-length'.~%")

(fundoc 'def-uuid-bit-vector-length-type
        "Convenience macro for defining subtypes of type `uuid-bit-vector-length'.~%~@
Arg BV-LENGTH is an positive integer value indicating the length of a uuid-bit-vector.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-bit-vector-length-type 16\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length', `def-uuid-byte-array-length'.~%")

(fundoc 'def-uuid-byte-array-length
        "Convenience macro for defining subtypes of type `uuid-byte-array'.~%~@
BYTE-ARRAY-LENGTH is a positive integer indication the length of a simple-array with elements of type `uuid-ub8'.
:EXAMPLE~%~@
 \(macroexpand-1 '\(def-uuid-byte-array-length 16\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length', `def-uuid-byte-array-length'.~%")

(fundoc 'def-uuid-unsigned-byte-integer-length
"Convenience macro for defining subtypes of type `uuid-unsigned-byte-integer-length'.~%~@
Arg UNSIGNED-LENGTH is an positive integer value indicating the
integer-length of the integer represetned at the upper-bounds of an
unsigned-byte.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-unsigned-byte-integer-length 16\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length'.~%")

(fundoc 'def-uuid-uuid-hex-string-length
"Convenience macro for defining subtypes of type `uuid-hex-string-length'.~%~@
Arg HEX-STRING-LENGTH is an positive integer value indicating the length of a simple-array of characters.~%~@
:EXAMPLE~%
\(macroexpand-1 '\(def-uuid-uuid-hex-string-length 12\)\)~%~@
:SEE-ALSO `def-uuid-type-definer', `def-uuid-unsigned-byte-size',
`def-uuid-bit-vector-N-type', `def-uuid-bit-vector-length-type',
`def-uuid-unsigned-byte-integer-length',
`def-uuid-uuid-hex-string-length'.~%")

(fundoc 'def-indexed-hexstring-integer-parser
        "Convenience macro for defining functions which `cl:parse-integer' of the
simple-array character elements returnd as nth-value 1 as an object of type
`uuid-simple-vector-5' by `uuid-hex-string-36-p'~%~@
FUN-NAME is a string used when generating a function-name to intern in the UNICLY package.
String should not be preceded/trailed by #\\- or whitespace.~%~@
Arg VEC-INDEX is an integer value of type (mod 5) indicating an index into the
return value of `uuid-hex-string-36-p'.~%~@
Arg STRING-TYPE-AT-INDEX is an unqouted symol naming a subtype of
`uuid-hex-string-length'. Valid types are:~%
 `uuid-hex-string-8' `uuid-hex-string-4' `uuid-hex-string-12'~%~@
Arg STRING-START is a postivie integer indicating the lower bounds of the
hex-string located at VEC-INDEX.~%~@
Arg STRING-END is a postivie integer value indicating an upper bounds of the
hex-string located at VEC-INDEX.~%~@
STRING-INTEGER-TYPE Is an unquoted symbol naming an uuid unsigned-byte type to
declare for the value returned by `cl:parse-integer' of the string at
VEC-INDEX. Valid types are:~%
 `uuid-ub32' `uuid-ub16' `uuid-ub8' `uuid-ub48'~%~@
Expands to a function defining form with the following format:~%
 \(defun <FUN-NAME> \(hex-vector-5\)
   \(declare \(uuid-simple-vector-5 hex-vector-5\)
            \(optimize \(speed 3\)\)\)
   \(uuid-string-parse-integer 
    \(uuid-svref-for-parse-integer HEX-VECTOR-5 <VEC-INDEX> <STRING-TYPE-AT-INDEX>\)
    <STRING-START> <STRING-END> <STRING-INTEGER-TYPE> \)\)~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-indexed-hexstring-integer-parser
                  \"time-low\"
                  0
                  uuid-hex-string-8
                  0 8 uuid-ub32\)\)~%~@
:SEE-ALSO `uuid-hex-vector-parse-time-low', `uuid-hex-vector-parse-time-mid',
`uuid-hex-vector-parse-time-high-and-version',
`uuid-hex-vector-parse-clock-seq-and-reserved',
`uuid-hex-vector-parse-clock-seq-low', `uuid-hex-vector-parse-node'.~%")

(fundoc 'uuid-svref-for-parse-integer
        "Convenience macro for declared references to STRING-TYPEs at INDEX of SIMPLE-VECTOR-5.~%~@
Wrapped by macros `def-indexed-hexstring-integer-parser' and `uuid-string-parse-integer'.~%~@
SIMPLE-VECTOR-5 is an object of type `uuid-simple-vector-5' as returned as the
`cl:nth-value' 1 of `uuid-hex-string-36-p'.~%~@
STRING-TYPE is a subtype of `uuid-hex-string-length' ~%~@
Arg STRING-TYPE is an unqouted symol naming the type of string at INDEX. It
should be a subtype of `uuid-hex-string-length'. Valid types are:~%
`uuid-hex-string-8' `uuid-hex-string-4' `uuid-hex-string-12'~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(uuid-svref-for-parse-integer
                  \(nth-value 1 \(uuid-hex-string-36-p \(uuid-princ-to-string \(make-v4-uuid\)\)\)\)
                  4 uuid-hex-string-12\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-string-parse-integer
"Helper macro for `make-uuid-from-string' to `cl:parse-integer' a hex-string.~%~@
Wraps macro `uuid-svref-for-parse-integer'.~%~@
Wrapped by macro `def-indexed-hexstring-integer-parser'.~%~@
Arg UUID-HEX-STRING is a `uuid-hex-string-36' or a subtype.~%~@
Arg START is a lower bounds indexing into UUID-HEX-STRING.~%~@
Arg END is an upper bounds indexing into UUID-HEX-STRING.~%~@
INTEGER-TYPE is an unquoted symbol naming the type of uuid-unsigned-byte-size to
declare for the value returned by `cl:parse-integer' of the hex-string at at
index bounded by START and END Valid types are:~%
 `uuid-ub32' `uuid-ub16' `uuid-ub8' `uuid-ub48'~%~@
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
:SEE-ALSO `make-uuid-from-string-if'.~%")

(fundoc 'def-uuid-predicate-and-type-check-definer
        "Convenience macro for defining predicate and check-type functions for Unicly types.~%~@
TYPE-FOR-PRED-AND-CHECK is a token designating an existing type-specifier.
Its symbol-name is used to generate two symbol-names to use as function names when defining a
the predicate and check-type functions.
Generated predicate name has the form:~%~@
 <TYPE-FOR-PRED-AND-CHECK>-P
Generated check-type name has the form:
 <TYPE-FOR-PRED-AND-CHECK>-CHECK-TYPE
So given the type specifier UUID-BIT-VECTOR-8
the following two functions would be definened:
 uuid-bit-vector-8-p uuid-bit-vector-8-check-type
In the case, uuid-bit-vector-8-check-type is defined with a body which checks if
some value satisfies uuid-bit-vector-8-p and if not signals a condition of type
`uuid-simple-type-error' its body having the format:~%
 \(unless \(uuid-bit-vector-8-p <SOME-VALUE>\)
   \(uuid-simple-type-error :datum <SOME-VALUE> 
                           :expected-type uuid-bit-vector-8\)\)~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-predicate-and-type-check-definer uuid-bit-vector-32\)\)
 \(macroexpand-all '\(def-uuid-predicate-and-type-check-definer uuid-bit-vector-32\)\)~%~@
:NOTE The body of this macro expands into two addtional macros:~%
 `unicly::def-uuid-type-predicate-definer'
 `unicly::def-uuid-type-check-definer'~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'def-uuid-request-integer-bit-vector
 "Convenience macro for functions which extract slot values of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"time-low\" 0  32\)\)~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"time-mid\" 32 16\)\)~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"time-high-and-version\" 48 16\)\)~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"clock-seq-and-reserved\" 64 8\)\)~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"clock-seq-low\"          72 8\)\)~%
 \(macroexpand-1 '\(def-uuid-request-integer-bit-vector \"node\"                   80 48\)\)~%~@
:SEE-ALSO 
`%uuid_time-low-request-bit-vector', `%uuid_time-mid-request-bit-vector',
`%uuid_time-high-and-version-request-bit-vector',
`%uuid_clock-seq-and-reserved-request-bit-vector',
`%uuid_clock-seq-low-request-bit-vector', `%uuid_node-request-bit-vector'.~%")
)


;;; ==============================
;;; :UUID-TYPE-PREDICATE-DOCUMENTATION
;;; ==============================

(fundoc 'uuid-string-32-p
"Whether object is of type `uuid-string-32'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-string-36-p
"Whether object is of type `uuid-string-36'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-hex-string-32-p
"Whether object is of type `uuid-hex-string-32'.~%~@
Return T when object has the type signature:~%
 \(array character \(32\)\)~%~@
and satisfies `mon:string-all-hex-char-p'.~%~@
:EXAMPLE~%
 \(uuid-hex-string-32-p \(uuid-print-bytes nil *uuid-namespace-dns*\)\)~%
;; Following fails successfully:~%
 \(uuid-hex-string-32-p \(print-object *uuid-namespace-dns* nil\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%")

(fundoc 'uuid-delimited-string-36-p
"Whether MAYBE-DELIM-STRING-36 is potentially of type `uuid-hex-string-36' or `uuid-hex-string-36-zeroed'~%~@
Return nil when MAYBE-DELIM-STRING-36 is not `uuid-string-36-p'.
Else, return as if by `cl:values':
 cl:nth-value-0  is T
 cl:nth-value-1 is an object of type (simple-vector 5) 
 each elt of vector is a simple-string generated by splitting
 MAYBE-DELIM-STRING-36 at occurences of #\\-.
An object will satisfy `uuid-string-36-p' when following constraints are met:~%
 - Its type is '(array character 36)
 - When split to list of strings on the character delimter #\\- the list is of length is 5
 - The length of each string in the list matches the pattern 8-4-4-4-12
 - each string in the list satisfies is a hexadecimal character or #\\-~%~@
:EXAMPLE~%
 \(uuid-delimited-string-36-p \(uuid-princ-to-string \(make-v4-uuid\)\)\)~%
 \(uuid-delimited-string-36-p \(uuid-princ-to-string \(make-null-uuid\)\)\)~%
 \(uuid-delimited-string-36-p \(make-string 36 :initial-element #\\-\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

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
:SEE-ALSO `uuid-hex-string-32-p', `uuid-hex-string-32',
`%uuid-hex-string-36-null-string-p' `mon:split-string-on-chars'.~%")

(fundoc '%uuid-hex-string-36-null-string-p
"Whether MAYBE-ALL-ZERO-OR-DASH-STRING-36 is of type `uuid-hex-string-36' every
character is `cl:char=' either #\\0 or #\\-.~%~@
:EXAMPLE~%
 \(%uuid-hex-string-36-null-string-p \"00000000-0000-0000-0000-000000000000\"\)~%
 \(%uuid-hex-string-36-null-string-p \"00000000-0000-0000-0000-000000000001\"\)~%~@
:SEE-ALSO `uuid-hex-string-36-p'.~%")

(fundoc 'uuid-byte-array-16-p
"Whether MAYBE-UUID-BYTE-ARRAY-16 is of type `uuid-byte-array-16'.~%~@
Return T when MAYBE-UUID-BYTE-ARRAY-16 has the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-array-16-p \(uuid-to-byte-array *uuid-namespace-dns*\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%")

(fundoc 'uuid-byte-array-20-p
"Whether MAYBE-UUID-BYTE-ARRAY-16 is of type `uuid-byte-array-20'.~%~@
Return T when MAYBE-UUID-BYTE-ARRAY-16 has the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(20\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-array-20-p \(uuid-string-to-sha1-byte-array \"bubba\"\)\)~%~@
:SEE-ALSO `uuid-hex-string-36', `uuid-hex-string-36-p'.~%")

(fundoc 'uuid-byte-string-p
"Whether object is of type `uuid-byte-string'.~%~@
Return T when object has the type signature:~%
 \(simple-array character \(16\)\)~%~@
:EXAMPLE~%
 \(uuid-byte-string-p \(uuid-get-bytes \(uuid-print-bytes nil *uuid-namespace-dns*\)\)\)~%~@
:SEE-ALSO `uuid-hex-string-36-p', `uuid-byte-array-16-p'.~%")

(fundoc 'uuid-bit-vector-v3-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-version-bit-vector', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%")

(fundoc 'uuid-bit-vector-v4-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-version-bit-vector', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%")

(fundoc 'uuid-bit-vector-v5-p
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `uuid-version-bit-vector', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%")

(fundoc 'uuid-bit-vector-null-p
"Whether BIT-VECTOR-MAYBE-NULL is of type `unicly:uuid-bit-vector-null'.~%~@
:EXAMPLE~%
 \(uuid-bit-vector-null-p \(uuid-bit-vector-128-zeroed\)\)~%
 \(uuid-bit-vector-null-p \(make-array 128 :element-type 'bit :initial-element 1\)\)~%
 \(null \(uuid-bit-vector-null-p \(uuid-byte-array-16-zeroed\)\)\)~%~@
:SEE-ALSO `unicly:uuid-bit-vector-eql', `unicly:uuid-bit-vector-128-zeroed',
`unicly:uuid-bit-vector-128', `unicly:uuid-bit-vector-128-p'.~%")

(fundoc 'uuid-byte-array-null-p
        "Whether object is of type `unicly:uuid-byte-array-null'.~%~@
:EXAMPLE~%~@
 \(uuid-byte-array-null-p \(uuid-byte-array-16-zeroed\)\)~%
 \(uuid-byte-array-null-p \(make-array 16 :element-type 'uuid-ub8 :initial-element 0\)\)~%
 \(not \(uuid-byte-array-null-p \(make-array 16 :element-type 'uuid-ub8 :initial-element 1\)\)\)~%
 \(not \(uuid-byte-array-null-p \(uuid-bit-vector-128-zeroed\)\)\)~%~@
:SEE-ALSO `uuid-byte-array-16-zeroed', `uuid-byte-array-16-p'.~%")


;;; ==============================
;;; UUID-FUNCTIONS-DOCUMENTATION
;;; ==============================

(fundoc 'uuid-version-bit-vector
"Return the version of uuid version of UUID-BIT-VECTOR.~%~@
UUID-BIT-VECTOR is on object of type `uuid-bit-vector-128'.~%~@
When object is the null uuid return as if by `cl:values':
 0,null-uuid~%~@
If for some reason bit 48 of object is not `cl:zerop' an error is signaled.~%~@
:EXAMPLE~%
 \(uuid-version-bit-vector \(uuid-to-bit-vector \(make-v4-uuid\)\)\)~%
 \(uuid-version-bit-vector \(uuid-to-bit-vector 
                           \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(uuid-version-bit-vector \(uuid-to-bit-vector 
                           \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(uuid-version-bit-vector \(uuid-to-bit-vector \(make-null-uuid\)\)\)~%~@
;; Following successfully signals an error:~%
 \(let \(\(bv-z \(uuid-bit-vector-128-zeroed\)\)\)
   \(setf \(sbit bv-z 48\) 1\)
   \(uuid-version-bit-vector bv-z\)\)~%~@
:SEE-ALSO `uuid-version-bit-vector', `uuid-bit-vector-v3-p',
`uuid-bit-vector-v4-p' `uuid-bit-vector-v5-p', `uuid-bit-vector-128',
`uuid-eql'.~%")

(fundoc '%verify-slot-boundp-and-type
"Check that VERIFY-UUID has all slots `cl:slot-boundp' and of the correct type.~%~@
Signal either a `cl:simple-condition' or `cl:type-error' error if not.~%~@
Helper function for `uuid-copy-uuid'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

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
:NOTE Because there should only ever be one true instance of the null-uuid when
UUID-INSTANCE satisfies `unique-universal-identifier-null-p' the returned copy
of UUID-INSTANCE is an instance of class `unique-universal-identifier' not an
instance of class `unique-universal-identifier-null' and the returned copy does
not satisfy `unique-universal-identifier-null-p'. For example:~%
 \(unique-universal-identifier-null-p \(make-null-uuid\)\)~%
 \(unique-universal-identifier-null-p \(uuid-copy-uuid \(make-null-uuid\)\)\)~%
 \(type-of \(uuid-copy-uuid \(make-null-uuid\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-bit-vector-128-zeroed
"Return a bit vector of 128 elements with all elements zeroed.~%~@
:EXAMPLE~%
 \(uuid-bit-vector-128-zeroed\)~%
 \(typep \(uuid-bit-vector-128-zeroed\) 'uuid-bit-vector-128\)~%~@
:SEE-ALSO `uuid-bit-vector-128', `uuid-deposit-octet-to-bit-vector',
`uuid-byte-array-to-bit-vector', `make-null-uuid'.~%")

(fundoc 'uuid-byte-array-16-zeroed
"Return an array of type `uuid-byte-array-16' with all elements zeroed.~%~@
:EXAMPLE~%~@
 \(uuid-byte-array-16-zeroed\)~%
 \(typep \(uuid-byte-array-16-zeroed\) 'uuid-byte-array-16\)~%
 \(uuid-byte-array-16-p \(uuid-byte-array-16-zeroed\)\)~%~@
:SEE-ALSO `uuid-bit-vector-128-zeroed', `uuid-byte-array-16-p',
`uuid-byte-array-null-p', `uuid-byte-array-null'.~%")

(fundoc 'uuid-bit-vector-to-integer
  "Return BIT-VECTOR's representation as a positive integer.~%~@
BIT-VECTOR is an object of type `cl:simple-bit-vector'.~%~@
:EXAMPLE~%
 \(uuid-bit-vector-to-integer \(uuid-to-bit-vector \(make-v4-uuid\)\)\)~%~@
:NOTE This is a modified version of a a \"BIT-VECTOR-TO-INTEGER\" function
written by Stas Boukarev using `cl:flet' and `cl:loop'.
:SEE :FILE unicly/unicly.lisp for additional details.~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-bit-vector-to-byte-array
"Convert UUID-BV-128 to a UUID-BYTE-ARRAY-16.~%~@
Arg UUID-BV-128 should satisfy `uuid-bit-vector-128-check-type'.~%~@
:EXAMPLE~%
 \(equalp             
  \(uuid-bit-vector-to-byte-array \(uuid-to-bit-vector \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-request-integer
  "Decode an integer of LENGTH octets from ARRAY starting at OFFSET.~%~@
The number represented by BYTE-ARRAY may be any positive integer representable
in 48 bits.~%~@
The OFFSET is effectively :start and 
LENGTH is :end where :end => \(+ offset length\)
x86-32 OSs are LITTLE-ENDIAN but network-byte-order is big-endian...~%~@
:EXAMPLE~%
  \(uuid-request-integer \(uuid-number-to-byte-array 281474976710654\) 0 6\)~%
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-disassemble-ub48
"Return U48 as six integer values of type uuid-ub8 as if by `cl:values'.~%~@
U48 is an integer of type `uuid-ub48'. corresponding to the `%uuid_node' slot of
an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(multiple-value-list
  \(uuid-disassemble-ub48
   \(slot-value \(make-v5-uuid *uuid-namespace-dns* \"buuba\"\) '%uuid_node\)\)\)
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%")

(fundoc 'uuid-disassemble-ub32
"Return U32 as four integer values of type `uuid-ub8' as if by `cl:values'.~%~@
U32 is an integer of type `uuid-ub32' corresponding to the `%uuid_time-low' slot
of an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(slot-value  \(make-v4-uuid\) '%UUID_TIME-LOW\)~%
 \(uuid-disassemble-ub32 \(slot-value \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(equal \(multiple-value-list \(uuid-disassemble-ub16 13953\)\) '\(54 129\)\)~%~@
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%")

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
:SEE-ALSO `uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16'.~%")

(fundoc 'uuid-assemble-ub48
"Return an integer of type `uuid-ub48' constructed of the uuid-ub8 octets B1, B2, B3, B4, B5, and B6.~%
 HIGH ---> LOW
 B1   ...   B6~%~@
:EXAMPLE~%
 \(let \(\(ub48 #xFFFFFFFFFFFF\)\)
   \(multiple-value-bind \(b1 b2 b3 b4 b5 b6\) \(uuid-disassemble-ub48 ub48\)
     \(eql \(uuid-assemble-ub48 b1 b2 b3 b4 b5 b6\) ub48\)\)\)~%~@
:SEE-ALSO `uuid-assemble-ub32', `uuid-assemble-ub16',
`uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16',
`uuid-request-integer', `uuid-byte-array-16-to-integer'.~%")

(fundoc 'uuid-assemble-ub32
"Return an integer of type `uuid-ub32' constructed of the uuid-ub8 octets B1, B2, B3, and B4.~%
 HIGH ---> LOW
 B1   ...   B4~%~@
:EXAMPLE~%
 \(let \(\(ub32 #xFFFFFFFF\)\)
   \(multiple-value-bind \(b1 b2 b3 b4\) \(uuid-disassemble-ub32 ub32\)
     \(eql \(uuid-assemble-ub32 b1 b2 b3 b4\) ub32\)\)\)~%~@
:SEE-ALSO `uuid-assemble-ub48', `uuid-assemble-ub16',
`uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16',
`uuid-request-integer', `uuid-byte-array-16-to-integer'.~%")

(fundoc 'uuid-assemble-ub16
"Return an integer of type `uuid-ub48' constructed of the uuid-ub8 octets B1 and B2.~%
 HIGH ---> LOW
 B1   ...   B6~%~@
:EXAMPLE~%
 \(let \(\(ub16 #xFFFF\)\)
   \(multiple-value-bind \(b1 b2\) \(uuid-disassemble-ub16 ub16\)
     \(eql \(uuid-assemble-ub16 b1 b2\) ub16\)\)\)~%~@
:SEE-ALSO `uuid-assemble-ub48', `uuid-assemble-ub32',
`uuid-disassemble-ub48', `uuid-disassemble-ub32', `uuid-disassemble-ub16',
`uuid-request-integer', `uuid-byte-array-16-to-integer'.~%")

(fundoc 'uuid-get-namespace-bytes
  "Convert UUID to a byte-array.~%~@
Arg UUID should be an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
Return value is an array of type `uuid-byte-array-16' with the type signature:~%
 \(simple-array \(unsigned-byte 8\) \(16\)\)~%~@
It will satisfy the predicate `uuid-byte-array-16-p'.~%~@
:EXAMPLE~%
 \(uuid-get-namespace-bytes *uuid-namespace-dns*\)~%~@
:SEE-ALSO `uuid-from-byte-array'.~%")

(fundoc 'make-hash-table-uuid
        "Return a hash-table specialized to hold UUIDs as keys.~%~@
On SBCL this this is a hash-table with `uuid-eql' as `cl:hash-table-test'.~%~@
Keyword :SYNCHRONIZED when non-nile indicates hash-table may have multiple concurrent readers.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-ext:with-locked-hash-table'.~%")

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
:SEE-ALSO `uuid-eql', `uuid-copy-uuid', `uuid-to-bit-vector', `sb-int:bit-vector-='.~%")

(fundoc '%uuid_time-low-request
"Step three of RFC4122 Section 4.3:~%~@
 - Set octets zero through 3 of the time_low field to octets zero through 3 of the digest hash.~%~@
:EXAMPLE~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%
 \(%uuid_time-low-request \(uuid-get-namespace-bytes \(make-v4-uuid *uuid-namespace-dns* \"bubba\"\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%uuid_time-mid-request
"Step four of RFC4122 Section 4.3:~%~@
 - Set octets zero and one of the time_mid field to octets 4 and 5 of the digest hash.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

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
:SEE-ALSO `<XREF>'.~%")

(fundoc '%uuid_clock-seq-and-reserved-request
"Steps seven and eight of RFC4122 Section 4.3:~%~@
Step seven:~%
 - Set the clock_seq_hi_and_reserved field to octet 8 of the digest hash.~%~@
Step eight:~%
 - Set the two most significant bits (bits 6 and 7) of the clock_seq_hi_and_reserved
   to zero and one, respectively. IOW, set bit 6 to 0 - Set bit 7 to 1~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%uuid_clock-seq-low-request
"Step nine of RFC4122 Section 4.3:~%
 - Set the clock_seq_low field to octet 9 of the hash.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%uuid_node-request
"Step ten or RFC4122 Section 4.3:~%
 - Set octets zero through five of the node field to octets 10 through 15 of the digest hash.~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%verify-version-3-or-5
"Verify that arg VERSION is either the integer 3 or 5.~%~@
Signal an error if not.
Helper function for `digested-v3or5-uuid'.~%~@
:EXAMPLE~%
  \(%verify-version-3-or-5 3\)~%
  \(%verify-version-3-or-5 5\)~%~@
;; Following fails successfully:~%
  \(%verify-version-3-or-5 6\)~%~@
:SEE-ALSO `%verify-digest-version'.~%")

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
:SEE-ALSO `<XREF>'.~%")

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
:SEE-ALSO `%uuid-digest-uuid-instance-md5', `%uuid-digest-uuid-instance-sha1'.~%")

;; uuid-get-namespace-bytes
;; sb-ext:string-to-octets
;; flexi-streams:string-to-octets

(fundoc 'digested-v3or5-uuid ; ######
"Helper function to format UUIDv3 and UUIDv5 hashes according to UUID-VERSION.~%~@
DIGEST-BYTE-ARRAY is an argument suitable as the BYTE-ARRAY arg to `uuid-request-integer'.~%~@
DIGEST-BYTE-ARRAY is as per the the return value of `digested-v3or5-uuid'
UUID-VERSION should satisfy `%verify-version-3-or-5', an error is signaled if not.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")

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
:SEE-ALSO `make-v3-uuid', `make-v5-uuid', `make-v1-uuid', `make-null-uuid',
`*random-state-uuid*', `cl:random'.~%")

(fundoc 'make-v3-uuid ; ######
"Generate an RFC4122 version 3 (named based MD5) UUID for NAME in NAMESPACE.~%~@
Return value is an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
NAMESPACE is a UUID namespace object.~%~@
NAME is a string.~%~@
:EXAMPLE~%~@
 \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'make-v5-uuid ; ######
  "Generates an RFC4122 version 5 (nambe based SHA1) UUID with NAME in NAMESPACE.~%~@
Return value is an instance of the class UNIQUE-UNIVERSAL-IDENTIFIER.~%~@
NAMESPACE is a UUID namespace object.~%~@
NAME is a string.~%~@
:EXAMPLE~%
 \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%")

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
`uuid-string-parse-integer'.~%")

(fundoc 'make-null-uuid ; ######
"Generate a NULL UUID.~%~@
Per RFC4122 Setion 4.1.7. \"Nil UUID\": 
 The nil UUID is special form of UUID specified to have all 128 bits set to zero.~%~@
Return value is an instance of `unique-universal-identifier' with all slots
defaulted to 0.~%~@
Return value has the format:~%
 00000000-0000-0000-0000-000000000000~%~@
:EXAMPLE~%
 \(make-null-uuid\)~%~@
:NOTE Two null-uuids do not share identity:~%
 \(eq  \(make-instance 'unique-universal-identifier\) 
      \(make-instance 'unique-universal-identifier\)\)~%
 \(eql \(make-instance 'unique-universal-identifier\) 
      \(make-instance 'unique-universal-identifier\)\)~%
 \(equal  \(make-instance 'unique-universal-identifier\) 
         \(make-instance 'unique-universal-identifier\)\)~%
 \(equalp \(make-instance 'unique-universal-identifier\) 
         \(make-instance 'unique-universal-identifier\)\)~%~@
However, we can test with `uuid-eql' if they have the same representation in
uuid-bit-vector-128 format:~%
 \(uuid-eql
  \(make-instance 'unique-universal-identifier\) 
  \(make-instance 'unique-universal-identifier\)\)~%~@
:SEE-ALSO `uuid-bit-vector-128-zeroed'.~%")

(fundoc 'unique-universal-identifier-null-p
"Whether object is an instance of the class
`unicly::unique-universal-identifier-null' and its value is `cl:eq' that of
special variable `unicly::*uuid-null-uuid*'.~%~@
:EXAMPLE~%
 \(unique-universal-identifier-null-p *uuid-null-uuid*\)~%
 \(unique-universal-identifier-null-p \(make-null-uuid\)\)~%
 \(type-of \(make-instance 'unique-universal-identifier-null\)\)~%~@
;; Following both fail successfully:~%
 \(unique-universal-identifier-null-p \(make-instance 'unicly::unique-universal-identifier-null\)\)~%
 \(unique-universal-identifier-null-p \(make-instance 'unicly::unique-universal-identifier\)\)~%
:SEE-ALSO `unicly:unique-universal-identifier-p'.~%")

(fundoc 'uuid-from-byte-array ; ######
 "Convert BYTE-ARRAY to a UUID.~%~@
Return value is an instance of the UNIQUE-UNIVERSAL-IDENTIFIER class.~%~@
BYTE-ARRAY is a byte-array as returned by `uuid-to-byte-array' it should be of
type `uuid-to-byte-array' and satisfy the predicate
`uuid-to-byte-array-p':~%
:EXAMPLE~%~@
 \(uuid-from-byte-array \(uuid-to-byte-array \(make-uuid-from-string \"6ba7b814-9dad-11d1-80b4-00c04fd430c8\"\)\)\)
:SEE-ALSO `<XREF>'.~%")

(fundoc 'make-uuid-from-string-if
"Helper function for `make-uuid-from-string'.~%~@
If UUID-HEX-STRING-36-IF satisfies `uuid-hex-string-36-p' return it.
If the constraint fails signal a `mon:simple-error-mon' condition.~%~@
:EXAMPLE~%~@
 \(make-uuid-from-string-if \"6ba7b810-9dad-11d1-80b4-00c04fd430c8\"\)~%
 \(make-uuid-from-string-if \"6ba7b810-9dad--11d1-80b4-00c04fd430c8\"\)~%~@
:SEE-ALSO `<XREF>'.~%")

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
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-simple-type-error
        "Convenience function for signaling conditions of type `unicly::uuid-simple-type-error'.~%~@
Keywords DATUM and EXPECTED-TYPE are as per condition class CL:TYPE-ERROR.~%~@
:EXAMPLE~%
 \(uuid-simple-type-error :datum \"bubba\" :expected-type 'simple-bit-vector\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-valid-stream-p
"Whether STREAM is `cl:streamp' and `cl:open-stream-p'.
Return as if by `cl:values':~%
 cl:nth-value 0 is a boolean
 cl:nth-value 1 is value of STREAM arg~%~@
:EXAMPLE~%
 \(let \(\(os \(make-string-output-stream\)\)\)
   \(unwind-protect
        \(uuid-valid-stream-p os\)
     \(close os\)\)\)~%
 \(uuid-valid-stream-p \"bubba\"\)~%~@
:SEE-ALSO `uuid-valid-stream-verify-io-type',
`uuid-valid-stream-verify-for-input',
`uuid-valid-stream-verify-for-output'.~%")

(fundoc 'uuid-valid-stream-verify-io-type
"Return STREAM if it is valid for use as when writing a UUID representation.~%~@
Keyword DIRECTION is as per `cl:open'. Valid values are either:~%
 :INPUT :OUTPUT~%~@
An error is signaled if STREAM is not `uuid-valid-stream-p'.~%~@
When DIRECTION is :OUTPUT and stream is not `cl:output-stream-p' an error is signaled.~%~@
When DIRECTION is :INPUT and stream is not `cl:input-stream-p' an error is signaled.~%~@
:SEE-ALSO `uuid-valid-stream-verify-io-type',
`uuid-valid-stream-verify-for-input',
`uuid-valid-stream-verify-for-output'.~%")

(fundoc 'uuid-valid-stream-verify-for-output
"Return STREAM if it is valid for use as when writing a UUID representation.~%~@
Signal an error if STREAM is not true for either `uuid-valid-stream-p' or `cl:outpu-stream-p'.~%~@
:EXAMPLE~%
 \(with-output-to-string \(bubba\)
   \(and \(uuid-valid-stream-verify-for-output bubba\)
        \(princ \"bubba is ok\" bubba\)\)\)~%~@
;; Following fails succesfully:~%
  \(uuid-valid-stream-verify-for-output \"bubba\"\)~%~@
:SEE-ALSO `uuid-valid-stream-verify-io-type', `uuid-valid-stream-verify-for-input'.~%")
;;
(fundoc 'uuid-valid-stream-verify-for-input
"Return STREAM if it is valid for use as when reading a UUID representation.~%~@
Signal an error if STREAM is not true for either `uuid-valid-stream-p' or `cl:input-stream-p'.~%~@
:EXAMPLE~%
 \(let \(\(stream-in   \(make-string-input-stream \"bubba is ok\"\)\)
       \(stream-read \(make-array 11 :element-type 'character\)\)\)
   \(values \(uuid-valid-stream-verify-for-input stream-in\)
           \(progn \(read-sequence stream-read stream-in \)
                  stream-read\)\)\)~%~@
;; Following fails succesfully:~%
 \(uuid-valid-stream-verify-for-input \"bubba\"\)~%~@
:SEE-ALSO `uuid-valid-stream-verify-io-type',
`uuid-valid-stream-verify-for-output'.~%")

(fundoc 'uuid-serialize-byte-array-bytes
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
     \(uuid-serialize-byte-array-bytes  w-uuid s\)\)
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
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-byte-array-16-to-integer
        "Convert a UUID byte-array representaton to its decimal integer representaton.
UUID-BA-16 is an object of type `uuid-byte-array-16'.
Return value is an integer with an upper-bounds of a `uuid-ub128'.~%~@
:EXAMPLE
 \(uuid-bit-vector-to-integer
  \(uuid-to-bit-vector
   \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
 ;=> 317192554773903544674993329975922389959~%
 \(uuid-integer-128-to-byte-array 317192554773903544674993329975922389959\)
 ;=> #\(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199\)~%
 \(uuid-byte-array-16-to-integer
  \(uuid-integer-128-to-byte-array 317192554773903544674993329975922389959\)\)
  ;=> 317192554773903544674993329975922389959~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'uuid-integer-128-to-bit-vector
"Convert the decimal integer representation of a UUID to a `uuid-bit-vector-128'.~%~@
:EXAMPLE~%
 \(uuid-bit-vector-eql \(uuid-integer-128-to-bit-vector 317192554773903544674993329975922389959\)
                     \(uuid-to-bit-vector \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
\(uuid-bit-vector-eql \(uuid-integer-128-to-bit-vector 317192554773903544674993329975922389959\)~%
                     \(uuid-byte-array-to-bit-vector \(uuid-integer-128-to-byte-array 317192554773903544674993329975922389959\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc 'verify-sane-namespace-and-name
"Return as if by cl:values the uuid-byte-array representations of NAMESPACE and NAME.~%~@
NAMESPACE is an object of type `unicly:unique-universal-identifier'.~%~@
NAME is a string.~%~@
Depending on value of special variables:~%
`*uuid-allow-null-like-namespace-args*' `*uuid-allow-empty-string-name-args*'~%~@
Signal an error if NAMESPACE is a null-uuid not satisfying `unique-universal-identifier-null-p'.
Siganl an error if NAME fails to satisfy `unicly::%string-not-empty-p'.
:EXAMPLE~%
 \(verify-sane-namespace-and-name \(make-v4-uuid\) \"bubba\"\)~%
 \(let \(\(*uuid-allow-empty-string-name-args* t\)\)
   \(verify-sane-namespace-and-name \(make-v4-uuid\) \"\"\)\)~%
 \(null \(ignore-errors \(verify-sane-namespace-and-name \(make-v4-uuid\) \"\"\)\)\)~%
 \(let \(\(*uuid-allow-null-like-namespace-args* t\)\)
   \(verify-sane-namespace-and-name \(make-instance 'unique-universal-identifier\) \"bubba\"\)\)~%
 \(let \(\(*uuid-allow-null-like-namespace-args* t\)
       \(*uuid-allow-empty-string-name-args*   t\)\)
   \(verify-sane-namespace-and-name \(make-instance 'unique-universal-identifier\) \"\"\)\)~%
:SEE-ALSO `unicly::%verify-non-empty-name-arg', `unicly::%verify-non-null-namespace-arg'.~%")

(fundoc '%verify-non-empty-name-arg
        "If value of `unicly::*uuid-allow-empty-string-name-args*' is T retrun NAME-ARG.~%~@
If value of NAME-ARG is of type `unicly::string-not-empty' retrun NAME-ARG,
if not and error is signaled.~%~@
:EXAMPLE~%
 \(%verify-non-empty-name-arg \"bubba\"\)
 \(let \(\(*uuid-allow-empty-string-name-args* t\)\)
   \(%verify-non-empty-name-arg \"\"\)\)
 \(null \(ignore-errors \(%verify-non-empty-name-arg \"\"\)\)\)
:SEE-ALSO `unicly::verify-sane-namespace-and-name', `unicly::%verify-non-null-namespace-arg'.~%")

(fundoc '%verify-valid-uuid-subclass-type
 "Return MAYBE-VALID-UUID-SUBCLASS if it is verifiably `cl:subtypep' the class
`unicly:unique-universal-identifier' and not `cl:eq' the symbol
UNIQUE-UNIVERSAL-IDENTIFIER, an error is signaled if not.~%~@
MAYBE-VALID-UUID-SUBCLASS is a symbol designating a subclass.~%~@
:EXAMPLE~%
 \(null \(ignore-errors \(%verify-valid-uuid-subclass-type 'unique-universal-identifier\)\)\)~%
 \(null \(ignore-errors \(%verify-valid-uuid-subclass-type 42\)\)\)~%
 \(null \(ignore-errors \(%verify-valid-uuid-subclass-type 'cl:structure-class\)\)\)~%
:SEE-ALSO `<XREF>'.~%")

(fundoc '%verify-valid-uuid-subclass-slots
        "Instantiate an instance of CLASS-TO-VERIFY as if by `cl:make-instance' and
ensure that each slot of the class 'unicly:unique-universal-identifier is
`cl:slot-exists-p' for the instantiated instance and that their default
`cl:slot-value' is `cl:zerop', if so return CLASS-TO-VERIFY, if not, an error is
signaled.~%~@
:EXAMPLE~%
 \(%verify-valid-uuid-subclass-slots 'unique-universal-identifier\)~%~@
:NOTE The evaluation of `cl:make-instance' should finalize CLASS-TO-VERIFY if it is not already.~%~@
:NOTE This function is evaluated _after_ `unicly::%verify-valid-uuid-subclass'
by `unicly::%verify-valid-subclass-and-slots' and is not evaluated when class
CLASS-TO-VERIFY is not a valid subclass of the class `unicly::unique-universal-identifier'~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%verify-valid-subclass-and-slots
"Return SUBCLASS-TO-VERIFY when SUBCLASS-TO-VERIFY satisfies both:~%
 `unicly::%verify-valid-uuid-subclass-type' `unicly::%verify-valid-uuid-subclass-slots'~%~@
If not, an error is signaled.~%~@
SUBCLASS-TO-VERIFY is a symbol designating a class which subclasses the class `unicly:unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(%verify-valid-subclass-and-slots 'indexable-uuid\) ; assuming it exists~%
 \(null \(ignore-errors \(%verify-valid-subclass-and-slots 'unique-universal-identifier\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%make-uuid-from-string-extended-null-string-error
        "Return MAYBE-VALID-UUID-HEX-STRING-36 if it is not `cl:string=' the constant `unicly::+uuid-null-string+'.~%~@
MAYBE-VALID-UUID-HEX-STRING-36 is a string of type `unicly::uuid-string-36'.~%~@
:EXAMPLE~%
 \(%make-uuid-from-string-extended-null-string-error \"eea1105e-3681-5117-99b6-7b2b5fe1f3c7\"\)~%
 \(%make-uuid-from-string-extended-null-string-error \"not-a-valid-uuid-string-36\"\)~%
 \(null \(ignore-errors \(%make-uuid-from-string-extended-null-string-error \"00000000-0000-0000-0000-000000000000\"\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%make-uuid-from-byte-array-extended-null-array-error
        "Return MAYBE-VALID-UUID-BYTE-ARRAY if it is of type of type
`unicly:uuid-byte-array-16' without without all octets `cl:zerop', if not an
error is signaled.~%~@
:EXAMPLE~%
 \(%make-uuid-from-byte-array-extended-null-array-error \(uuid-to-byte-array \(make-v4-uuid\)\)\)~%
 \(%make-uuid-from-byte-array-extended-null-array-error \(uuid-byte-array-16-zeroed\)\)~%
 \(null 
  \(ignore-errors 
    \(%make-uuid-from-byte-array-extended-null-array-error 
     \(make-array 3 :element-type 'uuid-ub8 :initial-contents #\(255 255 255\)\)\)\)\)~%
 \(null \(ignore-errors \(%make-uuid-from-byte-array-extended-null-array-error \(uuid-byte-array-16-zeroed\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

(fundoc '%make-uuid-from-bit-vector-extendable-bv-zeroed-error
"Return MAYBE-VALID-UUID-BIT-VECTOR or error if it is `unicly::uuid-bit-vector-null-p'.~%~@
:EXAMPLE~%
 \(let \(\(zero-bits \(uuid-bit-vector-128-zeroed\)\)\)
    \(setf \(sbit zero-bits 0\) 1\)
    \(%make-uuid-from-bit-vector-extendable-bv-zeroed-error zero-bits\)\)~%~@
Following each fail succesfully:~%
 \(%make-uuid-from-bit-vector-extendable-bv-zeroed-error \(make-array 129 :element-type 'bit :initial-element 1\)\)~%
 \(%make-uuid-from-bit-vector-extendable-bv-zeroed-error \(make-array 129 :element-type 'bit :initial-element 1\)\)~%
 \(%make-uuid-from-bit-vector-extendable-bv-zeroed-error \(make-array 16 :element-type 'bit\)\)~%
:SEE-ALSO `<XREF>'.~%")

(fundoc 'def-make-uuid-extend-class-fun
"Define functions which provide a the equivalent of the Unicly uuid API for
subclasses of the class `unicly:unique-universal-identifier'.~%~@
MAKE-EXTENDED-SUFFIX is a non-quoted symbol which is appended to the
symbol-name of each defined function.~%~@
EXTENDED-CLASS is a non-quoted which designates a valid subclass of the class
`unicly:unique-universal-identifier'.~%~@
Following table outlines the correspondence of extended functions defined
with their standard Unicly counterparts:~%
 Extended Unicly API function   Standard Unicly API function~%
 make-v3-uuid-<FOO>              `make-v3-uuid'
 make-v4-uuid-<FOO>              `make-v4-uuid'
 make-v5-uuid-<FOO>              `make-v5-uuid'
 make-uuid-from-string-<FOO>     `make-uuid-from-string'
 make-uuid-from-byte-array-<FOO> `uuid-from-byte-array'
 make-uuid-from-bit-vector-<FOO> `uuid-from-bit-vector'~%~@
:SEE-ALSO `<XREF>'.~%")

;;; ==============================

(generic-doc #'uuid-print-bytes
             "Print the bytes of UUID in a format suitable to its class to STREAM.~%~@
UUID an object representing an instance of `unique-universal-identifier' class or subclass.~%~@
STREAM is an output-stream.~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%")

(generic-doc #'uuid-print-bytes-to-string
 "Print the bytes of UUID in a format suitable to its class to a string.~%~@
When keyword STRING-OR-CHAR-TYPE is non-nil specializing methods may provide opmitimizations around this arg.~%~@
:SEE-ALSO `uuid-print-bytes'.~%")

(generic-doc #'uuid-princ-to-string
"Return string representation of UUID-INSTANCE as if by `cl:princ-to-string'.~%~@
:SEE-ALSO `uuid-print-bytes-to-string', `uuid-print-bytes'.~%")

(generic-doc  #'uuid-print-bit-vector
 "Print the bit-vector representation of UUID in a format suitable to its class to STREAM.~%~@
UUID an object representing an instance of `unique-universal-identifier' class or subclass.~%~@
STREAM is an output-stream.~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%")

(generic-doc #'uuid-eql
             "Whether object UUID-A is eql UUID-B.~%~@
:EXAMPLE~%
 \(uuid-eql \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
           \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)~%
 \(uuid-eql \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
           \(make-v5-uuid *uuid-namespace-dns* \"bubbA\"\)\)~%
 \(uuid-eql \"bubb\" \"bobby\"\)~%
 \(uuid-eql *uuid-null-uuid* *uuid-null-uuid*\)~%
 \(uuid-eql *uuid-null-uuid* \(make-null-uuid\)\)~%
 \(uuid-eql \(make-null-uuid\) *uuid-null-uuid*\)~%
 \(uuid-eql \(make-null-uuid\) \(%make-null-uuid-loadtime\)\)~%
 \(uuid-eql \(make-null-uuid\) \(%make-null-uuid-loadtime\)\)~%
 \(uuid-eql \(make-null-uuid\) \(make-instance 'unique-universal-identifier-null\)\)~%
 \(uuid-eql \(make-instance 'unique-universal-identifier-null\) \(make-instance 'unique-universal-identifier-null\)\)~%
 \(uuid-eql \(make-instance 'unique-universal-identifier-null\) \(make-null-uuid\)\)~%
 \(uuid-eql \(make-instance 'unique-universal-identifier\) \(make-null-uuid\)\)~%~@
:NOTE When either UUID-A or UUID-B is an instance of the class
`unicly::unique-universal-identifier-null' they are only considered to be
`unicly:uuid-eql' if both objects are `cl:eql' the value of special variable
`unicly::*uuid-null-uuid*'.~%~@
:SEE-ALSO `unique-universal-identifier-p'.~%")

(generic-doc #'unique-universal-identifier-p
             "Whether MAYBE-UUID-INSTANCE is a unique-universal-identifier.~%~@
Return T if argument is a class instance of `unique-universal-identifier' or
one of its subclasses.~%~@
:EXAMPLE~%
 \(unique-universal-identifier-p *uuid-namespace-dns*\)~%
 \(unique-universal-identifier-p \(uuid-to-bit-vector *uuid-namespace-dns*\)\)~%
 \(unique-universal-identifier-p t\)~%~@
:SEE-ALSO `uuid-eql', `unicly::unique-universal-identifier-null-p'.~%")

(method-doc #'uuid-print-bytes nil '(t unique-universal-identifier)
"Print the raw bytes of UUID in hexadecimal format to STREAM.~%~@
UUID is an instance of `unique-universal-identifier' class.~%~@
STREAM is an output-stream.~%~@
Output of return value has the format:~%
 6ba7b8109dad11d180b400c04fd430c8~%~@
:EXAMPLE~%
 \(uuid-print-bytes nil \(make-v4-uuid\)\)~%
 \(uuid-print-bytes t \(make-v4-uuid\)\)~%~@
:NOTE Per RFC4122 Section 3. \"Namespace Registration Template\" 
 ,----
 | The hexadecimal values \"a\" through \"f\" are output as
 | lower case characters and are case insensitive on input.
 `----~%~@
:SEE-ALSO `uuid-print-bytes-to-string'.~%")

(method-doc #'uuid-print-bytes-to-string nil '(unique-universal-identifier)
            "Print bytes of UUID in hexadecimal representation to a `uuid-string-32'.~%~@
Keyword STRING-OR-CHAR-TYPE is non-nil it is a string or the symbol 'base-char or 'character.
When it is `cl:stringp' and satisfies `string-with-fill-pointer-p'
print hexadecimal representation of bytes to STRING.
Default method speciaclized on instances of class `unique-universal-identifier'.
:EXAMPLE~%
 \(uuid-print-bytes-to-string \(make-v4-uuid\)\)~%
 \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\) :upcase t\)
 \(let \(\(fp-string \(make-array 11 
                              :element-type 'base-char 
                              :fill-pointer 11 
                              :initial-contents \"UUID-BYTES:\"\)\)\)
   \(format fp-string \"~~%\"\)
   \(uuid-print-bytes-to-string \(make-v4-uuid\) fp-string\)\)~%
 \(type-of
  \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                              :string-or-char-type \(make-array 32 :element-type 'base-char :fill-pointer 0\)
                              :upcase t\)\)
 ;=> \(SIMPLE-BASE-STRING 32\)~%
 \(type-of
  \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                              :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)
                              :upcase t\)\)
 ;=> \(SIMPLE-ARRAY CHARACTER \(32\)\)~%
 \(type-of
  \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                              :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)\)\)
 ;=> \(AND \(VECTOR CHARACTER 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of
  \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                              :string-or-char-type \(make-array 32 :element-type 'base-char :fill-pointer 0\)\)\)
 ;=> \(AND \(BASE-STRING 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                                      :string-or-char-type 'character 
                                      :upcase t\)\)
 ;=> \(SIMPLE-ARRAY CHARACTER \(32\)\)~%
 \(type-of \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\) 
                                      :string-or-char-type 'base-char
                                      :upcase t\)\)
 ;=> \(SIMPLE-BASE-STRING 32\)~%
 \(type-of \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                                      :string-or-char-type 'character \)\)
 ;=> \(AND \(VECTOR CHARACTER 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)
                                      :string-or-char-type 'base-char \)\)
 ;=> \(AND \(BASE-STRING 32\) \(NOT SIMPLE-ARRAY\)\)~%~@
:NOTE Per RFC4122 Section 3. \"Namespace Registration Template\" 
 ,----
 | The hexadecimal values \"a\" through \"f\" are output as
 | lower case characters and are case insensitive on input.
 `----~%~@
:SEE-ALSO `uuid-print-bytes', `uuid-print-bit-vector', `uuid-princ-to-string'.~%")

(method-doc #'uuid-print-bytes-to-string nil '(vector)
          "Print the byte-array representation of UUID in a format suitable to its class to STREAM.~%~@
UUID is an object of type `uuid-byte-array-16' representang of an instance of
`unique-universal-identifier' class or subclass.
Keyword STRING-OR-CHAR-TYPE is non-nil it is a string or the symbol 'base-char or 'character.~%~@
When it is `cl:stringp' and satisfies `string-with-fill-pointer-p' print the
UUID's hexadecimal byte representation to STRING.~%~@
:EXAMPLE~%~@
 \(uuid-print-bytes-to-string 
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)
 ;=> \"eea1105e3681511799b67b2b5fe1f3c7\"~%
 \(uuid-print-bytes-to-string 
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
  :upcase t\)
 ;=> \"EEA1105E3681511799B67B2B5FE1F3C7\"~%
 \(uuid-print-bytes-to-string   
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
  :string-or-char-type \(make-array 32 :element-type 'base-char :fill-pointer 0\)\)
 ;=> \"eea1105e3681511799b67b2b5fe1f3c7\"~%
 \(uuid-print-bytes-to-string   
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
  :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)\)
 ;=> \"eea1105e3681511799b67b2b5fe1f3c7\"~%
 \(uuid-print-bytes-to-string   
  \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
  :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)
  :upcase t\)
 ;=> \"EEA1105E3681511799B67B2B5FE1F3C7\"~%
 \(type-of \(uuid-print-bytes-to-string   
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)
           :upcase t\)\)
 ;=> \(SIMPLE-ARRAY CHARACTER \(32\)\)~%
 \(type-of \(uuid-print-bytes-to-string   
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type \(make-array 32 :element-type 'base-char :fill-pointer 0\)
           :upcase t\)\)
 ;=> \(SIMPLE-BASE-STRING 32\)~%
 \(type-of \(uuid-print-bytes-to-string   
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type \(make-array 32 :element-type 'character :fill-pointer 0\)\)\)
 ;=> \(AND \(VECTOR CHARACTER 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string   
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type \(make-array 32 :element-type 'base-char :fill-pointer 0\)\)\)
 ;=> \(AND \(BASE-STRING 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string 
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)\)\)
 ;=> \(AND \(BASE-STRING 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string 
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type 'character\)\)
 ;=> \(AND \(VECTOR CHARACTER 32\) \(NOT SIMPLE-ARRAY\)\)~%
 \(type-of \(uuid-print-bytes-to-string 
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type 'character
           :upcase t\)\)
 ;=> \(SIMPLE-ARRAY CHARACTER \(32\)\)~%
 \(type-of \(uuid-print-bytes-to-string 
           \(uuid-to-byte-array \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\)\)
           :string-or-char-type 'base-char
           :upcase t\)\)
 ;=> \(SIMPLE-BASE-STRING 32\)~%
:SEE-ALSO `<XREF>'.~%")

(method-doc #'uuid-print-bit-vector nil #+:sbcl '(simple-bit-vector) #-:sbcl '(bit-vector t)
"Print the bit-vector reprsentation of UUID to STREAM.~%~@
UUID should be an object of type `uuid-bit-vector-128', an error is signaled if not.~%~@
:EXAMPLE~%
 \(uuid-print-bit-vector nil (uuid-bit-vector-128-zeroed))~%
 \(find-method #'uuid-print-bit-vector nil '\(t simple-bit-vector\)\)~%~@
:SEE-ALSO `uuid-to-bit-vector'.~%")

(method-doc #'uuid-print-bit-vector nil '(unique-universal-identifier)
"Print the `uuid-bit-vector-128' representation UUID to STREAM.~%~@
UUID is an instance of class `unique-universal-identifier'.~%~@
:EXAMPLE~%
 \(uuid-print-bit-vector \(make-v4-uuid\) nil\)~%
 \(find-method #'uuid-print-bit-vector nil '\(t unique-universal-identifier\)\)~%~@
:SEE-ALSO `uuid-princ-to-string'.~%")

(method-doc #'uuid-print-bit-vector nil '(unique-universal-identifier-null)
 "If UUID is `unique-universal-identifier-null-p' print it's bit-vector representation.~%~@
If not, an error is signaled.~%~@
:EXAMPLE~%
 \(uuid-print-bit-vector \(make-null-uuid\) nil\)~%~@
;; Following fails successfully:~%
 \(uuid-print-bit-vector \(make-instance 'unique-universal-identifier-null\) nil\)~%
 \(find-method \(#'uuid-print-bit-vector\) nil '\(unique-universal-identifier-null\)\)~%~@
:NOTE Only the null-UUID created as if by `make-null-uuid' may have its
uuid-bit-vector-128 representation printed with this method.
IOW, The results are unspecified if and attempt is made to to create null uuid objects as if by:~%
 \(make-instance 'unique-universal-identifier-null\)~%~@
:SEE-ALSO `<XREF>'.~%")

(method-doc #'cl:print-object  nil '(unique-universal-identifier t)
            "Print instance of class `unicly:unique-universal-identifier' to STREAM in string representation.~%
:EXAMPLE~%
 \(print-object \(make-v3-uuid *uuid-namespace-dns* \"bubba\"\) nil\)~%
 \(print-object \(make-v5-uuid *uuid-namespace-dns* \"bubba\"\) nil\)~%
 \(print-object \(make-v4-uuid\) nil\)~%
 \(print-object \(make-null-uuid\) nil\)~%~@
:SEE-ALSO `unicly:uuid-print-bytes', `unicly:uuid-princ-to-string',
`unicly:uuid-print-bit-vector', `unicly:uuid-print-bytes-to-string'.~%")


;;; ==============================
;;; :DEPRECATED-DOCS
;;; ==============================

#+(or)
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
:SEE-ALSO `<XREF>'.~%")

#+(or)
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
:SEE-ALSO `<XREF>'.~%")

#+(or)
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
:SEE-ALSO `<XREF>'.~%")

#+(or)
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
:SEE-ALSO `<XREF>'.~%")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
