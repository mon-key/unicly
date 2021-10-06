;;; :FILE-CREATED <Timestamp: #{2011-08-17T15:48:25-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-hash-table.lisp
;;; ==============================

;; :NOTE Keep the sxhash/hash-table stuff here or in a file which comes after
;; unicly-class.lisp otherwise the compiler complains about open coding

;; For good discussion on CL implementations of hash-tables and the underlying rational for some of the ANSI "API"
;; inncludes low spam-count discussion and input from: Duane Rettig, Erik
;; Naggum, Dan Barlow, Frode Vatvedt Fjeld, Peter Seibel, Paul F. Dietz etc.
;; :SEE comp.lang.lisp thread titled "(make-hash-table :test #'mytest)

;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/22095b402fc80c20?dmode=source')


(in-package #:unicly)


#+:sbcl
(defconstant +%%uuid-sxhash-truncating-node%%+
  (and (<= sb-vm:n-positive-fixnum-bits 48)
       sb-vm:n-positive-fixnum-bits))

;; on SBCL x86-32
;; 1mil invocations of sxhash-uuid for array of 1mil v4uuids
;;  0.212967 seconds of total run time (0.211968 user, 0.000999 system)
;; 1mil invocations of cl:sxhash with same array of 1mil v4uuids
;;  0.018997 seconds of total run time (0.018997 user, 0.000000 system)
(defun sxhash-uuid (uuid)
  (declare (unique-universal-identifier uuid)
           (optimize (speed 3)))
  ;; :NOTE The used to be:
  ;;   (sxhash (the uuid-bit-vector-128 (uuid-to-bit-vector uuid)))
  ;; On SBCL we can do better than that by verifying that a slot value is not zerop
  ;; (if it is we don't want to use that as our hash)
  ;; and knocking down the 48 bits to (unsigned-byte 29) on SBCL-x8632
  #-sbcl (sxhash uuid)
  ;; We don't bother checking slot-boundp of the UUID arg on the assumption that
  ;; only a madman would slot-makunbound %uuid_node
  #+sbcl (let ((node-int (slot-value uuid '%uuid_node)))
           (declare (type uuid-ub48 node-int))
           (if (zerop node-int)
               (sxhash uuid)
               (if +%%uuid-sxhash-truncating-node%%+
                   ;; (mask-field (byte (the (mod 49) +%%uuid-sxhash-truncating-node%%+) 0) node-int)
                   (logand node-int
                           (dpb -1 (byte (the (mod 49) +%%uuid-sxhash-truncating-node%%+) 0) 0))
                   node-int))))

#+:sbcl
(sb-ext:define-hash-table-test uuid-eql sxhash-uuid)

#+:clisp
(ext:define-hash-table-test uuid-eql uuid-eql sxhash-uuid)

;; CLisp hash-tables
;; (&key size rehash-size rehash-threshold initial-contents key-type value-type
;;       warn-if-needs-rehash-after-gc weak test)
;;
;; :TODO Figure out if our :test function `unicly:uuid-eql' can leverage
;; `ext:fasthash-<FOO>' or `ext:stablehash-<FOO>' particularly the `ext:stablehash-<FOO>'s.
;;
;;  :NOTE AFAICT default values for `custom:*eq-hashfunction*',
;;  `custom:*eql-hashfunction*', `custom:*equal-hashfunction*', default to
;;  `ext:fasthash-eq', `ext:fasthash-eql', `ext:fasthash-equal'
;;
#+:clisp
(defun make-hash-table-uuid (&key size rehash-size rehash-threshold initial-contents weakness synchronized)
  ;; Keyword INITIAL-CONTENTS when non-nil is a _proper_ alist (e.g. a list of
  ;; cons cells) used to populat the key/value pairs of returned hash-table.
  ;; Keyword WEAKNESS is one of:
  ;;  nil :key :value :key-and-value :key-or-value
  ;;  nil           -- the hash-table is not contained of weak relations
  ;; :key           -- an `ext:weak-mapping' from hash-key to hash-value
  ;; :value         -- an `ext:weak-mapping' from hash-value to hash-key
  ;; :key-and-value -- an `ext:weak-and-relation' of the key and the value,
  ;; :key-or-value  -- an `ext:weak-or-relation' of the key and the value.
  ;; :SEE (URL `http://clisp.cons.org/impnotes/make-hash.html')
  ;; :SEE (URL `http://www.clisp.org/impnotes/weak.html#weak-ht')
  ;; :SEE-ALSO `ext:standard-stablehash', `ext:structure-stablehash',
  ;; `custom:*warn-on-hashtable-needing-rehash-after-gc*', `ext:hash-table-weak-p'.
  (declare (ignore synchronized)
           (type (member nil :key :value :key-and-value :key-or-value) weak)) ;; &allow-other-keys ??
  (make-hash-table :size size
                   :rehash-size rehash-size
                   :rehash-threshold rehash-threshold
                   :test 'uuid-eql
                   :weak weakness
                   ;; Clisp specific but see the equivalent alist-hash-fu in Alexandria 
                   ;; or`sb-impl::%stuff-hash-table' in sbcl/src/code/target-hash-table.lisp 
                   :initial-contents initial-contents))

;; LispWorks hash-tables:
;; &key test size rehash-size rehash-threshold hash-function weak-kind single-thread free-function
;;
;; :NOTE regarding use of :TEST argument to LW's `make-hash-table'.
;; Paraphrasing LW manual:
;; ,----
;; | The standard definition of `cl:make-hash-table' is extended such that test can be any
;; | suitable user-defined function, except that it must not call process-wait or
;; | similar MP package functions which suspend the current process. If test is not
;; | one of the standard test functions:
;; |  `cl:eq', `cl:eql' `cl:equal' `cl:equalp')
;; | and if value of keyword HASH-FUNCTION is not supplied, then the hash value
;; | is the same as would be used if :TEST were `cl:equalp'.
;; `---- :SOURCE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-608.htm#47359')
;;
;; Keyword SYNCHRONIZED is a boolean. When t this indicates that
;; hash-table will _only_ be used in a single-threaded context (e.g. it needn't
;; be thread-safe) and/or it is known that the hash-table will never be accessed
;; outside the scop of a lock. Default is nil
;;
;; Keyword WEAKNESS is one one of:
;;  t, nil :value :key :one :both :either
;;
;;  NIL            -- Make hash-table non-weak. All entries are kept.
;;  T or :VALUE    -- Entires persist when a pointer to hash-value from another object exists.
;; :KEY            -- Entries persist when a pointer to hash-key from another object exists.
;; :BOTH           -- Entries persist when pointers to both hash-key and hash-value exist.
;; :ONE or :EITHER -- Entries persist when a pointer to either hash-key or hash-value exist.
;;
;; Keyword FREE-FUNCTION is a function-designator for a function accepting two
;; arguments (a hash-key and hash-value).
;; :SEE (URL `http://www.lispworks.com/documentation/lw60/LW/html/lw-608.htm#47359')
;; :SEE-ALSO `lw:choose-unicode-string-hash-function',
;; `hcl:set-hash-table-weak'.  `hcl:modify-hash', `hcl:with-hash-table-locked'.
#+:lispworks
(defun make-hash-table-uuid (&key size rehash-size rehash-threshold
                             (weakness nil)
                             (synchronized nil)
                             free-function)  ;; &allow-other-keys ??
  (declare (type (or boolean (member :value :key :one :both :either)) weakness)
           (type boolean synchronized))
  (make-hash-table :size             size
                   :rehash-size      rehash-size
                   :rehash-threshold rehash-threshold
                   :test             'uuid-eql
                   :hash-function    'sxhash-uuid
                   :weak-kind        weakness
                   :single-thread    synchronized
                   :free-function    free-function))


;; Keyword SIZE defaults to value of `SB-IMPL::+MIN-HASH-TABLE-SIZE+'.
;; Where the SIZE of the generated table is know It is likely that specifying a
;; value will enable the system to not require as much rehashing.
;; UPDATE: referencing SB-IMPL::+MIN-HASH-TABLE-SIZE+ causes an error on build
;; and was removed.
;; REHASH-SIZE defaults to 1.5. For large tables setting this value to
#+:sbcl
(defun make-hash-table-uuid (&key
                             size
                             (rehash-size 1.5)
                             (rehash-threshold 1)
                             (weakness nil)
                             (synchronized nil)) ;; &allow-other-keys ??
  (declare
   (type unsigned-byte size)
   (type (or (integer 1) (single-float (1.0)) (double-float (1.0d0))) rehash-size)
   (type (or (single-float 0.0 1.0) (double-float 0.0d0 1.0d0) (rational 0 1)) rehash-threshold)
   (type (member nil :key :value :key-and-value :key-or-value) weakness)
   ;; (type (or function symbol) hash-function) ;; not needed given that is already defined sxhash-uuid.
   (type boolean synchronized))
  (make-hash-table :size             size
                   :rehash-size      rehash-size
                   :rehash-threshold rehash-threshold
                   :weakness         weakness
                   :test             'uuid-eql
                   :synchronized     synchronized))

;; (mon:where-is "+MIN-HASH-TABLE-SIZE+")
;;
;; (defparameter *tt--uuid-ht* (make-hash-table-uuid))
;; (setf (gethash (make-v5-uuid *uuid-namespace-dns* "bubba") *tt--uuid-ht* )
;;       "bubba")



;;; ==============================
;;; :NOTES Regarding functions/idioms to incorporate from vivace-graph-v2
;;; :SEE https://github.com/kraison/vivace-graph-v2
;;; ==============================
;; :NOTE Given that the bit-vector representation is guaranteed to be a
;;  `uuid-bit-vector-128' we should be able to just bit dwiddle are way from
;;  index 0 to 127 until we find the "most significiant bit" at which point we
;;  have have the rudiments of less-than/greater-than without the hassle of
;;  string comparisons and with the benefit of integer/numeric sorts...
;;
;; :FILE vivace-graph-v2-GIT/utilities.lisp
;; (defgeneric less-than (x y)
;;   (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
;;   (:method ((x symbol) (y uuid:uuid))
;;     (string< (symbol-name x) (uuid:print-bytes nil y)))
;;   (:method ((x number) (y uuid:uuid))
;;     (string< (write-to-string x) (uuid:print-bytes nil y)))
;;   (:method ((x string) (y uuid:uuid))
;;     (string< x (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y uuid:uuid))
;;     (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y string)) (string< (uuid:print-bytes nil x) y))
;;   (:method ((x uuid:uuid) (y symbol))
;;     (string< (uuid:print-bytes nil x) (symbol-name y)))
;;   (:method ((x uuid:uuid) (y number))
;;     (string< (uuid:print-bytes nil x) (write-to-string y))))
;;
;; (defgeneric greater-than (x y)
;;   (:documentation "Generic greater-than operator.  Allows comparison of apples and oranges.")
;;   (:method ((x symbol) (y uuid:uuid)) (string> (symbol-name x) (uuid:print-bytes nil y)))
;;   (:method ((x number) (y uuid:uuid)) (string> (write-to-string x) (uuid:print-bytes nil y)))
;;   (:method ((x string) (y uuid:uuid)) (string> x (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y uuid:uuid)) (string> (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
;;   (:method ((x uuid:uuid) (y string)) (string> (uuid:print-bytes nil x) y))
;;   (:method ((x uuid:uuid) (y symbol)) (string> (uuid:print-bytes nil x) (symbol-name y)))
;;   (:method ((x uuid:uuid) (y number)) (string> (uuid:print-bytes nil x) (write-to-string y))))
;;; ==============================

;;; ==============================
;; vivace-graph-v2's use of `make-hash-table-uuid'
;; :FILE vivace-graph-v2-GIT/store.lisp
;; (defun make-fresh-store (name location &key (num-locks 10000))
;;   (let ((store
;; 	 (make-instance 'local-triple-store
;;  			:name name
;; 			:location location
;; 			:main-idx (make-hierarchical-index)
;; 			:lock-pool (make-lock-pool num-locks)
;; 			:locks (make-hash-table :synchronized t :test 'equal)
;; 			:text-idx (make-skip-list :key-equal 'equalp
;; 						  :value-equal 'uuid:uuid-eql
;; 						  :duplicates-allowed? t)
;; 			:log-mailbox (sb-concurrency:make-mailbox)
;; 			:index-queue (sb-concurrency:make-queue)
;; 			:delete-queue (sb-concurrency:make-queue)
;; 			:templates (make-hash-table :synchronized t :test 'eql)
;; 			:indexed-predicates (make-hash-table :synchronized t
;; 							     :test 'eql))))
;;     (add-to-index (main-idx store) (make-uuid-table :synchronized t) :id-idx)
;;     (setf (logger-thread store) (start-logger store))
;;     store))
;;
;; :FILE vivace-graph-v2-GIT/transaction.lisp
;; (defstruct (transaction
;; 	     (:print-function print-transaction)
;; 	     (:conc-name tx-)
;; 	     (:predicate transaction?))
;;   (id (make-uuid))
;;   (queue nil)
;;   (rollback nil)
;;   (mailbox (sb-concurrency:make-mailbox))
;;   (thread (current-thread))
;;   (store nil)
;;   (locks nil))
;;
;; :FILE vivace-graph-v2-GIT/triples.lisp
;; (defgeneric triple-equal (t1 t2)
;;   (:method ((t1 triple) (t2 triple)) (uuid:uuid-eql (id t1) (id t2)))
;;   (:method (t1 t2) nil))
;;
;; (defun make-anonymous-node ()
;;   "Create a unique anonymous node."
;;   (format nil "_anon:~A" (make-uuid)))
;;
;; (let ((regex
;;        "^_anon\:[0-9abcdefABCEDF]{8}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{4}\-[0-9abcdefABCEDF]{12}$"))
;;   (defun anonymous? (node)
;;     (when (stringp node)
;;       (cl-ppcre:scan regex node))))
;;
;;; ==============================


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
