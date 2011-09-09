;;; :FILE-CREATED <Timestamp: #{2011-08-17T15:48:25-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-hash-table.lisp
;;; ==============================


(in-package #:unicly)

;; :NOTE Keep the sxhash/hash-table stuff here or in a file which comes after unicly-class.lisp 
;; otherwise the compiler complains about open coding
(defun sxhash-uuid (uuid)
  (declare (unique-universal-identifier uuid)
           (optimize (speed 3)))
  (sxhash (the uuid-bit-vector-128 (uuid-to-bit-vector uuid))))

#+sbcl 
(sb-ext:define-hash-table-test uuid-eql sxhash-uuid)

#+clisp
(ext:define-hash-table-test uuid-eql uuid-eql sxhash-uuid)

#+clisp
(defun make-hash-table-uuid (&key synchronized) 
  (declare (ignore synchronized))
  (make-hash-table :test 'uuid-eql))

#+sbcl
(defun make-hash-table-uuid (&key synchronized) ;; &allow-other-keys ??
  (make-hash-table :test 'uuid-eql :synchronized synchronized))




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
