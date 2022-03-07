(defpackage register-table-test
  (:export #:run-tests)
  (:use #:cl #:fiveam)
  (:import-from #:register-table #:*register-table*)
  (:import-from #:alexandria
                #:compose
                #:curry))

(in-package #:register-table-test)

(def-suite register-table-suite
  :description "Test suite for the asm systems REGISTER-TABLE package.")

(in-suite register-table-suite)

(defun run-tests ()
  (run! 'register-table-suite))

;;; Define special variables to help avoid writing boilerplate code in the
;;; actual tests.

(defparameter *ids* (list #x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xA #xB #xC #xD #xE #xF))

(defparameter *id-strings* (mapcar (curry #'format nil "~x") *ids*))

(defparameter *register-names* (list :RAX :RCX :RDX :RBX :RSP :RBP :RSI :RDI :R8 :R9 :R10 :R11 :R12 :R13 :R14 :NOREG))

(defparameter *register-name-strings* (mapcar #'symbol-name *register-names*))

;;; Tests

(test pass-input-as-string
  (is-true (and (register-table :register-name-p :RAX)
                (register-table :register-name-p "RAX"))))
(test id-p-trues
  (is-true (every (curry #'register-table :id-p) *ids*)))

(test id-p-falses
  (is-true (notany (curry #'register-table :id-p)
                   (list -1 256))))

(test all-ids
  (is-true (equal (register-table :all-ids)
                  (sort (copy-seq *ids*) #'<))))

(test all-register-names
  (is-true (equal (register-table :all-register-names)
                  (sort (copy-seq *register-names*) #'string<))))

(test all-id-strings
  (is-true (equal (register-table :all-id-strings)
                  (sort (copy-seq *id-strings*) #'string<))))

(test all-register-name-strings
  (is-true
   (equal (register-table :all-register-name-strings)
          (sort (copy-seq *register-name-strings*) #'string<))))

(test id-register-name
  (is-true (eql :RSI (register-table :id-register-name #x6))))

(test register-name-id
  (is-true (eql #x6 (register-table :register-name-id :RSI))))

(test id-register-name-string
  (is-true
   (string= "R12" (register-table :id-register-name-string #xC))))

(test register-name-id-string
  (is-true
   (string= "F" (register-table :register-name-id-string :NOREG))))

(test id-register-name-match-p-true
  (is-true
   (register-table :id-register-name-match-p #x2 :RDX)))

(test id-register-name-match-p-false
  (is-false
   (register-table :id-register-name-match-p #x3 :RDX)))
