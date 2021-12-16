;;;; Test the Y8664-register-table package. This package exports one function
;;;; which is REGISTER-TABLE. This function returns a closure that closes over
;;;; Y86-64 Register Table, and provides a dispatch function for mutating and
;;;; querying the Register Table.

(defpackage Y8664-register-table-test
  (:nicknames #:regt-t)
  (:use #:cl #:fiveam)
  (:import-from #:alexandria
                #:compose
                #:curry))

(in-package #:Y8664-register-table-test)

(def-suite Y8664-register-table-suite
  :description "Test suite for the Y8664-opcode package")

(in-suite Y8664-register-table-suite)

(defun run-tests ()
  (run! 'Y8664-register-table-suite))

;;; *register-table* is a closure over the static Y86-64 Register Table. This
;;; special variable is the entire point of the Y8664-REGISTER-TABLE package.

(defparameter *register-table* (regt:register-table))

;;; Define special variables to help avoid writing boilerplate code in the
;;; actual tests.

(defparameter *ids* (list #x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xA #xB #xC #xD #xE #xF))

(defparameter *id-strings* (mapcar (curry #'format nil "~x") *ids*))

(defparameter *register-names* (list :RAX :RCX :RDX :RBX :RSP :RBP :RSI :RDI :R8 :R9 :R10 :R11 :R12 :R13 :R14 :NOREG))

(defparameter *register-name-strings* (mapcar #'symbol-name *register-names*))

;;; Tests

(test pass-input-as-string
  (is-true (and (funcall *register-table* :register-name-p :RAX)
                (funcall *register-table* :register-name-p "RAX"))))
(test id-p-trues
  (is-true (every (curry #'funcall *register-table* :id-p) *ids*)))

(test id-p-falses
  (is-true (notany (curry #'funcall *register-table* :id-p)
                   (list -1 256))))

(test all-ids
  (is-true (equal (sort (funcall *register-table* :all-ids) #'<)
                  (sort (copy-seq *ids*) #'<))))

(test all-register-names
  (is-true (equal (sort (funcall *register-table* :all-register-names) #'string<)
                  (sort (copy-seq *register-names*) #'string<))))

(test all-id-strings
  (is-true (equal (sort (funcall *register-table* :all-id-strings) #'string<)
                  (sort (copy-seq *id-strings*) #'string<))))

(test all-register-name-strings
  (is-true
   (equal (sort (funcall *register-table* :all-register-name-strings) #'string<)
          (sort (copy-seq *register-name-strings*) #'string<))))

(test id-register-name
  (is-true (eql :RSI (funcall *register-table* :id-register-name #x6))))

(test register-name-id
  (is-true (eql #x6 (funcall *register-table* :register-name-id :RSI))))

(test id-register-name-string
  (is-true
   (string= "R12" (funcall *register-table* :id-register-name-string #xC))))

(test register-name-id-string
  (is-true
   (string= "F" (funcall *register-table* :register-name-id-string :NOREG))))

(test id-register-name-match-p-true
  (is-true
   (funcall *register-table* :id-register-name-match-p #x2 :RDX)))

(test id-register-name-match-p-false
  (is-false
   (funcall *register-table* :id-register-name-match-p #x3 :RDX)))
