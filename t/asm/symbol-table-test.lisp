(defpackage symbol-table-test
  (:export #:run-tests)
  (:use #:cl #:fiveam #:symbol-table))

(in-package #:symbol-table-test)

(def-suite symbol-table-suite
  :description "Test suite for the asm systems SYMBOL-TABLE package.")

(in-suite symbol-table-suite)

(defun run-tests ()
  (run! 'symbol-table-suite))

(defmacro test-wfst (test-name &body body)
  "Same as FIVEAM:TEST but clear the symbol table before running the test."
  `(fiveam:test ,test-name (progn (symbol-table :clear-table) ,@body)))

;;; Tests

(test-wfst entry-p-and-insert  
  (is-true (progn
             (symbol-table :insert "FOO" :UNDEF 0)
             (symbol-table :entry-p "FOO"))))

(test-wfst entry-p-false
  (is-false (symbol-table :entry-p "FOO")))

(test-wfst insert-signals-duplicate-symbol
  (signals duplicate-symbol
    (progn
      (symbol-table :insert "FOO" :UNDEF 0)
      (symbol-table :insert "FOO" :UNDEF 1000))))

(test-wfst symbol-value
  (is-true (progn
             (symbol-table :insert "FOO" :UNDEF 12)
             (= 12 (symbol-table :symbol-value "FOO")))))

(test-wfst symbol-value-signals-undefined-symbol
  (signals undefined-symbol (symbol-table :symbol-value "FOO")))

(test-wfst symbol-type
  (is-true (progn
             (symbol-table :insert "FOO" :UNDEF 0)
             (eql :UNDEF (symbol-table :symbol-type "FOO")))))

(test-wfst symbol-type-signals-undefined-symbol
  (signals undefined-symbol (symbol-table :symbol-type "FOO")))
