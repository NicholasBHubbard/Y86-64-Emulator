(defpackage symbol-table-test
  (:export #:run-tests)
  (:use #:cl #:fiveam)
  (:import-from #:symbol-table #:*symbol-table*))

(in-package #:symbol-table-test)

(def-suite symbol-table-suite
  :description "Test suite for the asm systems SYMBOL-TABLE package.")

(in-suite symbol-table-suite)

(defun run-tests ()
  (run! 'symbol-table-suite))

(defmacro test-wfst (test-name &body body)
  "Same as fiveam:test but automatically clear the symbol table after running
the test."
  `(progn
     (fiveam:test ,test-name ,@body)
     (symbol-table :clear-table)))

;;; Tests

(test-wfst entry-p-and-insert  
  (is-true (progn
             (symbol-table :insert "FOO" :U 0)
             (symbol-table :entry-p "FOO"))))

(test-wfst entry-p-false
  (is-false (symbol-table :entry-p "FOO")))

(test-wfst insert-signals-duplicate-symbol
  (signals symbol-table:duplicate-symbol
    (progn
      (symbol-table :insert "FOO" :U 0)
      (symbol-table :insert "FOO" :U 1000))))

(test-wfst symbol-value
  (is-true (progn
             (symbol-table :insert "FOO" :U 12)
             (= 12 (symbol-table :symbol-value "FOO")))))

(test-wfst symbol-value-signals-undefined-symbol
  (signals symbol-table:undefined-symbol (symbol-table :symbol-value "FOO")))

(test-wfst symbol-type
  (is-true (progn
             (symbol-table :insert "FOO" :U 0)
             (eql :U (symbol-table :symbol-type "FOO")))))

(test-wfst symbol-type-signals-undefined-symbol
  (signals symbol-table:undefined-symbol (symbol-table :symbol-type "FOO")))
