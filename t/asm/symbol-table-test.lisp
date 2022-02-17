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
  "Same as fiveam:test but make available a fresh lexical symbol-table named 
   SYMBOL-TABLE. The acronym wfst stands for with-fresh-symbol-table."
  `(fiveam:test ,test-name
                (let ((symbol-table (symbol-table::init-symbol-table)))
                  ,@body)))

;;; Tests

(test-wfst entry-p-and-insert  
  (is-true (progn
             (funcall symbol-table :insert "FOO" :U 0)
             (funcall symbol-table :entry-p "FOO"))))

(test-wfst entry-p-false
  (is-false (funcall symbol-table :entry-p "FOO")))

(test-wfst insert-signals-duplicate-symbol
  (signals symbol-table:duplicate-symbol
    (progn
      (funcall symbol-table :insert "FOO" :U 0)
      (funcall symbol-table :insert "FOO" :U 1000))))

(test-wfst symbol-value
  (is-true (progn
             (funcall symbol-table :insert "FOO" :U 12)
             (= 12 (funcall symbol-table :symbol-value "FOO")))))

(test-wfst symbol-value-signals-undefined-symbol
  (signals symbol-table:undefined-symbol (funcall symbol-table :symbol-value "FOO")))

(test-wfst symbol-type
  (is-true (progn
             (funcall symbol-table :insert "FOO" :U 0)
             (eql :U (funcall symbol-table :symbol-type "FOO")))))

(test-wfst symbol-type-signals-undefined-symbol
  (signals symbol-table:undefined-symbol (funcall symbol-table :symbol-type "FOO")))
