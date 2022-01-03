(defpackage symbol-table-test
  (:export #:run-tests)
  (:nicknames #:symt-t)
  (:use #:cl #:fiveam))

(in-package #:symbol-table-test)

(def-suite symbol-table-suite
  :description "Test suite for the SYMBOL-TABLE package")

(in-suite symbol-table-suite)

(defun run-tests ()
  (run! 'symbol-table-suite))

(defparameter *symbol-table* (symt:symbol-table))

(defmacro test-wfst (test-name &body body)
  "wfst = with-fresh-symbol-table. Same as fiveam:test but make available a 
   fresh lexical symbol-table named SYMBOL-TABLE."
  `(fiveam:test ,test-name
     (let ((symbol-table (symt:symbol-table)))
       ,@body)))

;;; Tests

(test-wfst entry-p-and-insert  
  (is-true (progn
             (funcall symbol-table :insert ":FOO" :U 0)
             (funcall symbol-table :entry-p ":FOO"))))

(test-wfst entry-p-false
  (is-false (funcall symbol-table :entry-p ":FOO")))

(test-wfst insert-signals-duplicate-symbol
  (signals symt:duplicate-symbol
    (progn
      (funcall symbol-table :insert ":FOO" :U 0)
      (funcall symbol-table :insert ":FOO" :U 1000))))

(test-wfst symbol-value
  (is-true (progn
             (funcall symbol-table :insert ":FOO" :U 12)
             (= 12 (funcall symbol-table :symbol-value ":FOO")))))

(test-wfst symbol-value-signals-undefined-symbol
  (signals symt:undefined-symbol (funcall symbol-table :symbol-value ":FOO")))

(test-wfst symbol-type
  (is-true (progn
             (funcall symbol-table :insert ":FOO" :U 0)
             (eql :U (funcall symbol-table :symbol-type ":FOO")))))

(test-wfst symbol-type-signals-undefined-symbol
  (signals symt:undefined-symbol (funcall symbol-table :symbol-type ":FOO")))
