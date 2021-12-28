(defpackage symbol-table-test
  (:export #:run-tests)
  (:nicknames #:symt-t)
  (:use #:cl #:fiveam)
  (:import-from #:alexandria
                #:compose
                #:curry))

(in-package #:symbol-table-test)

;;; Tests

(def-suite symbol-table-suite
  :description "Test suite for the SYMBOL-TABLE package")

(in-suite symbol-table-suite)

(defun run-tests ()
  (run! 'symbol-table-suite))
