(defpackage y8664-asm-test
  (:export #:run-tests)
  (:use #:cl #:fiveam))

(in-package #:y8664-asm-test)

(def-suite y8664-asm-suite
  :description "Test suite for the entire Y8664-ASM system.")

(in-suite y8664-asm-suite)

(defun run-tests ()
  (progn
    (symbol-table-test:run-tests)
    (y8664-register-table-test:run-tests)
    (y8664-opcode-table-test:run-tests)))
