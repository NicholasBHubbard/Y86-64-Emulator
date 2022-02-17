(defpackage asm-test
  (:export #:run-tests)
  (:use #:cl #:fiveam))

(in-package #:asm-test)

(def-suite asm-suite
  :description "Test suite for the entire ASM system.")

(in-suite asm-suite)

(defun run-tests ()
  (progn
    (symbol-table-test:run-tests)
    (register-table-test:run-tests)
    (opcode-table-test:run-tests)
    (parser-test:run-tests)))
