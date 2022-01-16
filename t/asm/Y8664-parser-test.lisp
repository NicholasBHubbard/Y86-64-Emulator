;;;; Testing for the definitions in src/asm/Y8664-parser.lisp

(defpackage Y8664-parser-test
  (:export #:run-tests)
  (:use #:cl #:fiveam))

(in-package #:Y8664-parser-test)

(def-suite y8664-parser-suite
  :description "Test suite for the SYMBOL-TABLE package")

(in-suite y8664-parser-suite)

(defun run-tests ()
  (run! 'y8664-parser-suite))

;;; Tests

