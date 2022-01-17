;;;; Testing for the definitions in src/asm/Y8664-parser.lisp

(defpackage Y8664-parser-test
  (:export #:run-tests)
  (:use #:cl #:fiveam)
  (:import-from #:Y8664-parser #:parse-source-line))

(in-package #:Y8664-parser-test)

(def-suite y8664-parser-suite
  :description "Test suite for the SYMBOL-TABLE package")

(in-suite y8664-parser-suite)

(defun run-tests ()
  (run! 'y8664-parser-suite))

;;; Tests

(test null-arg
  (is-true
   (equal (parse-source-line "HALT")
          (list :label nil :mnemonic "HALT" :args nil :eol-comment nil))))

(test null-arg-eol-comment
  (is-true
   (equal (parse-source-line "HALT #comment")
          (list :label nil :mnemonic "HALT" :args nil :eol-comment "#comment"))))

(test single-register-arg
  (is-true
   (equal (parse-source-line ""))))
