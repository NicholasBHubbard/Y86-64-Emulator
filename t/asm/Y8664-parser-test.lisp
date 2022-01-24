;;;; Testing for the definitions in src/asm/Y8664-parser.lisp

(defpackage Y8664-parser-test
  (:export #:run-tests)
  (:use #:cl #:fiveam)
  (:import-from #:Y8664-parser #:parse-source-line))

(in-package #:Y8664-parser-test)

(def-suite y8664-parser-suite
  :description "Test suite for the Y8664-PARSER package")

(in-suite y8664-parser-suite)

(defun run-tests ()
  (run! 'y8664-parser-suite))

;;; Tests

(test null-arg
  (is-true (equal (parse-source-line "HALT")
                  (list :label nil
                        :mnemonic "HALT"
                        :args nil
                        :eol-comment nil))))

(test with-label-present
  (is-true (equal (parse-source-line ":MY-LABEL: HALT")
                  (list :label ":MY-LABEL:"
                        :mnemonic "HALT"
                        :args nil
                        :eol-comment nil))))

(test null-arg-with-comment
  (is-true (equal (parse-source-line "NOP #comment")
                  (list :label nil
                        :mnemonic "NOP"
                        :args nil
                        :eol-comment "#comment"))))

(test single-register-arg
  (is-true (equal (parse-source-line "PUSHQ RAX")
                  (list :label nil
                        :mnemonic "PUSHQ"
                        :args (list :reg "RAX")
                        :eol-comment nil))))

(test single-register-arg-with-comment
  (is-true (equal (parse-source-line "PUSHQ RDX #comment")
                  (list :label nil
                        :mnemonic "PUSHQ"
                        :args (list :reg "RDX")
                        :eol-comment "#comment"))))

(test two-register-args
  (is-true (equal (parse-source-line "XORQ R10,R14")
                  (list :label nil
                        :mnemonic "XORQ"
                        :args (list :src-reg "R10" :dst-reg "R14")
                        :eol-comment nil))))

(test two-register-args-with-comment
  (is-true (equal (parse-source-line "ANDQ R8,R9 #comment")
                  (list :label nil
                        :mnemonic "ANDQ"
                        :args (list :src-reg "R8" :dst-reg "R9")
                        :eol-comment "#comment"))))

(test immediate-register-args
  (is-true (equal (parse-source-line "IRMOVQ 12,R11")
                  (list :label nil
                        :mnemonic "IRMOVQ"
                        :args (list :imm "12" :reg "R11")
                        :eol-comment nil))))

(test immediate-register-args
  (is-true (equal (parse-source-line "IRMOVQ 0xAB,RSP")
                  (list :label nil
                        :mnemonic "IRMOVQ"
                        :args (list :imm "0xAB" :reg "RSP")
                        :eol-comment nil))))
