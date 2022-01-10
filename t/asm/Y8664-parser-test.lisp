;;;; Testing for the definitions in src/asm/Y8664-parser.lisp

(defpackage Y8664-parser-test
  (:export #:run-tests)
  (:nicknames #:parser-t)
  (:use #:cl #:fiveam))

(in-package #:Y8664-parser-test)

