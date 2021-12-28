;;;; The assemblers internal symbol table

(defpackage symbol-table
  (:export #:symbol-table #:make-entry) 
  (:nicknames #:symt)
  (:use #:cl #:bst))

(in-package #:symbol-table)

(defstruct entry 
  (name  nil :type string             :read-only t)
  (value nil :type (unsigned-byte 64) :read-only t)
  (type  nil :type keyword            :read-only t))

(defun symbol-table ()
  (let ((symbol-table (make-hash-table)))
    (lambda (function-keyword))
    ))
