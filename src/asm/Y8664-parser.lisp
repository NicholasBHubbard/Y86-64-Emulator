;;;; Parser for Y86-64 assembly language

(defpackage #:Y8664-parser
  (:nicknames #:parser #:p)
  (:use #:cl #:smug)
  (:import-from #:alexandria #:curry))

(in-package #:Y8664-parser)

(defparameter *opcode-table* (opct:opcode-table))

(defparameter *register-table* (regt:register-table))

(defun mnemonic-parser ()
  (.one-of #'.string= (funcall *opcode-table* :all-mnemonic-strings)))

(defun register-parser ()
  (.one-of #'.string= (funcall *register-table* :all-register-name-strings)))

(defun label-parser ()
  (.let* ((first (.upper-case-p))
          (rest  (.zero-or-more (.or (.upper-case-p) (.digit-char-p))))
          (_     (.char= #\:)))
    (.identity (format nil "~a~{~a~}" first rest))))
