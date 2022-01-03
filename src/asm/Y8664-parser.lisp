;;;; Parser for Y86-64 assembly language
;;;;
;;;; BNF Grammar
;;;;
;;;; Start ::= Label | Mnemonic
;;;; Mnemonic ::= halt | nop | ...
;;;; Label ::= /^:[A-Z][A-Z0-9]*$/
;;;; Instruction ::= Mnemonic Operand*
;;;; Operand ::= Register | Immediate | Memory
;;;; Register ::= rax | rdx | ...
;;;; Immediate ::= /[1-9][0-9]*/
;;;; Memory ::= ?

(defpackage #:Y8664-parser
  (:nicknames #:parser)
  (:use #:cl #:parse))

(in-package #:Y8664-parser)

(defun make-token-reader (list)
  #'(lambda ()
      (let ((x (pop list)))
        (when x
          (etypecase x
            (string    (values :string x))
            (character (values :character x))
            (number    (values :number x))
            (symbol    (values :symbol x)))))))

