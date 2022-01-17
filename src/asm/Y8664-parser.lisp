;;;; Parser for Y86-64 assembly language
;;;;
;;;; BNF Grammar
;;;;
;;;; Start ::= Label | Mnemonic
;;;; Mnemonic ::= halt | nop | ...
;;;; Label ::= /:[A-Z][A-Z0-9]*:/
;;;; Instruction ::= Mnemonic Operand*
;;;; Operand ::= Register | Immediate | Memory
;;;; Register ::= rax | rdx | ...
;;;; Immediate ::= /[1-9][0-9]*/
;;;; Memory ::= ?

(defpackage #:Y8664-parser
  (:export #:parse-source-line)
  (:use #:cl #:y8664-opcode-table #:parse)
  (:import-from #:lexer #:define-lexer #:with-lexer #:with-token-reader))

(in-package #:Y8664-parser)

(defun parse-source-line (source-line)
  (with-lexer (lexer 'source-line-lexer source-line)
    (with-token-reader (next-token lexer)
      (parse 'source-line-parser next-token))))

;;; Lexer

(define-lexer source-line-lexer (state)
  ("%s+"
   (values :next-token $$))
  ("#[^%n]*"
   (values :eol-comment $$))
  (","
   (values :comma $$))
  (":%u[%-%u%d]*:"
   (values :label $$))
  ("0[xX][%x]+|[1-9]%d*"
   (values :immediate $$))
  ("HALT|NOP|RRMOVQ|IRMOVQ|RMMOVQ|MRMOVQ|ADDQ|SUBQ|ANDQ|XORQ|JMP|JLE|JL|JE|JNE|JGE|JG|CMOVLE|CMOVL|CMOVE|CMOVNE|CMOVGE|CMOVG|CALL|RET|PUSHQ"
   (values :mnemonic $$))
  ("RAX|RCX|RDX|RBX|RSP|RBP|RSI|RDI|R8|R9|R10|R11|R12|R13|R14"
   (values :register $$)))

;;; Main parser

(define-parser source-line-parser
  "Parse a Y86-64 assembly language source line into a plist."
  (.let* ((label 'maybe-label-parser)
          (mn (.is :mnemonic))
          (args (case (funcall *opcode-table* :mnemonic-type mn)
                  (:N  nil)
                  (:R  'single-register-arg-parser)
                  (:RR 'register-register-args-parser)
                  (:IR 'immediate-register-args-parser)))
          (eol-comment 'eol-comment-or-eof-parser))
    (.ret (list :label label
                :mnemonic mn
                :args args
                :eol-comment eol-comment))))

;;; Helper parsers

(define-parser eol-comment-or-eof-parser
  "Parse either an eol comment or an eof."
  (.or (.is :eol-comment) (.eof)))

(define-parser maybe-label-parser
  "Try to parse a label. If successful return the label, else return nil."
  (.opt nil (.is :label)))

(define-parser single-register-arg-parser
  "Parse a single register name."
  (.let (reg (.is :register))
    (.ret (list :reg reg))))

(define-parser register-register-args-parser
  "Parse two register names that are separated by a comma."
  (.let* ((src-reg (.is :register))
          (_       (.ignore (.is :comma)))
          (dst-reg (.is :register)))
    (.ret (list :src-reg src-reg
                :dst-reg dst-reg))))

(define-parser immediate-register-args-parser
  "Parse an immediate then a register name that are separated by a comma."
  (.let* ((imm (.is :immediate))
          (_   (.ignore (.is :comma)))
          (reg (.is :register)))
    (.ret (list :imm imm :reg reg))))
