;;;; This package exists to export the *OPCODE-TABLE* special variable which
;;;; is a lexical closure that can be used to dynamically query the static and
;;;; immutable Y86-64 opcode table.

(defpackage #:opcode-table
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:*opcode-table*
           #:mnemonic
           #:opcode-type))

(in-package #:opcode-table)

;;; ==================== Types ====================

(deftype mnemonic ()
  '(member :HALT :NOP :RRMOVQ :IRMOVQ :RMMOVQ :MRMOVQ :ADDQ :SUBQ :ANDQ :XORQ :JMP :JLE :JL :JE :JNE :JGE :JG :CMOVLE :CMOVL :CMOVE :CMOVNE :CMOVGE :CMOVG :CALL :RET :PUSHQ))

(deftype opcode-type ()
  "The operand type of a Y86-64 mnemonic.
:N  -> Null operand 
:M  -> Memory operand 
:R  -> Register operand
:RR -> Register,Register operand
:IR -> Immediate,Register operand"
  '(member :N :M :R :RR :IR))

(u:defstruct-read-only entry
  "The type for a single Y86-64 opcode table entry."
  (opcode   nil :type (unsigned-byte 8))
  (mnemonic nil :type mnemonic)
  (type     nil :type opcode-type)
  (size     nil :type (unsigned-byte 8)))

;;; ==================== Opcode Table Definition ====================

(defun init-opcode-table ()
  "Initialize the *OPCODE-TABLE* global closure variable."
  (let ((opcode-table
          (list
           (make-entry :opcode #x00 :mnemonic :HALT   :type :N  :size  1)
           (make-entry :opcode #x10 :mnemonic :NOP    :type :N  :size  1)
           (make-entry :opcode #x20 :mnemonic :RRMOVQ :type :RR :size  2)
           (make-entry :opcode #x30 :mnemonic :IRMOVQ :type :IR :size 10)
           (make-entry :opcode #x40 :mnemonic :RMMOVQ :type :RR :size 10)
           (make-entry :opcode #x50 :mnemonic :MRMOVQ :type :RR :size 10)
           (make-entry :opcode #x60 :mnemonic :ADDQ   :type :RR :size  2)
           (make-entry :opcode #x61 :mnemonic :SUBQ   :type :RR :size  2)
           (make-entry :opcode #x62 :mnemonic :ANDQ   :type :RR :size  2)
           (make-entry :opcode #x63 :mnemonic :XORQ   :type :RR :size  2)
           (make-entry :opcode #x70 :mnemonic :JMP    :type :M  :size  9)
           (make-entry :opcode #x71 :mnemonic :JLE    :type :M  :size  9)
           (make-entry :opcode #x72 :mnemonic :JL     :type :M  :size  9)
           (make-entry :opcode #x73 :mnemonic :JE     :type :M  :size  9)
           (make-entry :opcode #x74 :mnemonic :JNE    :type :M  :size  9)
           (make-entry :opcode #x75 :mnemonic :JGE    :type :M  :size  9)
           (make-entry :opcode #x76 :mnemonic :JG     :type :M  :size  9)
           (make-entry :opcode #x21 :mnemonic :CMOVLE :type :RR :size  2)
           (make-entry :opcode #x22 :mnemonic :CMOVL  :type :RR :size  2)
           (make-entry :opcode #x23 :mnemonic :CMOVE  :type :RR :size  2)
           (make-entry :opcode #x24 :mnemonic :CMOVNE :type :RR :size  2)
           (make-entry :opcode #x25 :mnemonic :CMOVGE :type :RR :size  2)
           (make-entry :opcode #x26 :mnemonic :CMOVG  :type :RR :size  2)
           (make-entry :opcode #x80 :mnemonic :CALL   :type :M  :size  9)
           (make-entry :opcode #x90 :mnemonic :RET    :type :N  :size  1)
           (make-entry :opcode #xA0 :mnemonic :PUSHQ  :type :R  :size  2))))
    (lambda (function-keyword &rest inputs)
      ;; turn input strings into keywords so user the can choose input format
      (let ((inputs (mapcar (lambda (input)
                              (if (stringp input)
                                  (u:make-keyword input)
                                  input))
                            inputs)))
        
        (case function-keyword
          (:opcode-p
           ;; T iff (FIRST INPUTS) is a Y86-64 opcode
           (if (member (first inputs) (mapcar #'entry-opcode opcode-table) :test #'=)
               t))
          
          (:mnemonic-p
           ;; T iff (FIRST INPUTS) is a Y86-64 mnemonic
           (if (member (first inputs) (mapcar #'entry-mnemonic opcode-table))
               t))
          
          (:type-p
           ;; T iff (FIRST INPUTS) is a Y86-64 operation type
           (if (member (first inputs) (mapcar #'entry-type opcode-table))
               t))        

          (:all-opcodes
           ;; list of all the opcodes of the Y86-64 ISA
           (sort (mapcar #'entry-opcode opcode-table) #'<))
          
          (:all-mnemonics
           ;; list of all the mnemonics of the Y86-64 ISA
           (sort (mapcar #'entry-mnemonic opcode-table) #'string<))
          
          (:all-types
           ;; list of all the instruction types of the Y86-64 ISA
           (sort (remove-duplicates (mapcar #'entry-type opcode-table)) #'string<))
          
          (:all-opcode-strings
           ;; like :ALL-OPCODES but stringify the opcodes
           (sort (mapcar
                  (a:compose (a:curry #'format nil "~x") #'entry-opcode)
                  opcode-table)
                 #'string<))
          
          (:all-mnemonic-strings
           ;; like :ALL-MNEMONICS but stringify the mnemonics
           (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic) opcode-table) #'string<))
          
          (:all-type-strings
           ;; like :ALL-TYPES but stringify the types
           (sort (remove-duplicates
                  (mapcar (a:compose #'symbol-name #'entry-type) opcode-table))
                 #'string<))
          
          (:opcode-mnemonic
           ;; the mnemonic that has the opcode (FIRST INPUTS)
           (entry-mnemonic
            (find-if
             (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
             opcode-table)))
          
          (:opcode-type
           ;; the type of the opcode (FIRST INPUTS)
           (entry-type
            (find-if
             (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
             opcode-table))) 

          (:opcode-size
           ;; the size of the opcode (FIRST INPUTS)
           (entry-size
            (find-if
             (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
             opcode-table)))
          
          (:mnemonic-opcode
           ;; the code of the mnemonic (FIRST INPUTS)
           (entry-opcode
            (find-if
             (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))
          
          (:mnemonic-type
           ;; the type of the mnemonic (FIRST INPUTS)
           (entry-type
            (find-if
             (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))

          (:mnemonic-size
           ;; the size of a the mnemonic (FIRST INPUTS)
           (entry-size
            (find-if
             (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))
          
          (:opcode-mnemonic-string
           ;; like :OPCODE-MNEMONIC but stringify the mnemonic
           (symbol-name
            (entry-mnemonic
             (find-if
              (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
              opcode-table))))
          
          (:opcode-type-string
           ;; like :OPCODE-TYPE except stringify the type
           (symbol-name
            (entry-type
             (find-if
              (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
              opcode-table))))
          
          (:mnemonic-opcode-string
           ;; like :MNEMONIC-OPCODE except stringify the opcode.
           (format nil "~x"
                   (entry-opcode
                    (find-if
                     (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
                     opcode-table))))
          
          (:mnemonic-type-string
           ;; like :MNEMONIC-TYPE except stringify the type
           (symbol-name
            (entry-type
             (find-if
              (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
              opcode-table))))
          
          (:opcode-mnemonic-match-p
           ;; T iff the opcode (FIRST INPUTS) has mnemonic (SECOND INPUTS)
           (eql (second inputs)
                (entry-mnemonic
                 (find-if
                  (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
                  opcode-table))))

          (:opcode-type-match-p
           ;; T iff the opcode (FIRST INPUTS) has type (SECOND INPUTS)
           (eql (second inputs)
                (entry-type
                 (find-if
                  (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
                  opcode-table))))

          (:opcode-size-match-p
           ;; T iff the opcode (FIRST INPUTS) has size (SECOND-INPUTS)
           (= (second inputs)
              (entry-size
               (find-if (a:compose (a:curry #'= (first inputs)) #'entry-opcode)
                        opcode-table))))
          
          (:mnemonic-type-match-p
           ;; T iff the mnemonic (FIRST INPUTS) has type (SECOND INPUTS)
           (eql (second inputs)
                (entry-type
                 (find-if
                  (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)
                  opcode-table))))

          (:mnemonic-size-match-p
           ;; T iff the mnemonic (FIRST INPUTS) has size (SECOND INPUTS)
           (= (second inputs)
              (entry-size
               (find-if
                (a:compose (a:curry #'eql (first inputs)) #'entry-mnemonic)

                opcode-table))))
          
          (:type-mnemonics
           ;; list of all the mnemonics that have the type (FIRST INPUTS)
           (sort (mapcar #'entry-mnemonic
                         (remove-if-not
                          (a:compose (a:curry #'eql (first inputs)) #'entry-type)
                          opcode-table))
                 #'string<))
          
          (:type-opcodes
           ;; list of all the opcodes that have the type (FIRST INPUTS)
           (sort (mapcar #'entry-opcode
                         (remove-if-not
                          (a:compose (a:curry #'eql (first inputs)) #'entry-type)
                          opcode-table))
                 #'<))
          
          (:type-mnemonic-strings
           ;; like :TYPE-MNEMONICS but stringify the mnemonics
           (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic)
                         (remove-if-not
                          (a:compose (a:curry #'eql (first inputs)) #'entry-type)
                          opcode-table))
                 #'string<)) 

          (:type-opcode-strings
           ;; like :TYPE-OPCODES except stringify the opcodes
           (sort (mapcar (a:compose (a:curry #'format nil "~x") #'entry-opcode)
                         (remove-if-not
                          (a:compose (a:curry #'eql (first inputs)) #'entry-type)
                          opcode-table))
                 #'string<))
          
          (:size-opcodes
           ;; list of all the opcodes that have size (FIRST INPUTS)
           (sort (mapcar #'entry-opcode
                         (remove-if-not
                          (a:compose (a:curry #'= (first inputs)) #'entry-size)
                          opcode-table))
                 #'<))
          
          (:size-mnemonics
           ;; list of all the mnemonics that have size (FIRST INPUTS)
           (sort (mapcar #'entry-mnemonic
                         (remove-if-not
                          (a:compose (a:curry #'= (first inputs)) #'entry-size)
                          opcode-table))
                 #'string<)) 

          (:size-opcode-strings
           ;; like :SIZE-OPCODES except stringify the opcodes
           (sort (mapcar (a:compose (a:curry #'format nil "~x") #'entry-opcode)
                         (remove-if-not
                          (a:compose (a:curry #'= (first inputs)) #'entry-size)
                          opcode-table))
                 #'string<))
          
          (:size-mnemonic-strings
           ;; like :SIZE-MNEMONICS except stringify the mnemonics
           (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic)
                         (remove-if-not
                          (a:compose (a:curry #'= (first inputs)) #'entry-size)
                          opcode-table))
                 #'string<))
          
          (otherwise
           (error 'internal (format nil "The symbol ~a does not denote a valid function" function-keyword))))))))

(defparameter *opcode-table* (init-opcode-table)
  "Lexical closure over the static Y86-64 opcode table that can be used to query
information about the various opcodes.")
