;;;; OpCode Table for the Y86-64 ISA. This package exports just one function,
;;;; OPCODE-TABLE, which returns a closure over the static OpCode Table that is
;;;; used to dynamically query the table.

(defpackage #:Y8664-opcode-table
  (:nicknames #:opct)
  (:export #:opcode-table)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword))

(in-package #:Y8664-opcode-table)

(defstruct entry
  "The type for a Y86-64 OpCode Table entry. Note that this type is not exported
   as it is for internal use only."
  (opcode   nil :type (unsigned-byte 8) :read-only t)
  (mnemonic nil :type keyword           :read-only t)
  (type     nil :type keyword           :read-only t)
  (size     nil :type (unsigned-byte 8) :read-only t))
   
(defun opcode-table ()
  "Return a closure that closes over the static OpCode Table and returns a
   dispatch table to query the OpCode Table in different ways. None of the query
   functions can mutate the OpCode Table."
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
      
      ;; if an input is a string make it a keyword instead
      (let ((inputs (mapcar (lambda (input)
                              (if (stringp input)
                                  (make-keyword (string-upcase input))
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
           ;; all the opcodes of the Y86-64 ISA
           (mapcar #'entry-opcode opcode-table))
          
          (:all-mnemonics
           ;; all the mnemonics of the Y86-64 ISA
           (mapcar #'entry-mnemonic opcode-table)) 
          
          (:all-types
           ;; all the entry types of the Y86-64 ISA
           (remove-duplicates (mapcar #'entry-type opcode-table)))
          
          (:all-opcode-strings
           ;; like :ALL-OPCODES but stringify the opcodes
           (mapcar
            (compose (curry #'format nil "~x") #'entry-opcode)
            opcode-table))
          
          (:all-mnemonic-strings
           ;; like :ALL-MNEMONICS but stringify the mnemonics
           (mapcar (compose #'symbol-name #'entry-mnemonic) opcode-table))
          
          (:all-type-strings
           ;; like :ALL-TYPES but stringify the types
           (remove-duplicates
            (mapcar (compose #'symbol-name #'entry-type) opcode-table)))
          
          (:opcode-mnemonic
           ;; the mnemonic that has the opcode (FIRST INPUTS)
           (entry-mnemonic
            (find-if
             (compose (curry #'= (first inputs)) #'entry-opcode)
             opcode-table)))
          
          (:opcode-type
           ;; the type of the opcode (FIRST INPUTS)
           (entry-type
            (find-if
             (compose (curry #'= (first inputs)) #'entry-opcode)
             opcode-table))) 

          (:opcode-size
           ;; the size of the opcode (FIRST INPUTS)
           (entry-size
            (find-if
             (compose (curry #'= (first inputs)) #'entry-opcode)
             opcode-table)))
          
          (:mnemonic-opcode
           ;; the code of the mnemonic (FIRST INPUTS)
           (entry-opcode
            (find-if
             (compose (curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))
          
          (:mnemonic-type
           ;; the type of the mnemonic (FIRST INPUTS)
           (entry-type
            (find-if
             (compose (curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))

          (:mnemonic-size
           ;; the size of a the mnemonic (FIRST INPUTS)
           (entry-size
            (find-if
             (compose (curry #'eql (first inputs)) #'entry-mnemonic)
             opcode-table)))
          
          (:opcode-mnemonic-string
           ;; like :OPCODE-MNEMONIC but stringify the mnemonic
           (symbol-name
            (entry-mnemonic
             (find-if
              (compose (curry #'= (first inputs)) #'entry-opcode)
              opcode-table))))
          
          (:opcode-type-string
           ;; like :OPCODE-TYPE except stringify the type
           (symbol-name
            (entry-type
             (find-if
              (compose (curry #'= (first inputs)) #'entry-opcode)
              opcode-table))))
          
          (:mnemonic-opcode-string
           ;; like :MNEMONIC-OPCODE except stringify the opcode.
           (format nil "~x"
                   (entry-opcode
                    (find-if
                     (compose (curry #'eql (first inputs)) #'entry-mnemonic)
                     opcode-table))))
          
          (:mnemonic-type-string
           ;; like :MNEMONIC-TYPE except stringify the type
           (symbol-name
            (entry-type
             (find-if
              (compose (curry #'eql (first inputs)) #'entry-mnemonic)
              opcode-table))))
          
          (:opcode-mnemonic-match-p
           ;; T iff the opcode (FIRST INPUTS) has mnemonic (SECOND INPUTS)
           (eql (second inputs)
                (entry-mnemonic
                 (find-if
                  (compose (curry #'= (first inputs)) #'entry-opcode)
                  opcode-table))))

          (:opcode-type-match-p
           ;; T iff the opcode (FIRST INPUTS) has type (SECOND INPUTS)
           (eql (second inputs)
                (entry-type
                 (find-if
                  (compose (curry #'= (first inputs)) #'entry-opcode)
                  opcode-table))))

          (:opcode-size-match-p
           ;; T iff the opcode (FIRST INPUTS) has size (SECOND-INPUTS)
           (= (second inputs)
              (entry-size
               (find-if (compose (curry #'= (first inputs)) #'entry-opcode)
                        opcode-table))))
          
          (:mnemonic-type-match-p
           ;; T iff the mnemonic (FIRST INPUTS) has type (SECOND INPUTS)
           (eql (second inputs)
                (entry-type
                 (find-if
                  (compose (curry #'eql (first inputs)) #'entry-mnemonic)
                  opcode-table))))

          (:mnemonic-size-match-p
           ;; T iff the mnemonic (FIRST INPUTS) has size (SECOND INPUTS)
           (= (second inputs)
              (entry-size
               (find-if
                (compose (curry #'eql (first inputs)) #'entry-mnemonic)
                opcode-table))))
          
          (:type-mnemonics
           ;; all the mnemonics that have the type (FIRST INPUTS)
           (mapcar #'entry-mnemonic
                   (remove-if-not
                    (compose (curry #'eql (first inputs)) #'entry-type)
                    opcode-table)))
          
          (:type-opcodes
           ;; all the opcodes that have the type (FIRST INPUTS)
           (mapcar #'entry-opcode
                   (remove-if-not
                    (compose (curry #'eql (first inputs)) #'entry-type)
                    opcode-table)))
          
          (:type-mnemonic-strings
           ;; like :TYPE-MNEMONICS but stringify the mnemonics
           (mapcar (compose #'symbol-name #'entry-mnemonic)
                   (remove-if-not
                    (compose (curry #'eql (first inputs)) #'entry-type)
                    opcode-table))) 

          (:type-opcode-strings
           ;; like :TYPE-OPCODES except stringify the opcodes
           (mapcar (compose (curry #'format nil "~x") #'entry-opcode)
                   (remove-if-not
                    (compose (curry #'eql (first inputs)) #'entry-type)
                    opcode-table)))
          
          (:size-opcodes
           ;; all the opcodes that have size (FIRST INPUTS)
           (mapcar #'entry-opcode
                   (remove-if-not
                    (compose (curry #'= (first inputs)) #'entry-size)
                    opcode-table)))
          
          (:size-mnemonics
           ;; all the mnemonics that have size (FIRST INPUTS)
           (mapcar #'entry-mnemonic
                   (remove-if-not
                    (compose (curry #'= (first inputs)) #'entry-size)
                    opcode-table))) 

          (:size-opcode-strings
           ;; like :SIZE-OPCODES except stringify the opcodes
           (mapcar (compose (curry #'format nil "~x") #'entry-opcode)
                   (remove-if-not
                    (compose (curry #'= (first inputs)) #'entry-size)
                    opcode-table)))
          
          (:size-mnemonic-strings
           ;; like :SIZE-MNEMONICS except stringify the mnemonics
           (mapcar (compose #'symbol-name #'entry-mnemonic)
                   (remove-if-not
                    (compose (curry #'= (first inputs)) #'entry-size)
                    opcode-table)))
          
          (otherwise
           (error (format t "The symbol ~a does not denote a valid function" function-keyword))))))))
