(defpackage #:opcode-table
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:opcode-table
           #:mnemonic
           #:instruction-type))

(in-package #:opcode-table)

;;; ==================== Types ====================

(deftype mnemonic ()
  "The type of a Y86-64 mnemonic."
  '(member :HALT :NOP :RRMOVQ :IRMOVQ :RMMOVQ :MRMOVQ :ADDQ :SUBQ :ANDQ :XORQ :JMP :JLE :JL :JE :JNE :JGE :JG :CMOVLE :CMOVL :CMOVE :CMOVNE :CMOVGE :CMOVG :CALL :RET :PUSHQ))

(deftype instruction-type ()
  "The operand type of a Y86-64 instruction.
:N  -> Null operand 
:M  -> Memory operand 
:R  -> Register operand
:RR -> Register,Register operand
:IR -> Immediate,Register operand"
  '(member :N :M :R :RR :IR))

(u:defstruct-read-only entry
  "The type of a single Y86-64 opcode table entry."
  (opcode   nil :type (unsigned-byte 8))
  (mnemonic nil :type mnemonic)
  (type     nil :type instruction-type)
  (size     nil :type (unsigned-byte 8)))

;;; ==================== Opcode Table Definition ====================

(u:defclosure opcode-table
  "Lexical closure over the Y86-64 static opcode table. This closure uses
LOL:DLAMBDA to provide various function keywords for dynamically dispatching a
query function to the opcode table. 

Documentation for all the provided dispatch keywords:

:opcode-p opcode
  Return T if OPCODE is a valid Y86-64 opcode and return NIL otherwise.

:mnemonic-p mn
  Return T if MN is a Y86-64 mnemonic and return NIL otherwise. MN can be either
  a keyword or a string.

:type-p type
  Return T if TYPE is a Y86-64 instruction type and return NIL otherwise.

:all-opcodes
  Return a list of all the Y86-64 opcodes.

:all-mnemonics
  Return a list of all the Y86-64 mnemonic keywords.

:all-types
  Return a list of all the Y86-64 instruction types.

:all-opcode-strings
  Return a list of all the Y86-64 opcodes as strings.

:all-mnemonic-strings
  Return a list of all the Y86-64 mnemonics as strings.

:all-type-strings
  Return a list of all Y86-64 instruction types as strings.

:opcode-mnemonic opcode
  Return the mnemonic keyword of the instruction with opcode OPCODE.

:opcode-type opcode
  Return the type of the instruction with opcode OPCODE.

:opcode-size opcode
  Return the size of the instruction with opcode OPCODE.

:mnemonic-opcode mn
  Return the opcode of the instruction with mnemonic MN. MN can be either a 
  keyword or a string.

:mnemonic-type mn
  Return the type of the instruction with mnemonic MN. MN can be either a
  keyword or a string.

:mnemonic-size mn
  Return the size of the instruction with mnemonic MN. MN can be either a
  keyword or a string.

:opcode-mnemonic-string opcode
  Return the mnemonic string of the instruction with opcode OPCODE.

:opcode-type-string opcode
  Return the type of the instruction with opcode OPCODE as a string.

:mnemonic-opcode-string
  Return the opcode of the instruction with mnemonic MN as a string. MN can be
  either a keyword or a string.

:mnemonic-type-string mn
  Return the type of the instruction the mnemonic MN as a string. MN can be 
  either a keyword or a string.

:opcode-mnemonic-match-p opcode mn
  Return T if there exists an instruction with opcode OPCODE and mnemonic MN and
 return NIL otherwise. MN can be either a keyword or a string.

:opcode-type-match-p opcode type
  Return T if there exists an instruction with opcode OPCODE and type TYPE and
  return NIL otherwise.

:opcode-size-match-p opcode size
  Return T if there exists an instruction with opcode OPCODE and size SIZE and
  return NIL otherwise.

:mnemonic-type-match-p mn type
  Return T if there exists an instruction with mnemonic MN and type TYPE and
  return NIL otherwise. MN can be either a keyword or a string.

:mnemonic-size-match-p mn size
  Return T if there exists an instruction with mnemonic MN and size SIZE. MN can
  be either a keyword or a string.

:type-mnemonics type
  Return a list of all mnemonic keywords of instructions with type TYPE.

:type-opcodes
  Return a list of all the opcodes of the instructions with type TYPE.

:type-mnemonic-strings type
  Return a list of all mnemonic strings of instructions with type TYPE.

:type-opcode-strings type
  Return a list of all opcode strings of instructions with type TYPE.

:size-opcodes size
  Return a list of all the opcodes of instructions with size SIZE.

:size-mnemonics size
  Return a list of all the mnemonic keywords of instructions with size SIZE.

:size-opcode-strings size
  Return a list of all the opcode string of instructions with size SIZE.

:size-mnemonic-strings size
  Return a list of the mnemonic strings of instructions with size SIZE."
  (let ((opcode-table
          (list
           (make-entry :opcode #x00 :mnemonic :HALT   :type :N  :size  1)
           (make-entry :opcode #x10 :mnemonic :NOP    :type :N  :size  1)
           (make-entry :opcode #x20 :mnemonic :RRMOVQ :type :RR :size  2)
           (make-entry :opcode #x30 :mnemonic :IRMOVQ :type :IR :size 10)
           (make-entry :opcode #x40 :mnemonic :RMMOVQ :type :RR :size  2)
           (make-entry :opcode #x50 :mnemonic :MRMOVQ :type :RR :size  2)
           (make-entry :opcode #x60 :mnemonic :ADDQ   :type :RR :size  2)
           (make-entry :opcode #x61 :mnemonic :SUBQ   :type :RR :size  2)
           (make-entry :opcode #x62 :mnemonic :ANDQ   :type :RR :size  2)
           (make-entry :opcode #x63 :mnemonic :XORQ   :type :RR :size  2)
           (make-entry :opcode #x70 :mnemonic :JMP    :type :M  :size  11)
           (make-entry :opcode #x71 :mnemonic :JLE    :type :M  :size  11)
           (make-entry :opcode #x72 :mnemonic :JL     :type :M  :size  11)
           (make-entry :opcode #x73 :mnemonic :JE     :type :M  :size  11)
           (make-entry :opcode #x74 :mnemonic :JNE    :type :M  :size  11)
           (make-entry :opcode #x75 :mnemonic :JGE    :type :M  :size  11)
           (make-entry :opcode #x76 :mnemonic :JG     :type :M  :size  11)
           (make-entry :opcode #x21 :mnemonic :CMOVLE :type :RR :size  2)
           (make-entry :opcode #x22 :mnemonic :CMOVL  :type :RR :size  2)
           (make-entry :opcode #x23 :mnemonic :CMOVE  :type :RR :size  2)
           (make-entry :opcode #x24 :mnemonic :CMOVNE :type :RR :size  2)
           (make-entry :opcode #x25 :mnemonic :CMOVGE :type :RR :size  2)
           (make-entry :opcode #x26 :mnemonic :CMOVG  :type :RR :size  2)
           (make-entry :opcode #x80 :mnemonic :CALL   :type :M  :size  11)
           (make-entry :opcode #x90 :mnemonic :RET    :type :N  :size  1)
           (make-entry :opcode #xA0 :mnemonic :PUSHQ  :type :R  :size  2))))
    (lol:dlambda
      (:opcode-p (opcode)
        (if (member opcode (mapcar #'entry-opcode opcode-table) :test #'=) t))
      
      (:mnemonic-p (mn)
        (if (member (u:as-keyword mn) (mapcar #'entry-mnemonic opcode-table)) t))
      
      (:type-p (type)
        (if (member type (mapcar #'entry-type opcode-table)) t))
      
      (:all-opcodes ()

        (sort (mapcar #'entry-opcode opcode-table) #'<))
      
      (:all-mnemonics ()
        (sort (mapcar #'entry-mnemonic opcode-table) #'string<))
      
      (:all-types ()
        (sort (remove-duplicates (mapcar #'entry-type opcode-table)) #'string<))
      
      (:all-opcode-strings ()
        (sort (mapcar
               (a:compose (a:curry #'format nil "~x") #'entry-opcode)
               opcode-table)
              #'string<))
      
      (:all-mnemonic-strings ()
        (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic) opcode-table) #'string<))
      
      (:all-type-strings ()
        (sort (remove-duplicates
               (mapcar (a:compose #'symbol-name #'entry-type) opcode-table))
              #'string<))
      
      (:opcode-mnemonic (opcode)
        (entry-mnemonic
         (find-if
          (a:compose (a:curry #'= opcode) #'entry-opcode)
          opcode-table)))
      
      (:opcode-type (opcode)
        (entry-type
         (find-if
          (a:compose (a:curry #'= opcode) #'entry-opcode)
          opcode-table))) 

      (:opcode-size (opcode)
        (entry-size
         (find-if
          (a:compose (a:curry #'= opcode) #'entry-opcode)
          opcode-table)))
      
      (:mnemonic-opcode (mn)
        (entry-opcode
         (find-if
          (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
          opcode-table)))
      
      (:mnemonic-type (mn)
        (entry-type
         (find-if
          (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
          opcode-table)))

      (:mnemonic-size (mn)
        (entry-size
         (find-if
          (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
          opcode-table)))
      
      (:opcode-mnemonic-string (opcode)
        (symbol-name
         (entry-mnemonic
          (find-if
           (a:compose (a:curry #'= opcode) #'entry-opcode)
           opcode-table))))
      
      (:opcode-type-string (opcode)
        (symbol-name
         (entry-type
          (find-if
           (a:compose (a:curry #'= opcode) #'entry-opcode)
           opcode-table))))
      
      (:mnemonic-opcode-string (mn)
        (format nil "~x"
                (entry-opcode
                 (find-if
                  (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
                  opcode-table))))
      
      (:mnemonic-type-string (mn)
        (symbol-name
         (entry-type
          (find-if
           (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
           opcode-table))))
      
      (:opcode-mnemonic-match-p (opcode mn)
        (eql (u:as-keyword mn)
             (entry-mnemonic
              (find-if
               (a:compose (a:curry #'= opcode) #'entry-opcode)
               opcode-table))))

      (:opcode-type-match-p (opcode type)
        (eql type
             (entry-type
              (find-if
               (a:compose (a:curry #'= opcode) #'entry-opcode)
               opcode-table))))

      (:opcode-size-match-p (opcode size)
        (= size
           (entry-size
            (find-if (a:compose (a:curry #'= opcode) #'entry-opcode)
                     opcode-table))))
      
      (:mnemonic-type-match-p (mn type)
        (eql type
             (entry-type
              (find-if
               (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)
               opcode-table))))

      (:mnemonic-size-match-p (mn size)
        (= size
           (entry-size
            (find-if
             (a:compose (a:curry #'eql (u:as-keyword mn)) #'entry-mnemonic)

             opcode-table))))
      
      (:type-mnemonics (type)
        (sort (mapcar #'entry-mnemonic
                      (remove-if-not
                       (a:compose (a:curry #'eql type) #'entry-type)
                       opcode-table))
              #'string<))
      
      (:type-opcodes (type)
        (sort (mapcar #'entry-opcode
                      (remove-if-not
                       (a:compose (a:curry #'eql type) #'entry-type)
                       opcode-table))
              #'<))
      
      (:type-mnemonic-strings (type)
        (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic)
                      (remove-if-not
                       (a:compose (a:curry #'eql type) #'entry-type)
                       opcode-table))
              #'string<)) 

      (:type-opcode-strings (type)
        (sort (mapcar (a:compose (a:curry #'format nil "~x") #'entry-opcode)
                      (remove-if-not
                       (a:compose (a:curry #'eql type) #'entry-type)
                       opcode-table))
              #'string<))
      
      (:size-opcodes (size)
        (sort (mapcar #'entry-opcode
                      (remove-if-not
                       (a:compose (a:curry #'= size) #'entry-size)
                       opcode-table))
              #'<))
      
      (:size-mnemonics (size)
        (sort (mapcar #'entry-mnemonic
                      (remove-if-not
                       (a:compose (a:curry #'= size) #'entry-size)
                       opcode-table))
              #'string<)) 

      (:size-opcode-strings (size)
        (sort (mapcar (a:compose (a:curry #'format nil "~x") #'entry-opcode)
                      (remove-if-not
                       (a:compose (a:curry #'= size) #'entry-size)
                       opcode-table))
              #'string<))
      
      (:size-mnemonic-strings (size)
        (sort (mapcar (a:compose #'symbol-name #'entry-mnemonic)
                      (remove-if-not
                       (a:compose (a:curry #'= size) #'entry-size)
                       opcode-table))
              #'string<))

      (t (&rest ignore)
        (declare (ignore ignore))
        (error 'u:internal-error :reason "Illegal OPCODE-TABLE function keyword.")))))
