(defsystem "asm"
  :description "Assembler for the Y86-64 ISA described in CSAPP by Bryant and O'Hallaron."
  :author "Nicholas Hubbard"
  :maintainer "Nicholas Hubbard"
  :license "MIT"
  :version "0.1"
  :depends-on ("alexandria"
               "str"
               "let-over-lambda"
               "cl-ppcre"
               "maxpc"
               "maxpc-extensions"
               "trivial-indent")
  :components ((:file "src/utilities")
               (:file "src/asm/opcode-table")
               (:file "src/asm/register-table")
               (:file "src/asm/symbol-table")
               (:file "src/asm/parser")
               (:file "src/asm/assembler")))
