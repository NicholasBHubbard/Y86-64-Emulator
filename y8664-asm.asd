(asdf:defsystem "y8664-asm"
  :description "Assembler for the Y86-64 emulator"
  :author "Nicholas Hubbard"
  :maintainer "Nicholas Hubbard"
  :license "MIT"
  :version "0.1"
  :depends-on ("alexandria" "re" "parse")
  :components ((:file "src/asm/Y8664-register-table")
               (:file "src/asm/symbol-table")
               (:file "src/asm/Y8664-opcode-table")
               (:file "src/asm/Y8664-parser")))
