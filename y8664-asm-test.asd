(asdf:defsystem "y8664-asm-test"
  :description "Tests for the Y86-64 assembler"
  :author "Nicholas Hubbard"
  :maintainer "Nicholas Hubbard"
  :license "MIT"
  :version "0.1"
  :depends-on ("y8664-asm" "fiveam" "alexandria")
  :components ((:file "t/asm/Y8664-register-table-test")
               (:file "t/asm/Y8664-opcode-table-test")
               (:file "t/asm/symbol-table-test")
               (:file "t/final-test")))
