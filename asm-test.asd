(asdf:defsystem "asm-test"
  :description "Testing for the asm system."
  :author "Nicholas Hubbard"
  :maintainer "Nicholas Hubbard"
  :license "MIT"
  :version "0.1"
  :depends-on ("asm" "fiveam" "alexandria")
  :components ((:file "t/asm/register-table-test")
               (:file "t/asm/opcode-table-test")
               (:file "t/asm/symbol-table-test")
               (:file "t/asm/parser-test")
               (:file "t/asm-test")))
