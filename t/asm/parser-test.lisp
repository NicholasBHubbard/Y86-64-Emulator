;;;; Testing for the definitions in src/asm/parser.lisp

(defpackage parser-test
  (:use #:cl #:fiveam #:parser)
  (:local-nicknames (#:a #:alexandria))
  (:import-from #:maxpc #:parse)
  (:import-from #:opcode-table #:*opcode-table*)
  (:import-from #:register-table #:*register-table*)
  (:export #:run-tests))

(in-package #:parser-test)

(def-suite parser-suite
  :description "Test suite for the asm systems PARSER package.")

(in-suite parser-suite)

(defun run-tests ()
  (run! 'parser-suite))

;;; ==================== Helpers ====================

(defun all-succeed (parser inputs)
  "Each input in INPUTS will be tested if it succeeds under PARSER."
  (handler-case (every (lambda (input) (parse-success-p input parser)) inputs)
    (parse-failure (e)
      (declare (ignore e))
      nil)))

(defun all-fail (parser inputs)
  "T if all values in INPUTS fail to parse under PARSER."
  (handler-case (notany (lambda (input) (parse-success-p input parser)) inputs)
    (parse-failure (e)
      (declare (ignore e))
      t)))

;;; ==================== =SYMBOL-NAME ====================

(test =symbol-name-succeeds
  (is-true (all-succeed (parser::=symbol-name)
                        (list "foo" ".foo" "fo12o" "FOO" "foo-bar-baz"))))

(test =symbol-name-fails
  (is-true (all-fail (parser::=symbol-name)
                     (list ";foo" "12foo" "foo_bar" "fo;o" "foo.bar" "foo:"))))

(test =symbol-name-production
  (is-true (string= "foo" (parse "foo" (parser::=symbol-name)))))

;;; ==================== =LABEL ====================

(test =label-succeeds
  (is-true (parse-success-p "foo:" (parser::=label))))

(test =label-production
  (is-true (string= "foo" (parse "foo:" (parser::=label)))))

;;; ==================== =MNEMONIC ====================

(all-succeed (parser::=mnemonic)
             (list "foo" "bar" "baz"))

(test =mnemonic-succeeds
  (is-true (all-succeed (parser::=mnemonic)
                        (mapcar (lambda (mn)
                                  (if (= 1 (random 2))
                                      (string-upcase mn)
                                      mn))
                                (funcall *opcode-table* :all-mnemonic-strings)))))

(test =mnemonic-fails
  (is-true (all-fail (parser::=mnemonic)
                     (list "foo" "haltt" ".halt"))))

(test =mnemonic-production
  (is-true (eq :HALT (parse "halt" (parser::=mnemonic)))))

;;; ==================== =REGISTER ====================

(test =register-succeeds
  (is-true (all-succeed (parser::=register)
                        (mapcar (lambda (reg)
                                  (let ((reg (if (= 1 (random 2))
                                                 (string-upcase reg)
                                                 reg)))
                                    (concatenate 'string "%" reg)))
                                (funcall *register-table* :all-register-name-strings)))))

(test =register-fails
  (is-true (all-fail (parser::=register)
                     (list "foo" "%foo" "%raxx" ".%rax"))))

(test =register-production
  (is-true (eq :RAX (parse "%rax" (parser::=register)))))

;;; ==================== =IMMEDIATE ====================

(test =immediate-succeeds
  (is-true (all-succeed (parser::=immediate)
                        (list "$12" "$0xab" "$0b1010" "$-12" "$-0xab" "$-0b101"))))

(test =immediate-fails
  (is-true (all-fail (parser::=immediate)
                     (list "12" "ab" "1010" "0xab" "0b1010" "$0Xab" "-$12" "$12a"))))

(test =immediate-production
  (is-true (and (= 12  (parse "$12"     (parser::=immediate)))
                (= -12 (parse "$-12"    (parser::=immediate)))
                (= 171 (parse "$0xab"   (parser::=immediate)))
                (= 10  (parse "$0b1010" (parser::=immediate))))))

;;; ==================== =EOL-COMMENT ====================

(test =eol-comment-succeeds
  (is-true (all-succeed (parser::=eol-comment)
                        (list "#foo" "#" "##foo" "# foo"))))

(test =eol-comment-fails
  (is-true (all-fail (parser::=eol-comment)
                     (list "foo" "foo#" "f#oo"))))

(test =eol-comment-production
  (is-true (and (string= "#foo" (parse "#foo" (parser::=eol-comment)))
                (string= "###foo" (parse "###foo" (parser::=eol-comment))))))

;;; ==================== =REGISTER-OPERAND ====================

(test =register-operand-succeeds
  (is-true (all-succeed (parser::=register-operand)
                        (mapcar (lambda (reg) (concatenate 'string "%" reg))
                                (funcall *register-table* :all-register-name-strings)))))

(test =register-operand-fails
  (is-true (all-fail (parser::=register-operand)
                     (list "%foo" "rax" ",%rax"))))

(test =register-operand-production
  (is-true (equal (list :operand1 :RAX :operand2 nil)
                  (parse "%rax" (parser::=register-operand)))))

;;; ==================== =REGISTER-REGISTER-OPERANDS ====================

(test =register-register-operands-succeeds
  (is-true (all-succeed (parser::=register-register-operands)
                        (list "%rax,%rax" "%rax, %rdx" "%rax ,%rdx" "%rax , %rdx" "%rax  ,  %rdx"))))

(test =register-register-operands-fails
  (is-true (all-fail (parser::=register-register-operands)
                     (list "%rax" "%rax," ",%rdx" "%rax %rax" "%rax%rax"))))

(test =register-register-operands-production
  (is-true (equal (list :operand1 :RAX :operand2 :RDX)
                  (parse "%rax,%rdx" (parser::=register-register-operands)))))

;;; ==================== =IMMEDIATE-REGISTER-OPERANDS ====================

(test =immediate-register-operands-succeeds
  (is-true (all-succeed (parser::=immediate-register-operands)
                        (list "$12,%rax" "$0xab, %rax" "$0b1010 ,%rax" "$12 , %rax"))))

(test =immediate-register-operands-succeeds
  (is-true (all-fail (parser::=immediate-register-operands)
                     (list "$12," ",%rax" "$12 %rax" "$12%rax"))))

(test =immediate-register-operands-production
  (is-true (equal (list :operand1 12 :operand2 :RAX)
                  (parse "$12,%rax" (parser::=immediate-register-operands)))))

;;; ==================== =ABSOLUTE-MEMORY ====================

(test =absolute-memory-succeeds
  (is-true (all-succeed (parser::=absolute-memory)
                        (list "$12" "$-12" "$0xab" "$0b1010"))))

(test =absolute-memory-fails
  (is-true (all-fail (parser::=absolute-memory)
                     (list "12" "-12" "ab" "1010"))))

(test =absolute-memory-production
  (is-true (equalp (parser::make-memory :offset 12)
                   (parse "$12" (parser::=absolute-memory)))))

;;; ==================== =INDIRECT-MEMORY ====================

(test =indirect-memory-succeeds
  (is-true (all-succeed (parser::=indirect-memory)
                        (list "%rax" "%RDX"))))

(test =indirect-memory-fails
  (is-true (all-fail (parser::=indirect-memory)
                     (list "foo" "%foo" "$12"))))

(test =indirect-memory-production
  (is-true (equalp (parser::make-memory :base :RAX)
                   (parse "%rax" (parser::=indirect-memory)))))

;;; ==================== =BASE-DISPLACEMENT-MEMORY ====================

(test =base-displacement-memory-succeeds
  (is-true (all-succeed (parser::=base-displacement-memory)
                        (list "$12(%rax)" "$0xab(%RAX)"))))

(test =base-displacement-memory-fails
  (is-true (all-fail (parser::=base-displacement-memory)
                     (list "$12 (%rax)" "$12( %rax)" "$12(%rax )" "$12( %rax )"))))

(test =base-displacement-memory-production
  (is-true (equalp (parser::make-memory :offset 12 :base :RAX)
                   (parse "$12(%rax)" (parser::=base-displacement-memory)))))

;;; ==================== =INDEXED-MEMORY ====================

(test =indexed-memory-succeeds
  (is-true (all-succeed (parser::=indexed-memory)
                        (list "$12(%rax,%rdx)" "(%rax,%rdx)"))))

(test =indexed-memory-fails
  (is-true (all-fail (parser::=indexed-memory)
                     (list "$12(%rax ,%rdx)" "(%rax, %rdx)" "(%rax , %rdx)" "(%rax,%rdx" "%rax,%rdx)"))))

(test =indexed-memory-production
  (is-true (equalp (parser::make-memory :offset 12 :base :RAX :index :RDX)
                   (parse "$12(%rax,%rdx)" (parser::=indexed-memory)))))

;;; ==================== =SCALED-INDEXED-MEMORY ====================

(test =scaled-indexed-memory-succeeds
  (is-true (all-succeed (parser::=scaled-indexed-memory)
                        (list "$12(%rax,%rdx,1)" "(,%rdx,2)" "$12(,%rax,4)" "$0xab(,%rax,8)"))))

(test =scaled-indexed-memory-fails
  (is-true (all-fail (parser::=scaled-indexed-memory)
                     (list "(,%rax,3)" "(%rax ,%rdx,4)" "(,%rax,8"))))

(test =scaled-indexed-memory-production
  (is-true (equalp (parser::make-memory :offset 12 :base :RAX :index :RDX :scale 4)
                   (parse "$12(%rax,%rdx,4)" (parser::=scaled-indexed-memory)))))

;;; ==================== =MEMORY-OPERAND ====================

(test =memory-operand-succeeds
  (is-true (all-succeed (parser::=memory-operand)
                        (list "$12" "%rax" "$12(%rax)" "$12(%rax,%rdx)" "$12(%rax,%rdx,4)"))))

(test =memory-operand-fails
  (is-true (all-fail (parser::=memory-operand)
                     (list "%foo" "$$12" "(%rax , %rdx)" "(%rax,%rdx"))))

(test =memory-operand-production
  (is-true (equalp (list :operand1 (parser::make-memory :offset 12 :base :RAX :index :RDX :scale 4) :operand2 nil)
                   (parse "$12(%rax,%rdx,4)" (parser::=memory-operand)))))

;;; ==================== =SOURCE-LINE ====================

(test =source-line-succeeds
  (is-true (all-succeed (parser::=source-line)
                        (list "foo: halt #comment"
                              "halt"
                              "foo: addq %rax,%rdx#comment"
                              "addq %rax , %rdx"
                              "jmp $12(%rax,%rdx,8) #comment"
                              "foo: IRMOVQ $12,%RAX #comment"))))

(test =source-line-fails
  (is-true (all-fail (parser::=source-line)
                     (list "foo: %haltt"
                           "foo: andq #comment"
                           "je"))))

(test =source-line-production
  (is-true (equalp (parser::make-source-line :label "foo"
                                             :mnemonic :ANDQ
                                             :operand1 :RAX
                                             :operand2 :RDX
                                             :eol-comment "#comment")
                   (parse "foo: andq %rax,%rdx #comment"(parser::=source-line)))))
