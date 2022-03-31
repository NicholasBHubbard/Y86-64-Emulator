;;;; Testing for the definitions in src/asm/parser.lisp

(defpackage parser-test
  (:use #:cl #:fiveam #:parser #:maxpc #:maxpc-extensions)
  (:export #:run-tests))

(in-package #:parser-test)

;;; ==================== Test Suite ====================

(def-suite parser-suite
  :description "Test suite for the asm systems PARSER package.")

(in-suite parser-suite)

(defun run-tests ()
  (run! 'parser-suite))

;;; ==================== Helpers ====================

(defun all-succeed (parser inputs)
  "Return T if all values in INPUTS parse totally under PARSER and return NIL
otherwise."
  (every
   (lambda (input)
     (let ((parser::*current-source-line* input)
           (parser::*current-line-number* 0))
       (handler-case (parse-total-success-p input parser)
         (parse-failure (pf) (declare (ignore pf)) nil))))
   inputs))

(defun all-fail (parser inputs)
  "Return T if all values in INPUTS fail to parse totally under PARSER and
return NIL otherwise."
  (notany (lambda (input)
            (let ((parser::*current-source-line* input)
                  (parser::*current-line-number* 0))
              (handler-case (parse-total-success-p input parser)
                (parse-failure (pf) (declare (ignore pf)) nil))))
          inputs))

(defmacro test-all-succeed (parser inputs)
  "TODO"
  (let ((test-name
          (read-from-string (str:concat (symbol-name parser) "-successes"))))
    `(test ,test-name
       (is-true (all-succeed (,parser) ,inputs)))))

(trivial-indent:define-indentation test-all-succeed
    (4 &body))

(defmacro test-all-fail (parser inputs)
  "TODO"
  (let ((test-name
          (read-from-string (str:concat (symbol-name parser) "-failures"))))
    `(test ,test-name
       (is-true (all-fail (,parser) ,inputs)))))

(trivial-indent:define-indentation test-all-fail
    (4 &body))

(defmacro test-produces (parser &body input-production-conses)
  "TODO"
  (let ((test-name
          (read-from-string (str:concat (symbol-name parser) "-production"))))
    `(test ,test-name
       (is-true (every (lambda (cons)
                         (let* ((input (car cons))
                                (production (cdr cons))
                                (parser::*current-source-line* input)
                                (parser::*current-line-number* 0))
                           (equalp production (parse input (,parser)))))
                       (list ,@input-production-conses))))))

;;; ==================== TESTS ====================

(test-all-succeed parser::=comment
  (list "#" "# " "#foo" "# foo"))

(test-all-fail parser::=comment
  (list "" " " " #foo" ";foo"))

(test-produces parser::=comment
  '("#foo" . "#foo"))

(test-all-succeed parser::=symbol-name
  (list "foo" ".foo" "foo-12" "FOO-12-BAR"))

(test-all-fail parser::=symbol-name
  (list "" " " "12foo" "foo@bar" "foo.bar"))

(test-produces parser::=symbol-name
  '("foo" . "foo"))

(test-all-succeed parser::=label
  (list "foo:" ".foo:" "foo-12:"))

(test-all-fail parser::=label
  (list "" " " "foo" ".foo" "foo-12"))

(test-produces parser::=label
  '("foo:" . "foo"))

(test-all-succeed parser::=mnemonic
  (opcode-table:opcode-table :all-mnemonic-strings))

(test-all-fail parser::=mnemonic
  (list "" " " "foo" "foohalt"))

(test-produces parser::=mnemonic
  '("halt" . :halt))

(test-all-succeed parser::=register
  (mapcar (lambda (reg) (str:concat "%" reg))
          (register-table:register-table :all-register-name-strings)))

(test-all-fail parser::=register
  (list "" " " "%foo" "rax"))

(test-produces parser::=register
  '("%rax" . :rax))

(test-all-succeed parser::=immediate
  (list "$12" "$-12" "$0xAB" "$0b1010" "$-0xAB" "$-0b1010" "$0"))

(test-all-fail parser::=immediate
  (list "" " " "12" "-12" "0xab" "0b1010"))

(test-produces parser::=immediate
  '("$-0xab" . -171))

(test-all-succeed parser::=register-operand
  (mapcar (lambda (reg) (str:concat "%" reg))
          (register-table:register-table :all-register-name-strings)))

(test-all-fail parser::=register-operand
  (list "" " " "%foo" "rax"))

(test-produces parser::=register-operand
  (cons "%rax" (list :operand1 :rax :operand2 nil)))

(test-all-succeed parser::=register-register-operands
  (list "%rax,%rdx" "%rax, %rdx" "%rax ,%rdx" "%rax , %rdx" "%rax  ,  %rdx"))

(test-all-fail parser::=register-register-operands
  (list "" " " "%rax %rdx" "%rax%rdx" "rax,rdx" "%foo,%bar"))

(test-produces parser::=register-register-operands
  (cons "%rax,%rdx" (list :operand1 :rax :operand2 :rdx)))

(test-all-succeed parser::=immediate-register-operands
  (list "$12,%rax" "$0xab, %rax" "$-0b1010 ,%rax" "$0 , %rax" "$0  ,  %rax"))

(test-all-fail parser::=immediate-register-operands
  (list "" " " "$12 %rax" "12,rax" "12,%rax" "$12,rax"))

(test-produces parser::=immediate-register-operands
  (cons "$12,%rax" (list :operand1 12 :operand2 :rax)))

(test-all-succeed parser::=absolute-memory
  (list "$12" "$-12" "$0xab" "$0b1010" "foo" "foo-bar" "foo-12-bar"))

(test-all-fail parser::=absolute-memory
  (list "" " " "12" "foo@bar" "1010"))

(test-produces parser::=absolute-memory
  (cons "$12" (parser::make-relative-address :offset 12))
  (cons "foo" (parser::make-relative-address :offset "foo")))

(test-all-succeed parser::=indirect-memory
  (list "(%rax)"))

(test-all-fail parser::=indirect-memory
  (list "" " " "%rax" "( %rax)" "( %rax)" "( %rax )" "(%foo)" "(foo)" "foo(%rax)"))

(test-produces parser::=indirect-memory
  (cons "(%rax)" (parser::make-relative-address :base :rax)))

(test-all-succeed parser::=base-displacement-memory
  (list "$12(%rax)" "$-12(%rdx)" "foo(%rdx)" "$-0b1010(%r11)"))

(test-all-fail parser::=base-displacement-memory
  (list "" " " "12(%rax)" "$12(rax)" "foo@bar(%rax)" "$12 (%rax)" "$12( %rax )"))

(test-produces parser::=base-displacement-memory
  (cons "$12(%rax)" (parser::make-relative-address :offset 12
                                                   :base :rax)))

(test-all-succeed parser::=indexed-memory
  (list "foo(%rax,%rdx)" "$12(%rax,%rdx)" "(%rax,%rdx)"))

(test-all-fail parser::=indexed-memory
  (list "" " ""foo (%rax,%rdx)" "( %rax , %rdx )" "(rax,rdx)"
        "foo@bar(%rax,%rdx)" "foo(%rax %rdx)" "foo(%rax , %rdx)"))

(test-produces parser::=indexed-memory
  (cons "foo(%rax,%rdx)" (parser::make-relative-address :offset "foo"
                                                        :base :rax
                                                        :index :rdx)))

(test-all-succeed parser::=scaled-indexed-memory
  (list "foo(,%rax,1)" "$12(,%rax,2)" "(,%rax,4)" "(%rax,%rdx,8)" "foo(%rax,%rdx,8)"))

(test-all-fail parser::=scaled-indexed-memory
  (list "" " " "foo(,%rax,12)" "foo(%rax,8)" "foo(%rax , %rdx , 8)" "foo (,%rax,1)"))

(test-produces parser::=scaled-indexed-memory
  (cons "foo(%rax,%rdx,8)" (parser::make-relative-address :offset "foo"
                                                          :base :rax
                                                          :index :rdx 
                                                          :scale 8))
  (cons "$12(,%rdx,2)" (parser::make-relative-address :offset 12
                                                      :index :rdx 
                                                      :scale 2))
  (cons "(,%rdx,1)" (parser::make-relative-address :index :rdx 
                                                   :scale 1)))
(test-all-succeed parser::=memory-operand
  (list "$12" "foo" "(%rax)" "$12(%rax)" "$12(%rax,%rax)" "$12(%rax,%rax,8)"))

(test-all-fail parser::=memory-operand
  (list "" " " "$12( %rax )" "($rax)"))

(test-produces parser::=memory-operand
  (cons "$12(%rax)" (list :operand1 (parser::make-relative-address :offset 12
                                                                   :base :rax)
                          :operand2 nil)))

(test-all-succeed parser::=instruction-source-line
  (list
   "halt"
   "jmp $12(%rax)"
   "pushq %rax"
   "addq %rax,%rdx #comment"
   "irmovq $12, %rax"
   "halt  #comment "
   "halt  "
   "pushq %rax   "))

(test-all-fail parser::=instruction-source-line
  (list "" " " "foo" "halt %rax" "jmp$12(%rax)" "jmp $12( %rax )" " halt"))

(test-produces parser::=instruction-source-line
  (cons "halt #foo"
        (parser::make-instruction :mnemonic :halt
                                  :operand1 nil
                                  :operand2 nil
                                  :comment "#foo"
                                  :source-line-metadata
                                  (parser::make-source-line-metadata
                                   :type :instruction
                                   :source-line "halt #foo"
                                   :line-number 0)))
  (cons "pushq %rax"
        (parser::make-instruction :mnemonic :pushq
                                  :operand1 :rax
                                  :operand2 nil
                                  :comment nil
                                  :source-line-metadata
                                  (parser::make-source-line-metadata
                                   :type :instruction
                                   :source-line "pushq %rax"
                                   :line-number 0)))
  (cons "jmp $12(%rax) #comment"
        (parser::make-instruction :mnemonic :jmp
                                  :operand1 (parser::make-relative-address :offset 12 :base :rax)
                                  :operand2 nil
                                  :comment "#comment"
                                  :source-line-metadata
                                  (parser::make-source-line-metadata
                                   :type :instruction
                                   :source-line "jmp $12(%rax) #comment"
                                   :line-number 0)))
  (cons "andq %rax,%rdx"
        (parser::make-instruction :mnemonic :andq
                                  :operand1 :rax
                                  :operand2 :rdx
                                  :comment nil
                                  :source-line-metadata
                                  (parser::make-source-line-metadata
                                   :type :instruction
                                   :source-line "andq %rax,%rdx"
                                   :line-number 0)))
  (cons "irmovq $12,%rax"
        (parser::make-instruction :mnemonic :irmovq
                                  :operand1 12
                                  :operand2 :rax
                                  :comment nil
                                  :source-line-metadata
                                  (parser::make-source-line-metadata
                                   :type :instruction
                                   :source-line "irmovq $12,%rax"
                                   :line-number 0))))

(test-all-succeed parser::=label-source-line
  (list "foo:" "foo: " ".foo:" "FOO:" "foo12:"))

(test-all-fail parser::=label-source-line
  (list "" " " "foo" "foo@bar:"))

(test-produces parser::=label-source-line
  (cons "foo: #comment" (parser::make-label
                         :symbol "foo"
                         :comment "#comment"
                         :source-line-metadata
                         (parser::make-source-line-metadata
                          :type :label
                          :source-line "foo: #comment"
                          :line-number 0))))

(test-all-succeed parser::=comment-source-line
  (list "#foo" "# foo" "## foo" "#"))

(test-all-fail parser::=comment-source-line
  (list "" " " "foo"))

(test-produces parser::=comment-source-line
  (cons "#foo" (parser::make-comment
                :comment "#foo"
                :source-line-metadata
                (parser::make-source-line-metadata
                 :type :comment
                 :source-line "#foo"
                 :line-number 0))))
