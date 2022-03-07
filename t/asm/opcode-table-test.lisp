(defpackage opcode-table-test
  (:export #:run-tests)
  (:use #:cl #:fiveam #:opcode-table)
  (:import-from #:alexandria
                #:compose
                #:curry))

(in-package #:opcode-table-test)

(def-suite opcode-table-suite
  :description "Test suite for the asm systems OPCODE-TABLE package.")

(in-suite opcode-table-suite)

(defun run-tests ()
  (run! 'opcode-table-suite))

;;; Define special variables to help avoid writing boilerplate code in the
;;; actual tests.

(defparameter *opcodes* (list #x00 #x10 #x20 #x30 #x40 #x50 #x60 #x61 #x62 #x63 #x70 #x71 #x72 #x73 #x74 #x75 #x76 #x21 #x22 #x23 #x24 #x25 #x26 #x80 #x90 #xA0))

(defparameter *opcode-strings* (mapcar (curry #'format nil "~x") *opcodes*))

(defparameter *mnemonics* (list :HALT :NOP :RRMOVQ :IRMOVQ :RMMOVQ :MRMOVQ :ADDQ :SUBQ :ANDQ :XORQ :JMP :JLE :JL :JNE :JE :JGE :JG :CMOVLE :CMOVL :CMOVE :CMOVNE :CMOVGE :CMOVG :CALL :RET :PUSHQ))

(defparameter *mnemonic-strings* (mapcar #'symbol-name *mnemonics*))

(defparameter *types* (list :N :IR :RR :M :R))

(defparameter *type-strings* (mapcar #'symbol-name *types*))

;;; Tests

(test pass-input-as-string
      (is-true (and (opcode-table :mnemonic-p :HALT)
                    (opcode-table :mnemonic-p "HALT"))))

(test mnemonic-p-trues
      (is-true (every (curry #'opcode-table :mnemonic-p)
                      (append *mnemonics* *mnemonic-strings*))))

(test mnemonic-p-falses
  (is-true (notany (curry #'opcode-table :mnemonic-p)
                   (list "" " " "foo" :foo))))

(test opcode-p-trues
  (is-true (every (curry #'opcode-table :opcode-p) *opcodes*)))

(test opcode-p-falses
  (is-true (notany (curry #'opcode-table :opcode-p)
                   (list 256 -1))))

(test all-opcodes
  (is-true (equal (opcode-table :all-opcodes)
                  (sort (copy-seq *opcodes*) #'<))))

(test all-mnemonics
  (is-true (equal (opcode-table :all-mnemonics)
                  (sort (copy-seq *mnemonics*) #'string<))))

(test all-types
  (is-true (equal (opcode-table :all-types)
                  (sort (copy-seq *types*) #'string<))))

(test all-opcode-strings
  (is-true (equal (opcode-table :all-opcode-strings)
                  (sort (copy-seq *opcode-strings*) #'string<))))

(test all-mnemonic-strings
  (is-true (equal (opcode-table :all-mnemonic-strings)
                  (sort (copy-seq *mnemonic-strings*) #'string<))))

(test all-type-strings
  (is-true (equal (opcode-table :all-type-strings)
                  (sort (copy-seq *type-strings*) #'string<))))

(test opcode-mnemonic
  (is-true (eql :CMOVE (opcode-table :opcode-mnemonic #x23))))

(test opcode-type
  (is-true (eql :RR (opcode-table :opcode-type #x20))))

(test opcode-size
  (is-true (= 2 (opcode-table :opcode-size #xA0))))

(test mnemonic-opcode
  (is-true (= #x60 (opcode-table :mnemonic-opcode :ADDQ))))

(test mnemonic-type
  (is-true (eql :N (opcode-table :mnemonic-type :HALT))))

(test mnemonic-size
  (is-true (= 10 (opcode-table :mnemonic-size :MRMOVQ))))

(test opcode-mnemonic-string
  (is-true (string= "ANDQ" (opcode-table :opcode-mnemonic-string #x62))))

(test opcode-type-string
  (is-true (string= "M" (opcode-table :opcode-type-string #x70))))

(test mnemonic-opcode-string
  (is-true (string= "76" (opcode-table :mnemonic-opcode-string :JG))))

(test mnemonic-type-string
  (is-true (string= "RR" (opcode-table :mnemonic-type-string :CMOVNE))))

(test opcode-mnemonic-match-p-true
  (is-true (opcode-table :opcode-mnemonic-match-p #x10 :NOP)))

(test opcode-mnemonic-match-p-false
  (is-false (opcode-table :opcode-mnemonic-match-p #x10 :HALT)))

(test opcode-type-match-p-true
  (is-true (opcode-table :opcode-type-match-p #x21 :RR)))

(test opcode-type-match-p-false
  (is-false (opcode-table :opcode-type-match-p #x21 :N)))

(test opcode-size-match-p-true
  (is-true (opcode-table :opcode-size-match-p #x40 10)))

(test opcode-size-match-p-false
  (is-false (opcode-table :opcode-size-match-p #x40 1)))

(test mnemonic-type-match-p-true
  (is-true (opcode-table :mnemonic-type-match-p :HALT :N)))

(test mnemonic-type-match-p-false
  (is-false (opcode-table :mnemonic-type-match-p :HALT :RR)))

(test mnemonic-size-match-p-true
  (is-true (opcode-table :mnemonic-size-match-p :PUSHQ 2)))

(test mnemonic-size-match-p-false
  (is-false (opcode-table :mnemonic-size-match-p :PUSHQ 1)))

(test type-mnemonics
  (is-true (equal (opcode-table :type-mnemonics :N)
                  (sort (list :HALT :NOP :RET) #'string<))))

(test type-opcodes
  (is-true (equal (opcode-table :type-opcodes :IR)
                  (sort (list #x30) #'<))))

(test type-mnemonic-strings
  (is-true (equal (opcode-table :type-mnemonic-strings :N)
                  (sort (list "HALT" "NOP" "RET") #'string<))))

(test type-opcode-strings
  (is-true (equal (opcode-table :type-opcode-strings :IR)
                  (sort (list "30") #'string<))))

(test size-opcodes
  (is-true (equal (opcode-table :size-opcodes 10)
                  (sort (list #x30 #x40 #x50) #'<))))

(test size-mnemonics
  (is-true (equal (opcode-table :size-mnemonics 1)
                  (sort (list :HALT :NOP :RET) #'string<))))

(test size-opcode-strings
  (is-true (equal (opcode-table :size-opcode-strings 2)
                  (sort (list "20" "60" "61" "62" "63" "21" "22" "23" "24" "25" "26" "A0") #'string<))))

(test size-mnemonic-strings
  (is-true (equal (opcode-table :size-mnemonic-strings 10)
                  (sort (list "IRMOVQ" "RMMOVQ" "MRMOVQ") #'string<))))
