(defpackage #:parser
  (:use #:cl #:maxpc)
  (:import-from #:Y8664-opcode-table #:*opcode-table*)
  (:import-from #:Y8664-register-table #:*register-table*)
  (:export #:labelp))

(in-package #:parser)

(deftype operand ()
  (or immediate memory register-table:register))

(defstruct source-line
  "Struct to carry all fields of a Y86-64 asm source line."
  (label nil       :type symbol                :read-only t)
  (mnemonic nil    :type opcode-table:mnemonic :read-only t)
  (operand1 nil    :type operand               :read-only t)
  (operand2 nil    :type operand               :read-only t)
  (eol-comment ""  :type string                :read-only t))

(defun label ()
  (=destructure (first-char rest-chars _)
                (=list
                 (=subseq (%or (?eq #\.) (?satisfies #'alpha-char-p)))
                 (=subseq (%some (%or (?eq #\-) (?satisfies #'alphanumericp))))
                 (?eq #\:))
    (str:concat first-char rest-chars)))

(defun mnemonic ()
  (=transform
   (=subseq (?satisfies
             (lambda (string)
               (funcall *opcode-table* :mnemonic-p string))
             (=subseq (%some (?satisfies #'alpha-char-p)))))
   #'str:upcase))

(defun register ()
  (=destructure (_ register)
                (=list
                 (?eq #\%)
                 (=subseq (?satisfies
                           (lambda (string)
                             (funcall *register-table* :register-name-p string))
                           (=subseq (%some (?satisfies #'alphanumericp))))))
    (str:upcase register)))

(defun immediate ()
  (=destructure (_ imm)
                (=list
                 (?eq #\$)
                 (maxpc.digit:=natural-number 10))
    imm))

(defun eol-comment ()
  (=destructure (pound comment)
                (=list (=subseq (?eq #\#))
                       (=subseq (%any (?not (maxpc.char:?newline)))))
    (str:concat pound comment)))

(defun null-arg-instruction ()
  (=destructure (label mn op1 op2 eol-comment)
                (list= (%maybe (label))
                       (mnemonic)
                       (immediate)
                       ())))

(defmacro )
