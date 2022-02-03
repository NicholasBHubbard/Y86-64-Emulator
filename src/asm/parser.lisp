(defpackage #:parser
  (:use #:cl #:maxpc)
  (:import-from #:opcode-table #:*opcode-table*)
  (:import-from #:register-table #:*register-table* #:register)
  (:export #:labelp))

(in-package #:parser)

;;; ==================== Types ====================

(deftype operand ()
  '(or (signed-byte 64) memory register))

(defun scalep (int)
  (member int '(1 2 4 8) :test #'=))

(u:defstruct-read-only memory
  (offset nil :type (signed-byte 64))
  (base   nil :type (or (signed-byte 64) register))
  (index  nil :type (or (signed-byte 64) register))
  (scale  1   :type (and (unsigned-byte 8) (satisfies scalep))))

(u:defstruct-read-only source-line
  "Struct to carry the fields of a Y86-64 asm source line."
  (label nil      :type symbol-table:symbol-name)
  (mnemonic nil   :type opcode-table:mnemonic)
  (operand1 nil   :type operand)
  (operand2 nil   :type operand)
  (eol-comment "" :type string))

;;; ==================== Parsers ====================

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

;; (defun null-arg-instruction ()
;;   (=destructure (label mn op1 op2 eol-comment)
;;                 (list= (%maybe (label))
;;                        (mnemonic)
;;                        (immediate)
;;                        ())))

