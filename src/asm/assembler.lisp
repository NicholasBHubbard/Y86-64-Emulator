(defpackage #:assembler
  (:use #:cl #:symbol-table #:opcode-table #:register-table #:parser)
  (:export #:assemble-file))

(in-package #:assembler)

;;; ----------------------------------------------------

(defun assemble-file (file)
  "TODO"
  (unwind-protect
       (let ((source-lines (parse-asm-file file)))
         (symbol-table :clear-table)
         (do-first-pass source-lines)
         (do-second-pass source-lines))
    (symbol-table :clear-table)))

;;; ----------------------------------------------------

(defun do-first-pass (source-lines)
  "TODO"
  (let ((lc 0))
    (loop :for line :in source-lines
          :do (typecase line
                (label (symbol-table :insert (label-symbol line) :undef lc))
                (instruction (let* ((mnemonic (instruction-mnemonic line))
                                    (size (opcode-table :mnemonic-size mnemonic)))
                               (setf lc (+ lc size))))))))

;;; ----------------------------------------------------

(defun do-second-pass (source-lines)
  "TODO"
  (apply #'str:concat
         (loop :for line :in source-lines
               :when (typep line 'instruction)
                 :collect (assemble-instruction line))))

;;; ----------------------------------------------------

(defun assemble-instruction (instruction)
  "Assmemble INSTRUCTION into a hexadecimal string representing the machine
level instruction."
  (let ((mnemonic (instruction-mnemonic instruction))
        (operand1 (instruction-operand1 instruction))
        (operand2 (instruction-operand2 instruction)))
    (case (opcode-table :mnemonic-type mnemonic)
      (:N  (assemble-mnemonic mnemonic))
      
      (:R  (str:concat (assemble-mnemonic mnemonic)
                       (assemble-register operand1)))
      
      (:M  (str:concat (assemble-mnemonic mnemonic)
                       (assemble-memory operand1)))
      
      (:IR (str:concat (assemble-mnemonic mnemonic)
                       (assemble-immediate operand1)
                       (assemble-register operand2)))
      
      (:RR (str:concat (assemble-mnemonic mnemonic)
                       (assemble-register operand1 operand2)))

      (otherwise (error 'u:internal-error :reason "Unreachable code.")))))

;;; ----------------------------------------------------

(defun assemble-mnemonic (mn)
  "TODO"
  (format nil "~2,'0x" (opcode-table :mnemonic-opcode mn)))

;;; ----------------------------------------------------

(defun assemble-immediate (imm)
  "TODO"
  (format nil "~16,'0x" imm))

;;; ----------------------------------------------------

(defun assemble-register (reg1 &optional reg2)
  "TODO"
  (let ((reg1-id (register-table :register-name-id reg1))
        (reg2-id (if reg2
                     (register-table :register-name-id reg2)
                     (register-table :register-name-id :noreg))))
    (str:concat (format nil "~1,'0x" reg1-id) (format nil "~1,'0x" reg2-id))))

;;; ----------------------------------------------------

(defun assemble-memory (memory)
  "TODO"
  (let* ((offset (relative-address-offset memory))
         (offset* (if (symbol-name-p offset)
                      (symbol-table :symbol-value offset)
                      offset))
         (base (relative-address-base memory))
         (index (relative-address-index memory))
         (scale (relative-address-scale memory)))
    (str:concat (assemble-immediate offset*)
                (assemble-register base index)
                (format nil "~2,'0x" scale))))
