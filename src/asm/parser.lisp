;;;; Z86-64 assembly language parsing.

(defpackage #:parser
  (:use #:cl
        #:maxpc #:maxpc-extensions #:maxpc.char #:maxpc.digit
        #:symbol-table
        #:opcode-table
        #:register-table) 
  (:local-nicknames (#:a #:alexandria))
  (:export #:parse-asm-file
           #:parse-failure
           #:source-line
           #:source-line-type
           #:source-line-line-number
           #:instruction
           #:instruction-mnemonic
           #:instruction-operand1
           #:instruction-operand2
           #:instruction-comment
           #:label
           #:label-symbol
           #:comment
           #:comment-comment
           #:relative-address
           #:relative-address-offset
           #:relative-address-base
           #:relative-address-index
           #:relative-address-scale))

(in-package #:parser)

;;; ----------------------------------------------------

(defparameter *current-source-line* nil)

(defparameter *current-line-number* nil)

;;; ----------------------------------------------------

(defun parse-asm-file (file-or-stream)
  "Parse FILE-OR-STREAM into a list of SOURCE-LINE's."
  (flet ((parse-file (stream)
           (loop :for source-line = (str:trim (read-line stream nil))
                 :for line-number :from 1
                 :while source-line
                 :collect (let ((*current-source-line* source-line)
                                (*current-line-number* line-number))
                            (handler-case
                                (parse source-line
                                       (case (determine-source-line-type source-line)
                                         (:instruction (=instruction-source-line))
                                         (:label       (=label-source-line))
                                         (:comment     (=comment-source-line))
                                         (:blank       (?blank-line))
                                         (t (error 'u:internal-error :reason "Unreachable code"))))
                              (parse-failure (pf) (progn
                                                    (report-parse-failure pf)
                                                    (return-from parse-asm-file nil))))))))
    (etypecase file-or-stream
      (string (with-open-file (stream file-or-stream) (parse-file stream)))
      (stream (parse-file file-or-stream)))))

;;; ----------------------------------------------------

(define-condition parse-failure (error)
  ((input-string  :initarg :input-string  :reader parse-failure-input-string  :type string)
   (fail-position :initarg :fail-position :reader parse-failure-fail-position :type integer)
   (line-number   :initarg :line-number   :reader parse-failure-line-number   :type integer)
   (expected      :initarg :expected      :reader parse-failure-expected      :type string))
  (:documentation "Condition signaled upon a parse failure.")
  (:report (lambda (c s) (report-parse-failure c s))))

(defmethod report-parse-failure ((pf parse-failure) &optional stream)
  "Print parse failure diagnostic to STREAM. If STREAM is null print to stderr."
  (with-slots (input-string fail-position line-number expected)
      pf
    (format (or stream *error-output*)
            "Parse failure on line ~a:~%  ~a~%~vt ^~%Expected: ~a"
            line-number input-string fail-position expected)))

;;; ----------------------------------------------------

(deftype source-line-type ()
  "There are five different Z86-64 asm source line types.
:INSTRUCTION, :LABEL, :DIRECTIVE, :COMMENT, or :BLANK."
  '(member :INSTRUCTION :LABEL :DIRECTIVE :COMMENT :BLANK))

(deftype operand ()
  "A Z86-64 operand can be a register, immediate, symbol, or relative address."
  '(or register immediate relative-address))

(deftype immediate ()
  "A Z86-64 immediate is a signed 64 bit integer."
  '(signed-byte 64))

(deftype memory-scale ()
  '(and (signed-byte 8) (member 1 2 4 8)))

(u:defstruct-read-only relative-address
  "Type of a Z86-64 relative address operand."
  (offset 0      :type (or immediate asm-symbol))
  (base   :NOREG :type register)
  (index  :NOREG :type register)
  (scale  1      :type memory-scale))

;;; ----------------------------------------------------

(defclass source-line ()
  ((type        :initarg :type        :reader source-line-type        :type source-line-type)
   (line-number :initarg :line-number :reader source-line-line-number :type (unsigned-byte 64)))
  (:documentation "Base class for Z86-64 asm source lines."))

(defclass instruction (source-line)
  ((mnemonic :initarg :mnemonic :reader instruction-mnemonic :type mnemonic)
   (operand1 :initarg :operand1 :reader instruction-operand1 :type (or null operand))
   (operand2 :initarg :operand2 :reader instruction-operand2 :type (or null operand))
   (comment  :initarg :comment  :reader instruction-comment  :type (or null string)))
  (:documentation "A source line for a Z86-64 asm instruction."))

(defun make-instruction (&key line-number mnemonic operand1 operand2 comment)
  (check-type line-number (unsigned-byte 64))
  (check-type mnemonic    mnemonic)
  (check-type operand1    (or null operand))
  (check-type operand2    (or null operand))
  (check-type comment     (or null string))
  (make-instance 'instruction :type :instruction
                              :line-number line-number
                              :mnemonic mnemonic
                              :operand1 operand1
                              :operand2 operand2
                              :comment comment))

(defclass label (source-line)
  ((symbol  :initarg :symbol  :reader label-symbol  :type asm-symbol)
   (comment :initarg :comment :reader label-comment :type (or null string)))
  (:documentation "A source line for a Z86-64 label."))

(defun make-label (&key line-number symbol comment)
  (check-type line-number (unsigned-byte 64))
  (check-type symbol      asm-symbol)
  (check-type comment     (or null string))
  (make-instance 'label :type :label
                        :line-number line-number
                        :symbol symbol
                        :comment comment))

(defclass comment (source-line)
  ((comment :initarg :comment :reader comment-comment :type string))
  (:documentation "A comment source line."))

(defun make-comment (&key line-number comment)
  (check-type line-number (unsigned-byte 64))
  (check-type comment     string)
  (make-instance 'label :type :comment
                        :line-number line-number
                        :comment comment))

;;; ----------------------------------------------------

(defmacro %or-fail (expected parser)
  "If PARSER succeeds return it's output, otherwise signal a PARSE-FAILURE
condition with an :EXPECTED field of EXPECTED."
  `(%or ,parser (?fail (error 'parse-failure
                              :input-string *current-source-line*
                              :fail-position (get-input-position)
                              :line-number *current-line-number*
                              :expected ,expected))))

(defun ?space-or-tab ()
  "Parser for either a space or tab character."
  (let ((*whitespace* '(#\Tab #\ )))
    (?whitespace)))

(defun ?blank-line ()
  "Parse a line that only contains space and tab characters."
  (?seq (%any (?space-or-tab)) (?end)))

(defun =comment ()
  "Parser for end of line comments starting with a #\# character."
  (%let* ((pound (=eq #\#))
          (comment (=any (=not (?eq #\Newline)))))
    (concatenate 'string pound comment)))

;;; ----------------------------------------------------

(defun determine-source-line-type (source-line)
  "Return a SOURCE-LINE-TYPE keyword that denotes the type of SOURCE-LINE. If
SOURCE-LINE is erroneous signal a PARSE-FAILURE condition."
  (cond
    ((parse-success-p source-line (=mnemonic))   :INSTRUCTION)
    ((parse-success-p source-line (=label))      :LABEL)
    ((parse-success-p source-line (=comment))    :COMMENT)
    ((parse-success-p source-line (?blank-line)) :BLANK)
    (t (parse source-line (?fail (error 'parse-failure
                                        :input-string source-line
                                        :fail-position (get-input-position)
                                        :line-number *current-line-number*
                                        ::expected "Mnemonic, label, directive, or comment"))))))

(defun =instruction-source-line ()
  "Parser for an instruction source line. On success return an
INSTRUCTION-SOURCE-LINE struct and on failure signal a PARSE-FAILURE condition."
  (%let* ((mnemonic (%or-fail "Mnemonic" (=mnemonic)))
          (_ (%some (?space-or-tab)))
          (operands (case (opcode-table :mnemonic-type mnemonic)
                      (:N  (?null))
                      (:R  (%or-fail "Register operand" (=register-operand)))
                      (:M  (%or-fail "Memory operand" (=memory-operand)))
                      (:RR (%or-fail "Register,Register operands" (=register-register-operands)))
                      (:IR (%or-fail "Immediate,Register operands" (=immediate-register-operands)))))
          (_ (%any (?space-or-tab)))
          (comment (%maybe (=comment)))
          (_ (%or-fail "End of source line" (?end))))
    (make-instruction :mnemonic mnemonic
                      :operand1 (getf operands :operand1)
                      :operand2 (getf operands :operand2)
                      :comment comment
                      :line-number *current-line-number*)))

(defun =label-source-line ()
  "Parser for a label source line. On success return a LABEL-SOURCE-LINE and on
failure signal a PARSE-FAILURE condition."
  (%let* ((symbol (%or-fail "Label" (%prog1 (=symbol-name) (?eq #\:))))
          (_ (%any (?space-or-tab)))
          (comment (%maybe (=comment)))
          (_ (%or-fail "End of source line" (?end))))
    (make-label :symbol symbol
                :comment comment
                :line-number *current-line-number*)))

(defun =comment-source-line ()
  "Parser of a comment source line. On success return a COMMENT-SOURCE-LINE and
on failure signal a PARSE-FAILURE condition."
  (%let* ((comment (%or-fail "Comment" (=comment)))
          (_ (%and (?space-or-tab)))
          (_ (%or-fail "End of source line" (?end))))
    (make-comment :comment comment
                  :line-number *current-line-number*)))

;;; ----------------------------------------------------

(defun =symbol-name ()
  "Parser for Z86-64 symbol names."
  (=satisfies #'symbol-table:symbol-name-p
              (=some (%or (=eq #\.)
                          (=eq #\-)
                          (=satisfies #'alphanumericp)))))

(defun =label ()
  "Parser for a Z86-64 asm label."
  (%prog1 (=symbol-name) (?eq #\:)))

(defun =mnemonic ()
  "Parse a Y86-64 mnemonic."
  (=as-keyword
   (=satisfies
    (a:curry #'opcode-table :mnemonic-p)
    (=some (=satisfies #'alpha-char-p)))))

(defun =register ()
  "Parser for register operands."
  (=as-keyword
   (%progn (?eq #\%)
           (=satisfies
            (a:curry #'register-table :register-name-p)
            (=some (=satisfies #'alphanumericp))))))

(defun =immediate ()
  "Parser for immediate operands."
  (%let* ((_ (?eq #\$))
          (negative (%maybe (=eq #\-)))
          (num (%or (%progn (?string "0x") (=natural-number 16))
                    (%progn (?string "0b") (=natural-number 2))
                    (=natural-number 10))))
    (if negative (- num) num)))

(defun =register-operand ()
  "Parse the operand for an instruction of type :R."
  (=transform
   (%or-fail "Register" (=register))
   (lambda (reg) (list :operand1 reg :operand2 nil))))

(defun =register-register-operands ()
  "Parse the operands for an instruction of type :RR."
  (%let* ((src-reg (%or-fail "Register" (=register)))
          (_       (%any (?space-or-tab)))
          (_       (%or-fail "Comma" (?eq #\,)))
          (_       (%any (?space-or-tab)))
          (dst-reg (%or-fail "Register" (=register))))
    (list :operand1 src-reg :operand2 dst-reg)))

(defun =immediate-register-operands ()
  "Parse the operands for an instruction of type :IR."
  (%let* ((imm     (%or-fail "Immediate" (=immediate)))
          (_       (%any (?space-or-tab)))
          (_       (%or-fail "Comma" (?eq #\,)))
          (_       (%any (?space-or-tab)))
          (dst-reg (%or-fail "Register" (=register))))
    (list :operand1 imm :operand2 dst-reg)))

(defun =memory-operand ()
  "Parse the operand for an instruction of type :M"
  (=transform (%or (=scaled-indexed-memory)
                   (=indexed-memory)
                   (=base-displacement-memory)
                   (=absolute-memory)
                   (=indirect-memory))
              (lambda (mem) (list :operand1 mem :operand2 nil))))

(defun =absolute-memory ()
  "Parse an absolute memory address into a RELATIVE-ADDRESS struct."
  (=transform (%or (=symbol-name) (=immediate))
              (lambda (imm) (make-relative-address :offset imm))))

(defun =indirect-memory ()
  "Parse an indirect memory address into a RELATIVE-ADDRESS struct."
  (%let* ((_   (?eq #\())
          (reg (=register))
          (_   (?eq #\))))
    (make-relative-address :base reg)))

(defun =base-displacement-memory ()
  "Parse a base+displacement memory address into a RELATIVE-ADDRESS struct."
  (%let* ((offset (%or (=symbol-name) (=immediate)))
          (_ (?eq #\())
          (base (=register))
          (_ (?eq #\))))
    (make-relative-address :offset offset :base base)))

(defun =indexed-memory ()
  "Parse an indirect memory address into a RELATIVE-ADDRESS struct."
  (%let* ((offset (%maybe (%or (=symbol-name) (=immediate))))
          (_ (?eq #\())
          (base (=register))
          (_ (?eq #\,))
          (index (=register))
          (_ (?eq #\))))
    (make-relative-address :offset (or offset 0) :base base :index index)))

(defun =scaled-indexed-memory ()
  "Parse a scaled indexed memory address into a RELATIVE-ADDRESS struct."
  (%let* ((offset (%maybe (%or (=symbol-name) (=immediate))))
          (_ (?eq #\())
          (base (%maybe (=register)))
          (_ (?eq #\,))
          (index (=register))
          (_ (?eq #\,))
          (scale (%or (=eq #\1) (=eq #\2) (=eq #\4) (=eq #\8)))
          (_ (?eq #\))))
    (make-relative-address :offset (or offset 0)
                           :base (or base :NOREG)
                           :index index
                           :scale (parse-integer scale))))
