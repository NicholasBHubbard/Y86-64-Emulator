;;;; Provide functions involved in parsing the Y86-64 assembly language.

(defpackage #:parser
  (:use #:cl #:maxpc #:maxpc.char #:maxpc.digit)
  (:import-from #:opcode-table #:*opcode-table*)
  (:import-from #:register-table #:*register-table* #:register)
  (:local-nicknames (#:a #:alexandria))
  (:export #:parse-source-line
           #:parse-failure))

(in-package #:parser)

;;; ==================== Interface ====================

(defvar *source-line* nil)

(defun parse-source-line (input-string)
  (let ((*source-line* input-string))
    (parse input-string (=source-line))))

;;; ==================== Parse Failure Condition ====================

(define-condition parse-failure (error)
  ((input-string  :initarg :input-string  :reader input-string  :type string)
   (fail-position :initarg :fail-position :reader fail-position :type integer)
   (expected      :initarg :expected      :reader expected      :type string))
  (:documentation "Condition signaled upon a parse failure.")
  (:report (lambda (c s)
             (with-slots (input-string fail-position expected)
                 c
               (format s "Parse failure at:~%~vtv~% ~a~%Expected: ~a"
                       fail-position input-string expected)))))

;;; ==================== Types ====================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun scalep (int)
    (member int '(1 2 4 8) :test #'=)))

(u:defstruct-read-only memory
  "Type of a Y86-64 memory operand."
  (offset 0 :type (signed-byte 64))
  (base :NOREG :type register)
  (index :NOREG :type register)
  (scale  1 :type (and (unsigned-byte 8) (satisfies scalep))))

(u:defstruct-read-only source-line
  "Struct to carry the fields of a Y86-64 asm source line."
  (label       (error "Must provide :LABEL"))
  (mnemonic    (error "Must provide :MNEMONIC"))
  (operand1    (error "Must provide :OPERAND1"))
  (operand2    (error "Must provide :OPERAND2"))
  (eol-comment (error "Must provide :EOL-COMMENT")))

;;; ==================== Custom Parsers ====================

(defmacro %or-fail (expected parser)
  "If PARSER succeeds return it's output, otherwise signal a PARSE-FAILURE
condition with an :EXPECTED field of EXPECTED."
  `(%or ,parser (?fail
                  (error (make-instance 'parse-failure
                                        :input-string *source-line*
                                        :fail-position (get-input-position)
                                        :expected ,expected)))))

(defun %bind (parser make-parser)
  "Monadic bind function."
  (lambda (input)
    (multiple-value-bind (rest value) (funcall parser input)
      (when rest
        (funcall (funcall make-parser value) rest)))))

(defmacro %let* (bindings &body body)
  "Convenience macro around chaining together %BIND calls."
  (if (null bindings)
      (let ((input (gensym "input")))
        `(lambda (,input) (values ,input (progn ,@body))))
      (let ((var (first (first bindings)))
            (parser (second (first bindings))))
        `(%bind ,parser
                (lambda (,var)
                  ,(when (string= var "_")
                     `(declare (ignore ,var)))
                  (%let* ,(rest bindings) ,@body))))))

(defun ?null ()
  "Always succeed and return NIL without consuming any input."
  (%maybe (?satisfies (u:const nil))))

(defmacro =eq (x &optional (parser '(=element)))
  "Like ?EQ but return the matching input."
  `(=subseq (?eq ,x ,parser)))

(defmacro =not (parser)
  "Like ?NOT but return the matching input."
  `(=subseq (?not ,parser)))

(defmacro =satisfies (test &optional (parser '(=element)))
  "Like ?SATISFIES but return the matching input."
  `(=subseq (?satisfies ,test ,parser)))

(defmacro =as-keyword (parser)
  "Transform the result of PARSER into a keyword."
  `(=transform ,parser #'u:make-keyword))

(defmacro %some-string (parser)
  "Match PARSER is sequence one or more times and concatenate the results as a
string."
  `(=transform (%some ,parser) (a:curry #'format nil "~{~a~}")))

(defmacro %any-string (parser)
  "Match PARSER is sequence one or more times and concatenate the results as a
string."
  `(=transform (%any ,parser) (a:curry #'format nil "~{~a~}")))

(defmacro =prog1 (&rest parsers)
  "Parse PARSERS in sequence"
  `(=transform (=list ,@parsers) #'first))

(defmacro =progn (&rest parsers)
  "Parse PARSERS in sequence and return the result of the final parser."
  `(=transform (=list ,@parsers) #'a:last-elt))

(defun ?space-or-tab ()
  "Parse either a space or tab character."
  (let ((*whitespace* '(#\  #\Tab)))
    (?whitespace)))

(defun ?space-or-tab-or-newline ()
  "Parse either a space, tab, or newline character."
  (let ((*whitespace* '(#\  #\Tab #\Newline)))
    (?whitespace)))

;;; ==================== Basic Grammar Elements ====================

(defun =symbol-name ()
  "Parser for symbol names."
  (=satisfies #'symbol-table:symbol-name-p
      (%some-string (%or (=eq #\.)
                         (=eq #\-)
                         (=satisfies #'alphanumericp)))))

(defun =label ()
  "Parser for labels. Labels are symbol names followed by a colon."
  (=prog1 (=symbol-name) (?eq #\:)))

(defun =mnemonic ()
  "Parse a Y86-64 mnemonic."
  (=as-keyword
   (=satisfies
       (a:curry #'funcall *opcode-table* :mnemonic-p)
       (%some-string (=satisfies #'alpha-char-p)))))

(defun =register ()
  "Parser for register operands."
  (=as-keyword
   (=progn (?eq #\%)
           (=satisfies
               (a:curry #'funcall *register-table* :register-name-p)
               (%some-string (=satisfies #'alphanumericp))))))

(defun =immediate ()
  "Parser for immediate operands."
  (%let* ((_ (?eq #\$))
          (negative (%maybe (=eq #\-)))
          (num (%or (=progn (?string "0x") (=natural-number 16))
                    (=progn (?string "0b") (=natural-number 2))
                    (=natural-number 10))))
    (if negative (- num) num)))

(defun =eol-comment ()
  "Parser for end of line comments starting with a #\# character."
  (%let* ((pound (=eq #\#))
          (comment (%any-string (=not (?eq #\Newline)))))
    (concatenate 'string pound comment)))

;;; ==================== Source line Parser ====================

(defun =source-line ()
  "Parse a source line and on success return a SOURCE-LINE struct and on failure
signal a PARSE-FAILURE condition."
  (%let* ((label (%maybe (=label)))
          (_ (if label
                 (%or-fail "Whitespace" (%some (?space-or-tab-or-newline)))
                 (?null)))
          (mnemonic (%or-fail "Mnemonic" (=mnemonic)))
          (_ (%any (?space-or-tab)))
          (operands (case (funcall *opcode-table* :mnemonic-type mnemonic)
                      (:N  (?null))
                      (:R  (%or-fail "Register operand" (=register-operand)))
                      (:M  (%or-fail "Memory operand" (=memory-operand)))
                      (:RR (%or-fail "Register,Register operands" (=register-register-operands)))
                      (:IR (%or-fail "Immediate,Register operands" (=immediate-register-operands)))))
          (_ (%any (?space-or-tab)))
          (eol-comment (%maybe (=eol-comment)))
          (_ (%or-fail "End of source line" (?end))))
    (make-source-line :label label
                      :mnemonic mnemonic
                      :operand1 (getf operands :operand1)
                      :operand2 (getf operands :operand2)
                      :eol-comment eol-comment)))

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
  "Parse an absolute memory address into a MEMORY struct."
  (=transform (=immediate)
              (lambda (imm) (make-memory :offset imm))))

(defun =indirect-memory ()
  "Parse an indirect memory address into a MEMORY struct."
  (=transform (=register)
              (lambda (reg) (make-memory :base reg))))

(defun =base-displacement-memory ()
  "Parse a base+displacement memory address into a MEMORY struct."
  (%let* ((offset (=immediate))
          (_ (?eq #\())
          (base (=register))
          (_ (?eq #\))))
    (make-memory :offset offset :base base)))

(defun =indexed-memory ()
  "Parse an indirect memory address into a MEMORY struct."
  (%let* ((offset (%maybe (=immediate)))
          (_ (?eq #\())
          (base (=register))
          (_ (?eq #\,))
          (index (=register))
          (_ (?eq #\))))
    (make-memory :offset (or offset 0) :base base :index index)))

(defun =scaled-indexed-memory ()
  "Parse a scaled indexed memory address into a MEMORY struct."
  (%let* ((offset (%maybe (=immediate)))
          (_ (?eq #\())
          (base (%maybe (=register)))
          (_ (?eq #\,))
          (index (=register))
          (_ (?eq #\,))
          (scale (%or (=eq #\1) (=eq #\2) (=eq #\4) (=eq #\8)))
          (_ (?eq #\))))
    (make-memory :offset (or offset 0)
                 :base (or base :NOREG)
                 :index index
                 :scale (parse-integer scale))))
