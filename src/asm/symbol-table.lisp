;;;; This package exists to export the *SYMBOL-TABLE* special variable which
;;;; is a lexical closure that can be used to query and insert into the Y86-64
;;;; symbol table.

(defpackage symbol-table
  (:use #:cl)
  (:shadow #:symbol-name)
  (:export #:*symbol-table*
           #:symbol-name-p
           #:asm-symbol
           #:asm-symbol-type
           #:undefined-symbol
           #:duplicate-symbol))

(in-package #:symbol-table)

;;; ==================== Types ====================

(let ((regex (cl-ppcre:create-scanner "^[.a-zA-Z][-a-zA-Z0-9]*$")))
  (defun symbol-name-p (string)
    (cl-ppcre:scan regex string)))

(deftype asm-symbol ()
  "The type of a valid Z86-64 asm symbol name."
  '(satisfies symbol-name-p))

(deftype asm-symbol-type ()
  "The type for the type of a Z86-64 asm symbol."
  '(member :UNDEF :ABS :TEXT :DATA :BSS))

(u:defstruct-read-only entry
  "The type of a single symbol table entry."
  (name  nil :type asm-symbol)
  (type  nil :type asm-symbol-type)
  (value nil :type (unsigned-byte 64)))

;;; ==================== Conditions ====================

(define-condition undefined-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type asm-symbol)
   (table       :initarg :table       :reader table       :type hash-table))
  (:documentation "Condition signaled when trying to access a symbol that has not been defined.")
  (:report (lambda (c s)
             (with-slots (symbol-name)
                 c
               (format s "Undefined symbol error: \"~a\" is undefined."
                       symbol-name)))))

(define-condition duplicate-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type asm-symbol)
   (table       :initarg :table       :reader table       :type hash-table))
  (:documentation "Condition signaled when trying to redefine an existing symbol.")
  (:report (lambda (c s)
             (with-slots (symbol-name)
                 c
               (format s "Duplicate symbol error: \"~a\" is already defined."
                       symbol-name)))))

;;; ==================== Symbol Table Definition ====================

(setf (symbol-function 'symbol-table) 
      (let ((symbol-table (make-hash-table :test #'equal)))
        (lol:dlambda 
         (:entry-p (name)
                   ;; T iff NAME is the name of an entry in SYMBOL-TABLE and.
                   (nth-value 1 (gethash name symbol-table)))
         
         (:insert (name type value)
                  ;; Insert an entry into SYMBOL-TABLE with name NAME, type TYPE,
                  ;; and value VALUE. Signal a DUPLICATE-SYMBOL error if a symbol
                  ;; with NAME is already present in SYMBOL-TABLE.
                  (if (not (nth-value 1 (gethash name symbol-table)))
                      (setf (gethash name symbol-table)
                            (make-entry :name name
                                        :type type
                                        :value value))
                      (error 'duplicate-symbol :symbol-name name :table symbol-table)))

         (:symbol-value (name)
                        ;; Get the value of the symbol named NAME. If there is no
                        ;; symbol named NAME in SYMBOL-TABLE then signal an
                        ;; UNDEFINED-SYMBOL error.
                        (lol:aif (nth-value 1 (gethash name symbol-table))
                                 (entry-value it)
                                 (error 'undefined-symbol :symbol-name name :table symbol-table)))
         
         (:symbol-type (name)
                       ;; Get the type of the symbol named NAME. If there is no
                       ;; symbol named NAME in SYMBOL-TABLE then signal an
                       ;; UNDEFINED-SYMBOL error.
                       (lol:aif (nth-value 1 (gethash symbol-name symbol-table))
                                (entry-type it)
                                (error 'undefined-symbol :symbol-name it :table symbol-table)))

         (:clear-table ()
                       ;; Reset SYMBOL-TABLE to an empty hash.
                       (setf symbol-table (make-hash-table :test #'equal))))))

