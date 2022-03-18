;;;; This package exists to export the *SYMBOL-TABLE* special variable which
;;;; is a lexical closure that can be used to query and insert into the Y86-64
;;;; symbol table.

(defpackage symbol-table
  (:use #:cl)
  (:shadow #:symbol-name)
  (:export #:symbol-table
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

;;; ==================== Error Conditions ====================

(define-condition undefined-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type asm-symbol)
   (table       :initarg :table       :reader table       :type hash-table))
  (:documentation "Condition signaled when attempting to access a symbol that has not been defined.")
  (:report (lambda (c s)
             (with-slots (symbol-name)
                 c
               (format s "The symbol \"~a\" is undefined." symbol-name)))))

(define-condition duplicate-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type asm-symbol)
   (table       :initarg :table       :reader table       :type hash-table))
  (:documentation "Condition signaled when attempting to redefine an existing symbol.")
  (:report (lambda (c s)
             (with-slots (symbol-name)
                 c
               (format s "The symbol \"~a\" is already defined." symbol-name)))))

;;; ==================== Symbol Table Definition ====================

(u:defclosure symbol-table
  "Lexical closure over the Z86-64 symbol table. This closure uses LOL:DLAMBDA
to provide various function keywords for dynamically dispatching a query or
mutation function to the symbol table. 

Documentation for all the provided dispatch keywords:

:entry-p name  
  Return T if NAME is the name of an entry in the symbol table and return NIL
  otherwise.

:insert name type value
  Insert a symbol named NAME with type TYPE and value VALUE into the symbol
  table. If there already exists a symbol named NAME in the symbol table then
  signal a DUPLICATE-SYMBOL error condition.

:symbol-value name
  Return the value of the symbol named NAME in the symbol table. If there is no
  symbol named NAME then signal an UNDEFINED-SYMBOL error condition.

:symbol-type name
  Return the type of the symbol named NAME in the symbol table. If this is no
  symbol named NAME then signal an UNDEFINED-SYMBOL error condition.

:clear-table
  Remove all entries from the symbol table."
  (let ((symbol-table (make-hash-table :test #'equal)))
    (lol:dlambda 
      (:entry-p (name)
        (nth-value 1 (gethash name symbol-table)))
      
      (:insert (name type value)
        (if (not (nth-value 1 (gethash name symbol-table)))
            (setf (gethash name symbol-table)
                  (make-entry :name name
                              :type type
                              :value value))
            (error 'duplicate-symbol :symbol-name name :table symbol-table)))

      (:symbol-value (name)
        (lol:aif (gethash name symbol-table)
                 (entry-value lol:it)
                 (error 'undefined-symbol :symbol-name name :table symbol-table)))
      
      (:symbol-type (name)
        (lol:aif (gethash name symbol-table)
                 (entry-type lol:it)
                 (error 'undefined-symbol :symbol-name name :table symbol-table)))

      (:clear-table ()
        (setf symbol-table (make-hash-table :test #'equal)))

      (t (&rest ignore)
        (declare (ignore ignore))
        (error 'u:internal-error :reason "Illegal SYMBOL-TABLE function keyword.")))))
