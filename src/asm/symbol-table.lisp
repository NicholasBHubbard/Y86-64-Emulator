;;;; The assemblers internal symbol table

(defpackage symbol-table
  (:export #:symbol-table
           #:make-entry
           #:duplicate-symbol
           #:undefined-symbol) 
  (:nicknames #:symt)
  (:use #:cl)
  (:shadow #:symbol-name))

(in-package #:symbol-table)

(defstruct entry 
  (name  nil :type symbol-name        :read-only t)
  (type  nil :type keyword            :read-only t)
  (value nil :type (unsigned-byte 64) :read-only t))

(deftype symbol-name ()
  '(and string (satisfies symbol-name-p)))

(defun symbol-name-p (string)
  (cl-ppcre:scan "^:[A-Z][A-Z0-9]*$" string))

(define-condition undefined-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type symbol-name)
   (table :initarg :table :reader table :type hash-table)))

(define-condition duplicate-symbol (error)
  ((symbol-name :initarg :symbol-name :reader symbol-name :type symbol-name)
   (table :initarg :table :reader table :type hash-table)))

(defun symbol-table ()
  (let ((symbol-table (make-hash-table)))
    
    (lambda (function-keyword &rest inputs)
      (case function-keyword
        
        (:entry-p
         ;; T iff (FIRST INPUTS) is the name of an entry in SYMBOL-TABLE
         (nth-value 1 (gethash (first inputs) symbol-table)))
        
        (:insert
         ;; Insert an entry into SYMBOL-TABLE with name, type, and value
         ;; inputted in that order. Signal a duplicate-symbol error if the
         ;; symbol has already been defined.
         (let* ((symbol-name (first inputs))
                (symbol-type (second inputs))
                (symbol-value (third inputs))
                (symbol-exists (nth-value 1 (gethash symbol-name symbol-table))))
           (if (not symbol-exists)
               (setf (gethash symbol-name symbol-table)
                     (make-entry :name symbol-name
                                 :type symbol-type
                                 :value symbol-value))
               (error 'duplicate-symbol :symbol-name symbol-name :table symbol-table))))

        (:symbol-value
         ;; The value of the symbol named (FIRST INPUTS).          
         (let* ((symbol-name (first inputs))
                (symbol-exists (nth-value 1 (gethash symbol-name symbol-table))))
           (if symbol-exists
               (entry-value (gethash symbol-name symbol-table))
               (error 'undefined-symbol :symbol-name symbol-name :table symbol-table))))
        
        (:symbol-type
         ;; The type of the symbol named (FIRST INPUTS)
         (let* ((symbol-name (first inputs))
                (symbol-exists (nth-value 1 (gethash symbol-name symbol-table))))
           (if symbol-exists
               (entry-type (gethash symbol-name symbol-table))
               (error 'undefined-symbol :symbol-name symbol-name :table symbol-table))))))))