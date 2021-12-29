;;;; The assemblers internal symbol table

(defpackage symbol-table
  (:export #:symbol-table #:make-entry) 
  (:nicknames #:symt)
  (:use #:cl))

(in-package #:symbol-table)

(defstruct entry 
  (name  nil :type string             :read-only t)
  (value nil :type (unsigned-byte 64) :read-only t)
  (type  nil :type keyword            :read-only t))

(deftype symbol-name (&optional type)
  
  )

(defun symbol-table ()
  (let ((symbol-table (make-hash-table)))
    
    (lambda (function-keyword &rest inputs)
      (case function-keyword
        
        (:entry-p
         ;; T iff (FIRST INPUTS) is the name of an entry in SYMBOL-TABLE
         (nth-value 1 (gethash (first inputs) symbol-table)))
        
        (:insert
         ;; insert an entry into SYMBOL-TABLE with name, value, and type
         ;; inputted in that order.
         (setf (gethash (first inputs) symbol-table)
               (make-entry :name (first inputs)
                           :value (second inputs)
                           :type (third inputs))))

        (:symbol-value
         ;; the value of the symbol with the name (FIRST INPUTS)
         (entry-value (gethash (first inputs) symbol-table)))))))
