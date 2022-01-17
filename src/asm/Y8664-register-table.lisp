;;;; This package exists to export the *REGISTER-TABLE* special variable which
;;;; is a lexical closure that can be used to dynamically query the static and
;;;; immutable Y86-64 register table.

(defpackage Y8664-register-table
  (:export #:*register-table*)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword))

(in-package #:Y8664-register-table)

(deftype register ()
  '(member :RAX :RCX :RDX :RBX :RSP :RBP :RSI :RDI :R8 :R9 :R10 :R11 :R12 :R13 :R14 :NOREG))

(defstruct entry
  (id    nil :type (unsigned-byte 8) :read-only t)
  (name  nil :type register          :read-only t))

(defun init-register-table ()
  (let ((register-table
          (list
           (make-entry :id #x0 :name :RAX)
           (make-entry :id #x1 :name :RCX)
           (make-entry :id #x2 :name :RDX)
           (make-entry :id #x3 :name :RBX)
           (make-entry :id #x4 :name :RSP)
           (make-entry :id #x5 :name :RBP)
           (make-entry :id #x6 :name :RSI)
           (make-entry :id #x7 :name :RDI)
           (make-entry :id #x8 :name :R8)
           (make-entry :id #x9 :name :R9)
           (make-entry :id #xA :name :R10)
           (make-entry :id #xB :name :R11)
           (make-entry :id #xC :name :R12)
           (make-entry :id #xD :name :R13)
           (make-entry :id #xE :name :R14)
           (make-entry :id #xF :name :NOREG))))
    
    (lambda (function-keyword &rest inputs)
      
      ;; turn input strings into keywords so user the can choose input format
      (let ((inputs (mapcar (lambda (input)
                              (if (stringp input)
                                  (make-keyword (string-upcase input))
                                  input))
                            inputs)))
        
        (case function-keyword
          
          (:id-p
           ;; T iff (FIRST INPUTS) is a valid Y86-64 register id
           (if (member (first inputs) (mapcar #'entry-id register-table) :test #'=)
               t))
          
          (:register-name-p
           ;; T iff (FIRST INPUTS) is a valid Y86-64 register name
           (if (member (first inputs) (mapcar #'entry-name register-table))
               t))
          
          (:all-ids
           ;; list of all the Y86-64 register ids
           (sort (mapcar #'entry-id register-table) #'<))
          
          (:all-register-names
           ;; list of all the Y86-64 register names
           (sort (mapcar #'entry-name register-table) #'string<))
          
          (:all-id-strings
           ;; like :ALL-IDS except stringify the ids
           (sort (mapcar
                  (compose (curry #'format nil "~x") #'entry-id)
                  register-table)
                 #'string<))
          
          (:all-register-name-strings
           ;; like :ALL-REGISTER-NAMES except stringify the register names
           (sort (mapcar (compose #'symbol-name #'entry-name) register-table)
                 #'string<))
          
          (:id-register-name
           ;; the register name associated to the id (FIRST INPUTS)
           (entry-name
            (find-if
             (compose (curry #'= (first inputs)) #'entry-id)
             register-table)))
          
          (:register-name-id
           ;; the id of the register named (FIRST INPUTS)
           (entry-id
            (find-if
             (compose (curry #'eql (first inputs)) #'entry-name)
             register-table)))
          
          (:id-register-name-string
           ;; like :ID-REGISTER-NAME except stringify the register name
           (symbol-name
            (entry-name
             (find-if
              (compose (curry #'= (first inputs)) #'entry-id)
              register-table))))
          
          (:register-name-id-string
           ;; like :REGISTER-NAME-ID except stringify the id
           (format nil "~x"
                   (entry-id
                    (find-if
                     (compose (curry #'eql (first inputs)) #'entry-name)
                     register-table))))

          (:id-register-name-match-p
           ;; T iff the id (FIRST INPUTS) has register name (SECOND INPUTS)
           (eql (second inputs)
                (entry-name
                 (find-if
                  (compose (curry #'= (first inputs)) #'entry-id)
                  register-table))))

          (otherwise
           (error 'internal (format nil "The symbol ~a does not denote a valid function" function-keyword))))))))


(defparameter *register-table* (init-register-table))
