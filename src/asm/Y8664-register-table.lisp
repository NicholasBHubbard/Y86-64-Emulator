;;;; Register Table for Y86-64.

(defpackage Y8664-register-table
  (:nicknames #:regt)
  (:export #:register-table)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword))

(in-package #:Y8664-register-table)

(defstruct entry
  (id    nil :type (unsigned-byte 8))
  (name  nil :type keyword))

(defun register-table ()
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
      
      ;; if an input is a string make it a keyword instead
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
           ;; a list of all the Y86-64 register ids
           (mapcar #'entry-id register-table))
          
          (:all-register-names
           ;; a list of all the Y86-64 register names
           (mapcar #'entry-name register-table))
          
          (:all-id-strings
           ;; like :ALL-IDS except stringify the ids
           (mapcar
            (compose (curry #'format nil "~x") #'entry-id)
            register-table))
          
          (:all-register-name-strings
           ;; like :ALL-REGISTER-NAMES except stringify the register names
           (mapcar (compose #'symbol-name #'entry-name) register-table))
          
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
           (error (format t "The symbol ~a does not denote a valid function" function-keyword))))))))