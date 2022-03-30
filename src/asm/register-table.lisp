(defpackage register-table
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:register-table
           #:register))

(in-package #:register-table)

;;; ==================== Types ====================

(deftype register ()
  '(member :rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :noreg))

(u:defstruct-read-only entry
  "The type of a single Y86-64 register table entry."
  (id   nil :type (unsigned-byte 8))
  (name nil :type register))

;;; ==================== Register Table Definition ====================

(u:defclosure register-table
  "Lexical closure over the Y86-64 static register table. This closure uses
LOL:DLAMBDA to provide various function keywords for dynamically dispatching a
query function to the register table. 

Documentation for all the provided dispatch keywords:

:id-p id
  Return T if ID is a valid Y86-64 register id and return NIL otherwise.

:register-name-p reg
  Return T if REG is a valid Y86-64 register and return NIL otherwise. REG can
  be either a keyword or a string.

:all-ids
  Return a list of all the Y86-64 register ids.

:all-register-names
  Return a list of all the Y86-64 register name keywords.

:all-id-strings
  Return a list of all the Y86-64 register id's as strings.

:all-register-name-strings
  Return a list of all the Y86-64 register names as strings.

:id-register-name id
  Return the register name keyword with id ID.

:register-name-id reg
  Return the id of the register named REG. REG can be either a keyword or a
  string.

:id-register-name-string id
  Return the register name string of the register with id ID.

:register-name-id-string reg
  Return the id of the register named REG as a string. REG can be either a
  keyword or a string.

:id-register-name-match-p id reg
  Return T if the register REG has id ID and return NIL otherwise. REG can be
  either a keyword or a string. "
  (let ((register-table
          (list
           (make-entry :id #x0 :name :rax)
           (make-entry :id #x1 :name :rcx)
           (make-entry :id #x2 :name :rdx)
           (make-entry :id #x3 :name :rbx)
           (make-entry :id #x4 :name :rsp)
           (make-entry :id #x5 :name :rbp)
           (make-entry :id #x6 :name :rsi)
           (make-entry :id #x7 :name :rdi)
           (make-entry :id #x8 :name :r8)
           (make-entry :id #x9 :name :r9)
           (make-entry :id #xa :name :r10)
           (make-entry :id #xb :name :r11)
           (make-entry :id #xc :name :r12)
           (make-entry :id #xd :name :r13)
           (make-entry :id #xe :name :r14)
           (make-entry :id #xf :name :noreg))))
    (lol:dlambda
      (:id-p (id)
        (if (member id (mapcar #'entry-id register-table) :test #'=)
            t))
      
      (:register-name-p (reg)
        (if (member (u:as-keyword reg) (mapcar #'entry-name register-table))
            t))
      
      (:all-ids ()
        (sort (mapcar #'entry-id register-table) #'<))
      
      (:all-register-names ()
        (sort (mapcar #'entry-name register-table) #'string<))
      
      (:all-id-strings ()
        (sort (mapcar
               (a:compose (a:curry #'format nil "~x") #'entry-id)
               register-table)
              #'string<))
      
      (:all-register-name-strings ()
        (sort (mapcar (a:compose #'symbol-name #'entry-name) register-table)
              #'string<))
      
      (:id-register-name (id)
        (entry-name
         (find-if
          (a:compose (a:curry #'= id) #'entry-id)
          register-table)))
      
      (:register-name-id (reg)
        (entry-id
         (find-if
          (a:compose (a:curry #'eql (u:as-keyword reg)) #'entry-name)
          register-table)))
      
      (:id-register-name-string (id)
        (symbol-name
         (entry-name
          (find-if
           (a:compose (a:curry #'= id) #'entry-id)
           register-table))))
      
      (:register-name-id-string (reg)
        (format nil "~x"
                (entry-id
                 (find-if
                  (a:compose (a:curry #'eql (u:as-keyword reg)) #'entry-name)
                  register-table))))

      (:id-register-name-match-p (id reg)
        (eql (u:as-keyword reg)
             (entry-name
              (find-if
               (a:compose (a:curry #'= id) #'entry-id)
               register-table))))

      (t (&rest ignore)
        (declare (ignore ignore))
        (error 'u:internal-error :reason "Illegal REGISTER-TABLE function keyword.")))))
