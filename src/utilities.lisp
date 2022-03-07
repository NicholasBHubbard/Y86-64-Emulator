;;;; Basic convenience utilities.

(defpackage utilities
  (:use #:cl)
  (:nicknames #:utils #:u)
  (:local-nicknames (#:a #:alexandria))
  (:export #:internal-error
           #:const
           #:make-keyword
           #:defstruct-read-only
           #:defclosure))

(in-package #:utilities)

;;; ===============================================

(define-condition internal-error (error)
  ((reason :initarg :reason :reader internal-error-reason :type string))
  (:documentation "Condition for internal programmer errors.")
  (:report (lambda (c s) (format s (internal-error-reason c)))))

;;; ===============================================

(defun const (x)
  "Return a function that takes one argument and always returns X."
  (lambda (y) (declare (ignore y)) x))

;;; ===============================================

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (a:make-keyword (string-upcase name)))

;;; ===============================================

(defmacro defstruct-read-only (name &body options)
  "Like DEFSTRUCT but each field is made read-only."
  (flet ((append-read-only (options)
           (loop :for opt :in options
                 :collect (append (a:remove-from-plist opt :read-only)
                                  (list :read-only t)))))
    (if (stringp (first options))
        `(defstruct ,name
           ,(first options)
           ,@(append-read-only (rest options)))
        `(defstruct ,name
           ,@(append-read-only options)))))

;;; ===============================================

(defmacro defclosure (name &body body)
  "Bind a lexical closure to a function symbol named NAME. BODY should denote a
form that returns a LAMBDA."
  (let ((forms (gensym "forms"))
        (_ (gensym "_"))
        (docstring (gensym "docstring")))
    `(multiple-value-bind (,forms ,_ ,docstring)
         (a:parse-body ,body :documentation t)
       (declare (ignore ,_))
       (setf (symbol-function ',name) ,@body)
       (setf (documentation ',name 'function) ,docstring))))
