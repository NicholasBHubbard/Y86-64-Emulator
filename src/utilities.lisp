;;;; Basic convenience utilities.

(defpackage utilities
  (:use #:cl)
  (:nicknames #:utils #:u)
  (:local-nicknames (#:a #:alexandria))
  (:export #:internal-error
           #:const
           #:make-keyword
           #:as-keyword
           #:defstruct-read-only
           #:defclosure))

(in-package #:utilities)

;;; ===============================================

(trivial-indent:define-indentation lol:dlambda
    (&rest (&whole 2 &rest 2)))

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

(defun make-keyword (string)
  "Interns the string designated by STRING in the keyword package."
  (a:make-keyword (string-upcase string)))

;;; ===============================================

(defun as-keyword (thing)
  "Coerce THING into a keyword symbol interned in the keyword package."
  (etypecase thing
    (keyword thing)
    (symbol (make-keyword (symbol-name thing)))
    (string (make-keyword thing))
    (number (make-keyword (write-to-string thing)))))

;;; ===============================================

(defmacro defstruct-read-only (name &body options)
  "Like DEFSTRUCT but each field is made read-only."
  (flet ((append-read-only-slot-option (options)
           (loop :for opt :in options
                 :collect (append (a:remove-from-plist opt :read-only)
                                  (list :read-only t)))))
    (if (stringp (first options))
        `(defstruct ,name
           ,(first options)
           ,@(append-read-only-slot-option (rest options)))
        `(defstruct ,name
           ,@(append-read-only-slot-option options)))))

;;; ===============================================

(defmacro defclosure (name &body body)
  "Bind a lexical closure to a function symbol named NAME.

BODY should contain an optional docstring followed by a lambda expression."
  (multiple-value-bind (form _ docstring)
      (a:parse-body body :documentation t)
    (declare (ignore _))
    `(progn
       (setf (symbol-function ',name) ,(first form))
       (setf (documentation ',name 'function) ,docstring))))
