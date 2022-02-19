(defpackage utilities
  (:use #:cl #:lol)
  (:nicknames #:utils #:u)
  (:local-nicknames (#:a #:alexandria))
  (:export #:const
           #:make-keyword
           #:defstruct-read-only))

(in-package #:utilities)

;;; ===============================================

(defun const (x)
  "Return a function that takes one argument and always returns X."
  (lambda (y) (declare (ignore y)) x))

;;; ===============================================

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string-upcase (string name)) :keyword))

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
