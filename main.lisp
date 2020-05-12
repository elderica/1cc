; vim:set ts=2 sw=2 et:
(defpackage :1cc
  (:use :cl)
  (:shadow :compile)
  (:export :compile))
(in-package :1cc)

(defclass token ()
  ((kind
     :documentation "Kind of token"
     :initarg :kind
     :initform (error "Must supply a kind of token.")
     :reader token-kind)
   (string
     :documentation "Token string."
     :initarg :string
     :initform ""
     :reader token-string)
   (number
     :documentation "If kind is `'number`, its value."
     :initarg :number)))

(defgeneric token-number (tok)
  (:documentation "return a number if kind is `'number'."))

(defmethod token-number ((tok token))
  (unless (eql (token-kind tok) 'number)
    (error "expected a number."))
  (slot-value tok 'number))

(defmethod print-object ((obj token) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream ":kind ~s :string ~s" (token-kind obj) (token-string obj))
    (when (eql (token-kind obj) 'number)
      (format stream " :number ~d" (token-number obj)))))

(defparameter *tokens* nil)

(defun match (s)
  "Consumes the current token if it match `s`."
  (string-equal (token-string (first *tokens*)) s))

(defun skip (s)
  "Ensure that the current token is `s`."
  (unless (match s)
    (error (format nil "expected '~a'" s)))
  (setf *tokens* (rest *tokens*)))

(defun tokenize (code)
  (setf *tokens* nil)
  (loop
    (setf code (string-left-trim '(#\Space #\Tab #\Newline) code))
    (when (zerop (length code))
      (push (make-instance 'token
                           :kind 'end-of-file)
            *tokens*)
      (setf *tokens* (nreverse *tokens*))
      (return *tokens*))
    (let ((c (elt code 0)))
      (case c
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (multiple-value-bind (num pos)
                (parse-integer code :junk-allowed t)
           (setf code (subseq code pos))
           (push (make-instance 'token
                                :kind 'number
                                :string (format nil "~d" num)
                                :number num)
                 *tokens*)))
        ((#\+ #\-)
         (push (make-instance 'token
                              :kind 'reserved
                              :string (format nil "~a" c))
           *tokens*)
         (setf code (subseq code 1)))
        (t (error "Invalid token."))))))

(defun compile (code)
  (setf *tokens* (tokenize code))
  (format t ".intel_syntax noprefix~%")
  (format t ".global main~%")
  (format t "main:~%")
  (format t "  mov rax, ~d~%" (token-number (first *tokens*)))
  (setf *tokens* (rest *tokens*))
  (flet ((we-reached-eof-p ()
           (eql (token-kind (first *tokens*)) 'end-of-file))
         (next-number ()
           (token-number (second *tokens*)))
         (proceed ()
           (setf *tokens* (subseq *tokens* 2))))
    (loop
      (when (we-reached-eof-p) (return))
      (when (match "+")
        (format t "  add rax, ~d~%" (next-number)))
      (when (match "-")
        (format t "  sub rax, ~d~%" (next-number)))
      (proceed)))
  (format t "  ret~%"))
