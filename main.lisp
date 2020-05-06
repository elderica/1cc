; vim:set ts=2 sw=2 et:
(defpackage :1cc
  (:use :cl)
  (:shadow :compile)
  (:export :compile))
(in-package :1cc)

(defun strtol (nstr s)
  (parse-integer nstr :start s
                      :radix 10
                      :junk-allowed t))

(defun compile (code)
  (format t ".intel_syntax noprefix~%")
  (format t ".global main~%")
  (format t "main:~%")
  (let ((p 0)
        (num 0))
    (multiple-value-setq (num p) (strtol code p))
    (format t "  mov rax, ~d~%" num)
    (do ()
        ((>= p (length code)))
      (ccase (elt code p)
        (#\+ (incf p)
             (multiple-value-setq (num p) (strtol code p))
             (format t "  add rax, ~d~%" num))
        (#\- (incf p)
             (multiple-value-setq (num p) (strtol code p))
             (format t "  sub rax, ~d~%" num))))
    (format t "  ret~%")))
