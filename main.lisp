; vim:set ts=2 sw=2 et:
(defpackage :1cc
  (:use :cl)
  (:shadow :compile)
  (:export :compile))
(in-package :1cc)

(defun compile (code)
  (format t ".intel_syntax noprefix~%")
  (format t ".global main~%")
  (format t "main:~%")
  (format t "  mov rax, ~d~%" (parse-integer code))
  (format t "  ret~%"))
