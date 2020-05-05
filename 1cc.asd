; vim:set ts=2 sw=2 et syntax=lisp:
(defsystem :1cc
  :components ((:file "main"))
  :in-order-to ((test-op (test-op :1cc/test))))

(defsystem :1cc/test
  :depends-on (:1am :uiop :1cc)
  :components ((:file "test"))
  :perform (prepare-op :before (o c)
             (set (find-symbol "*TESTS*" :1am) 'nil)) ; must be uppercase
  :perform (test-op (o c)
             (let ((*package* (find-package :1cc)))
               (uiop:symbol-call :1am :run))))
