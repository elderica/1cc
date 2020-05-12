; vim:set ts=2 sw=2 et:
(defpackage :1cc/test
  (:use :cl
        :uiop
        :1am)
  (:shadowing-import-from :1cc :compile))
(in-package :1cc/test)

(defparameter *tmpfile* (merge-pathnames #p"tmp.s" (uiop:getcwd)))

(defun try (code)
  (with-open-file (*standard-output* *tmpfile*
                                     :direction :output
                                     :if-exists :supersede)
    (1cc:compile code))
  (let ((asm (namestring *tmpfile*))
        (exe (namestring
               (make-pathname :type nil
                              :defaults *tmpfile*))))
    (uiop:run-program (list "gcc" "-g" "-o" exe asm))
    (nth-value 2 (uiop:run-program exe :ignore-error-status t))))

(test compile-test
  (is (= 0 (try "0")))
  (is (= 42 (try "42")))
  (is (= 21 (try "5+20-4")))
  (is (= 41 (try " 12 + 34 -5 "))))
