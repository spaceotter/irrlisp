(defpackage :irrlisp
  (:use :common-lisp :ffi)
  (:export #:increment2))

(cl::in-package :irrlisp)

(defun increment2 (n)
  (+ n 2))
