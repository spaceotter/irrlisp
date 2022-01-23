(defpackage :irrlisp
  (:use :common-lisp :ffi))

(cl::in-package :irrlisp)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.json")
                    (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.h"))
