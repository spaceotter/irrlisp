(defpackage :irrlisp
  (:use :common-lisp :ffi)
  (:export))

(cl::in-package :irrlisp)

(cl-upp:include-c++ cl-user::*irrlicht-json*)
