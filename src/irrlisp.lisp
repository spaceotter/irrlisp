(defpackage :irrlisp
  (:use :common-lisp :ffi)
  (:export #:async-delayed-error))

(cl::in-package :irrlisp)

(cl-upp:include-c++ cl-user::*irrlicht-json*)

(ffi:def-function ("gui_debugger" gui-debugger) ((condition :object) (old-hook :object)))

(defun delayed-error () (sleep 1) (error 'simple-type-error))

(defun async-delayed-error () (mp:process-run-function 'troll #'delayed-error))
