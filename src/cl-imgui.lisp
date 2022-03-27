(defpackage :cl-imgui
  (:use :common-lisp :ffi))

(cl::in-package :cl-imgui)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IMGUI-clib.json")
                    (concatenate 'string cl-user::*project-binary-dir* "/IMGUI-clib.h"))
