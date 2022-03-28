(defpackage :cl-imgui
  (:use :common-lisp :ffi)
  (:export #:begin #:end #:text #:button))

(cl::in-package :cl-imgui)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IMGUI-clib.json")
                    (concatenate 'string cl-user::*project-binary-dir* "/IMGUI-clib.h"))

(defun begin (name &optional (p-open (ffi:make-null-pointer :void)) (flags 0))
  (declare (type base-string name))
  (with-foreign-string (title name)
    (cl-imgui::upp-ImGui-Begin title p-open flags)))

(defun text (content)
  (declare (type base-string content))
  (ffi:c-inline (content) (:object) :void
                "upp_ImGui_TextUnformatted(#0->base_string.self, #0->base_string.self + #0->base_string.dim);" :one-liner nil))

(defun button (text w h)
  (declare (type base-string text))
  (with-foreign-string (c-text text)
      (let* ((size (cl-imgui::upp-new-imvec2-2 w h))
             (result (cl-imgui::upp-ImGui-Button c-text size)))
        (cl-imgui::upp-del-imvec2 size)
        (= 1 result))))

(defun end () (cl-imgui::upp-ImGui-End))
