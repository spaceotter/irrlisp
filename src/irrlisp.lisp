(defpackage :irrlisp
  (:use :common-lisp :ffi)
  (:export #:make-irr-string #:with-irr-string #:with-irr-strings))

(cl::in-package :irrlisp)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.json")
                    (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.h"))

(defun make-irr-string (b-str)
  (etypecase b-str
    (base-string
     (ffi:c-inline (b-str) (:object) :pointer-void
                   "upp_new_irr_core_string_char_irr_core_irrAllocator_char_8(#0->base_string.self, #0->base_string.dim)"
                                     :one-liner t))
    (string
     (ffi:c-inline (b-str) (:object) :pointer-void
                   "upp_new_irr_core_string_char_irr_core_irrAllocator_char_9(#0->string.self, #0->string.dim)"
                                     :one-liner t))))

(defmacro with-irr-string ((irr-string lisp-string) &rest body)
  "Syntax: (with-irr-string ((irr-string lisp-string) &rest body)

Binds IRR-STRING to an Irrlicht string created from conversion of a
STRING and evaluated the BODY. Automatically frees the IRR-STRING."
  `(let* ((,irr-string (make-irr-string ,lisp-string)))
     (mp:without-interrupts
       (unwind-protect
            (mp:with-restored-interrupts ,@body)
         (irrlisp::upp-del-irr-core-string-char-irr-core-irrAllocator-char ,irr-string)))))

(defmacro with-irr-strings (bindings &rest body)
  "Syntax: (with-irr-strings ((foreign-string string)*) &body body)

See: WITH-FOREIGN-STRING. Works similar to LET*."
  (if bindings
      `(with-irr-string ,(car bindings)
                        (with-irr-strings ,(cdr bindings)
                          ,@body))
      `(progn ,@body)))
