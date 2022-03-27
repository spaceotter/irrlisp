(defpackage :irrlisp
  (:use :common-lisp :ffi))

(cl::in-package :irrlisp)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.json")
                    (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.h"))

(defmacro with-irr-string ((irr-string lisp-string) &rest body)
  "Syntax: (with-irr-string ((irr-string lisp-string) &rest body)

Binds IRR-STRING to an Irrlicht string created from conversion of a
STRING and evaluated the BODY. Automatically frees the IRR-STRING."
  (let ((foreign-string (gensym)))
    `(let* ((,foreign-string (convert-to-foreign-string (coerce ,lisp-string 'base-string)))
            (,irr-string (irrlisp::upp-new-irr-core-string-char-irr-core-irrAllocator-char-8
                          ,foreign-string)))
       (mp:without-interrupts
         (unwind-protect
              (mp:with-restored-interrupts ,@body)
           (irrlisp::upp-del-irr-core-string-char-irr-core-irrAllocator-char ,irr-string)
           (free-foreign-object ,foreign-string))))))

(defmacro with-irr-strings (bindings &rest body)
  "Syntax: (with-irr-strings ((foreign-string string)*) &body body)

See: WITH-FOREIGN-STRING. Works similar to LET*."
  (if bindings
      `(with-irr-string ,(car bindings)
                        (with-irr-strings ,(cdr bindings)
                          ,@body))
      `(progn ,@body)))

(export (list 'with-irr-string 'with-irr-strings))
