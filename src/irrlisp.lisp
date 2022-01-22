(defpackage :irrlisp
  (:use :common-lisp :ffi)
  (:export #:async-delayed-error
           #:setup
           #:convert-string))

(cl::in-package :irrlisp)

(cl-upp:include-c++ (concatenate 'string cl-user::*project-binary-dir* "/IrrlichtMt-clib.json"))

;; TODO: GUI debugger and better error handling
(ffi:def-function ("gui_debugger" gui-debugger) ((condition :object) (old-hook :object)))
(defun delayed-error () (sleep 1) (error 'simple-type-error))
(defun async-delayed-error () (mp:process-run-function 'troll #'delayed-error))

;; TODO: Avoid leaking memory
(defun convert-string (str)
  (ffi:with-foreign-string (c-str str)
    (format t "cstr: ~S~%" c-str)
    (ffi:with-cast-pointer (c-str (* :byte))
      (format t "cstr: ~S~%" c-str)
      (upp-new-irr-core-string-char-irr-core-irrAllocator-char-8 c-str))))

(defun setup (device osmgr)
  (format t "~A~%" device)
  (let* ((smgr (upp-irr-IrrlichtDevice-getSceneManager device))
         )
    (format t "osmgr~A~%" osmgr)
    (format t "smgr~A~%" smgr)
    (let* ((path (convert-string "irrlicht/media/faerie.md2"))
           (mesh (upp-irr-scene-ISceneManager-getMesh-2 smgr path)))
      (format t "~A~%" path)
      (format t "~A~%" mesh)
      (upp-irr-scene-ISceneManager-addAnimatedMeshSceneNode smgr mesh)
      (format t "Added to scene~%")

      )
    ))
