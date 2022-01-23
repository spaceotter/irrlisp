(defpackage :irrlisp-test
  (:use :common-lisp :ffi :irrlisp)
  (:export #:async-delayed-error
           #:setup
           #:convert-string))

(cl::in-package :irrlisp-test)

;; TODO: GUI debugger and better error handling
;;(ffi:def-function ("gui_debugger" gui-debugger) ((condition :object) (old-hook :object)))
(defun delayed-error () (sleep 1) (error 'simple-type-error))
(defun async-delayed-error () (mp:process-run-function 'troll #'delayed-error))

;; TODO: Avoid leaking memory
(defun convert-string (str)
  (ffi:with-foreign-string (c-str str)
    (format t "cstr: ~S~%" c-str)
    (irrlisp::upp-new-irr-core-string-char-irr-core-irrAllocator-char-8 c-str)))

(defmacro coerce-base (string)
  (coerce string 'base-string))

(defun get-mesh (smgr path)
  (ffi:with-foreign-string (c-path path)
    (ffi:with-foreign-string (alt-cache (coerce "" 'base-string))
      (irrlisp::upp-irr-scene-ISceneManager-getMesh
       smgr
       (irrlisp::upp-new-irr-core-string-char-irr-core-irrAllocator-char-8 c-path)
       (irrlisp::upp-new-irr-core-string-char-irr-core-irrAllocator-char-8 alt-cache)
       )
      )))

(defun setup (device osmgr)
  (format t "~A~%" device)
  (let* ((smgr (irrlisp::upp-irr-IrrlichtDevice-getSceneManager device))
         )
    (format t "osmgr ~A~%" osmgr)
    (format t "smgr ~A~%" smgr)
    (let ((mesh (get-mesh smgr (coerce "irrlicht/media/faerie.md2" 'base-string))))
      (format t "~A~%" mesh)
      (unless (ffi:null-pointer-p mesh)
        (irrlisp::upp-irr-scene-ISceneManager-addAnimatedMeshSceneNode
         smgr mesh
         (ffi:make-null-pointer :void)
         -1
         (irrlisp::upp-new-irr-core-vector3d-float)
         (irrlisp::upp-new-irr-core-vector3d-float)
         (irrlisp::upp-new-irr-core-vector3d-float-2 1.0 1.0 1.0)
         (coerce #\Null 'base-char))
        (format t "Added to scene?~%"))
      )
    ))
