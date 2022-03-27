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
  (ffi:with-foreign-string
      (c-str (coerce str 'base-string))
      (irrlisp::upp-new-irr-core-string-char-irr-core-irrAllocator-char-8 c-str)))

(defmacro coerce-base (string)
  (coerce string 'base-string))

(defun get-mesh (smgr path)
  (with-irr-strings ((c-path path) (alt-cache ""))
    (irrlisp::upp-irr-scene-ISceneManager-getMesh smgr c-path alt-cache)))

(defvar *device* nil)
(defvar *driver* nil)
(defvar *node* nil)
(defvar *texture* nil)

(defun setup ()
  (format t "~A~%" *device*)
  (let* ((smgr (irrlisp::upp-irr-IrrlichtDevice-getSceneManager *device*))
         )
    (format t "smgr ~A~%" smgr)
    (let ((mesh (get-mesh smgr "irrlicht/media/faerie.md2")))
      (format t "~A~%" mesh)
      (unless (ffi:null-pointer-p mesh)
        (setq *node* (irrlisp::upp-irr-scene-ISceneManager-addAnimatedMeshSceneNode
               smgr mesh
               (ffi:make-null-pointer :void)
               -1
               (irrlisp::upp-new-irr-core-vector3d-float-2 0.0 0.0 0.0)
               (irrlisp::upp-new-irr-core-vector3d-float)
               (irrlisp::upp-new-irr-core-vector3d-float-2 1.0 1.0 1.0)
               0))
        (format t "Added to scene?~%")
        (irrlisp::upp-irr-scene-ianimatedmeshscenenode-setmd2animation *node* 0)
        (irrlisp::upp-irr-scene-iscenenode-setmaterialflag *node* 8 0)
        (setq *texture*
              (with-irr-string (bmp-path "irrlicht/media/faerie2.bmp")
                               (irrlisp::upp-irr-video-ivideodriver-gettexture *driver* bmp-path)))
        (irrlisp::upp-irr-scene-iscenenode-setmaterialtexture *node* 0 *texture*))
      )
    ))
