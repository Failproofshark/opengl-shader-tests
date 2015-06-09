(in-package :cl-user)
(defpackage :open-gl-test
  (:use :cl
        :sdl2
        :alexandria)
  (:export :main))
(require :cl-opengl)

(in-package :open-gl-test)
(defparameter *vertex-array* '(-20.0 -20.0 20.0 -20.0 -20.0 20.0 20.0 20.0))
(defparameter *element-array* '(0 1 2 3))

(defparameter *vertex-shader-source* (asdf:system-relative-pathname 'opengl-shader-test "vertex.glsl"))
(defparameter *fragment-shander-source* (asdf:system-relative-pathname 'opengl-shader-test "fragment.glsl"))

(defun main ()
  (with-init (:everything)
    (with-window (my-window :title "CL-OpenGL test" :flags '(:shown :opengl))
      (with-gl-context (gl-context my-window)
        (gl-make-current my-window gl-context)
        (gl:viewport 0 0 800 600)
        (with-event-loop (:method :poll)
          (:idle ()
                 (gl:clear-color 0.0 0.0 0.0 0.0)
                 (gl:clear :color-buffer)
                 (gl-swap-window my-window))
          (:quit () t)))))) 
