(in-package :cl-user)
(defpackage :simple-rectangle-test
  (:use :cl
        :sdl2
        :alexandria)
  (:export :main))
(require :cl-opengl)

(in-package :simple-rectangle-test)
(defparameter *vertex-array* (let ((gl-array (gl:alloc-gl-array :float 8))
                                   (items #(-0.5 -0.5 0.5 -0.5 -0.5 0.5 0.5 0.5)))
                               (dotimes (i (length items))
                                 (setf (gl:glaref gl-array i) (aref items i)))
                               gl-array))
(defparameter *element-array* (let ((gl-array (gl:alloc-gl-array :unsigned-short 4))
                                    (items #(0 1 2 3)))
                                (dotimes (i (length items))
                                  (setf (gl:glaref gl-array i) (aref items i)))
                                gl-array))

(defparameter *vertex-shader-source* (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test "vertex.glsl")))
(defparameter *fragment-shader-source* (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test "fragment.glsl")))

(defun main ()
  (with-init (:everything)
    (with-window (my-window :title "CL-OpenGL test" :flags '(:shown :opengl))
      (with-gl-context (gl-context my-window)
        (gl-make-current my-window gl-context)
        (gl:viewport 0 0 800 600)
        (let ((vertex-buffer (car (gl:gen-buffers 1)))
              (element-buffer (car (gl:gen-buffers 1))))
          
          (gl:bind-buffer :array-buffer vertex-buffer)
          (gl:buffer-data :array-buffer :static-draw *vertex-array*)
          
          (gl:bind-buffer :element-array-buffer element-buffer)
          (gl:buffer-data :element-array-buffer :static-draw *element-array*)
          
          (let ((vertex-shader (gl:create-shader :vertex-shader))
                (fragment-shader (gl:create-shader :fragment-shader))
                (shader-program (gl:create-program)))
            
            (gl:shader-source vertex-shader *vertex-shader-source*)
            (gl:compile-shader vertex-shader)
            
            (gl:shader-source fragment-shader *fragment-shader-source*)
            (gl:compile-shader fragment-shader)
            
            (gl:attach-shader shader-program vertex-shader)
            (gl:attach-shader shader-program fragment-shader)
            
            (gl:link-program shader-program)
            (gl:use-program shader-program)
            
            (gl:bind-buffer :array-buffer vertex-buffer)
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "position")
                                      2
                                      :float
                                      :false
                                      (* 2 (cffi:foreign-type-size :float))
                                      (cffi:null-pointer))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
            (with-event-loop (:method :poll)
              (:idle ()
                     (gl:clear-color 0.0 0.0 0.0 0.0)
                     (gl:clear :color-buffer)
                     (gl:bind-buffer :element-array-buffer element-buffer)
                     (gl:draw-elements :triangle-strip
                                       (gl:make-null-gl-array :unsigned-short)
                                       :count 4)
                     (gl-swap-window my-window))
              (:quit ()
                     (gl:disable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
                     t))))))))