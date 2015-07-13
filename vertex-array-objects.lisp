(in-package :cl-user)
(defpackage :vertex-array-and-stride-test
  (:use :cl
        :alexandria
        :sdl2)
  (:export :main))
(require :cl-opengl)
(in-package :vertex-array-and-stride-test)

(defun create-gl-array (type lisp-array)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun main ()
  (let ((vertex-attribute-array (create-gl-array :float #(-0.5 -0.5 1.0 1.0 1.0 
                                                          0.5 -0.5 1.0 1.0 1.0 
                                                          -0.5  0.5 1.0 1.0 1.0 
                                                          0.5  0.5 1.0 1.0 1.0)))
        (element-attribute-array (create-gl-array :unsigned-short #(0 1 2 3))))
    (with-init (:everything)
      (with-window (my-window :title "CL-OpenGL test" :flags '(:shown :opengl))
        (with-gl-context (gl-context my-window)
          (gl-make-current my-window gl-context)
          (gl:viewport 0 0 800 600)
          ;;The first buffer is our verticies, the second is our elements
          (let ((buffers (gl:gen-buffers 2))
                (vao (car (gl:gen-vertex-arrays 1)))
                (vertex-shader (gl:create-shader :vertex-shader))
                (fragment-shader (gl:create-shader :fragment-shader))
                (shader-program (gl:create-program)))
            
            (gl:shader-source vertex-shader (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test "vao-stride-vertex-shader.glsl")))
            (gl:compile-shader vertex-shader)
            
            (gl:shader-source fragment-shader (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test "vao-stride-fragment-shader.glsl")))
            (gl:compile-shader fragment-shader)
            
            (gl:attach-shader shader-program vertex-shader)
            (gl:attach-shader shader-program fragment-shader)
            
            (gl:link-program shader-program)
            (gl:use-program shader-program)

            (gl:bind-vertex-array vao)
            (gl:bind-buffer :array-buffer (first buffers))
            (gl:buffer-data :array-buffer :static-draw vertex-attribute-array)
            (gl:free-gl-array vertex-attribute-array)
            
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "position")
                                      2
                                      :float
                                      :false
                                      (* 5 (cffi:foreign-type-size :float))
                                      (cffi:null-pointer))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "input_color")
                                      3
                                      :float
                                      :false
                                      (* 5 (cffi:foreign-type-size :float))
                                      (* 2 (cffi:foreign-type-size :float)))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "input_color"))
            
            (gl:bind-buffer :element-array-buffer (second buffers))
            (gl:buffer-data :element-array-buffer :static-draw element-attribute-array)
            (gl:free-gl-array element-attribute-array)

            (with-event-loop (:method :poll)
              (:idle ()
                     (gl:clear-color 0.0 0.0 0.0 0.0)
                     (gl:clear :color-buffer)
                     (gl:draw-elements :triangle-strip
                                       (gl:make-null-gl-array :unsigned-short)
                                       :count 4)
                     (gl-swap-window my-window))
              (:quit ()
                     (gl:delete-shader vertex-shader)
                     (gl:delete-shader fragment-shader)
                     (gl:delete-program shader-program)
                     (gl:delete-buffers buffers)
                     (gl:delete-vertex-arrays `(,vao))
                     t))))))))
