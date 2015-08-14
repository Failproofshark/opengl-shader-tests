(in-package :cl-user)
(defpackage :glkit-test
  (:use :cl
        :sdl2)
  (:export :main))

(in-package :glkit-test)
(require :glkit)
(require :mathkit)

(kit.gl.shader:defdict my-shader (:shader-path (asdf:system-source-directory :opengl-shader-test))
  (kit.gl.shader:program (:basic-shader
            :attrs ((:position 0)))
           (:vertex-shader (:file "vertex.glsl"))
           (:fragment-shader (:file "fragment.glsl"))))

(kit.gl.vao:defvao basic-vao ()
  (:separate ()
             (vertex :float 2)
             (color :float 3)))

(defun main ()
  (with-init (:everything)
    (with-window (window :title "GLKit test" :flags '(:shown :opengl))
      (with-gl-context (gl-context window)
        (gl-make-current window gl-context)
        (gl:viewport 0 0 800 600)

        (let* ((shader-dictionary-object (kit.gl.shader:compile-shader-dictionary 'my-shader))
               (element-array-buffer (first (gl:gen-buffers 1)))
               (element-array (let ((element-array (gl:alloc-gl-array :unsigned-short 4)))
                                (dotimes (i 4)
                                  (setf (gl:glaref element-array i) i))
                                element-array))
               (verticies (make-array 8
                                      :element-type 'single-float
                                      :initial-contents '(-0.5 -0.5   
                                                         0.5 -0.5  
                                                         -0.5  0.5 
                                                         0.5  0.5)))
               (colors (make-array 12
                                   :element-type 'single-float
                                   :initial-contents '(1.0 0.0 0.0 
                                                       0.0 1.0 0.0 
                                                       0.0 0.0 1.0 
                                                       1.0 1.0 1.0)))
               (vao (make-instance 'kit.gl.vao:vao :type 'basic-vao)))
          
          (kit.gl.shader:use-program shader-dictionary-object :basic-shader)
          (gl:bind-buffer :element-array-buffer element-array-buffer)
          
          (gl:buffer-data :element-array-buffer :static-draw element-array)
          (gl:free-gl-array element-array)
          
          (kit.gl.vao:vao-buffer-vector vao
                                        0
                                        (* 4 (length verticies))
                                        verticies)
          
          (kit.gl.vao:vao-buffer-vector vao
                                        1
                                        (* 4 (length colors))
                                        colors)

          (with-event-loop (:method :poll)
            (:idle ()
                   (gl:clear-color 0.0 0.0 0.0 0.0)
                   (gl:clear :color-buffer)
                   (gl:draw-elements :triangle-strip
                                     (gl:make-null-gl-array :unsigned-short)
                                     :count 4)
                   (gl-swap-window window))
            (:quit (kit.gl.vao:vao-unbind vao)
                   (gl:delete-buffers `(,element-array-buffer))
                   t)))))))
