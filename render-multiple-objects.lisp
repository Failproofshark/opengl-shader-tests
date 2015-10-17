(in-package :cl-user)
(defpackage :render-multiple-objects
  (:use :cl
        :sdl2)
  (:export :main))

(in-package :render-multiple-objects)

(kit.gl.shader:defdict my-shader (:shader-path (asdf:system-source-directory :opengl-shader-test))
  (kit.gl.shader:program (:basic-shader
                          :attrs ((:position 0)
                                  (:color 1))
                          :uniforms ((:model-view-projection-matrix "model_view_projection_matrix")))
                         (:vertex-shader (:file "multi-vs.glsl"))
                         (:fragment-shader (:file "multi-fs.glsl"))))

(kit.gl.vao:defvao flat-vao ()
  (:interleave ()
               (vertex :float 2)
               (color :float 3)))

(defun main ()
  (with-init (:everything)
    (with-window (window :title "Rendering multiple objects" :flags '(:shown :opengl))
      (with-gl-context (gl-context window)
        (gl-make-current window gl-context)
        (gl:viewport 0 0 800 600)

        (flet ((generate-element-array (count)
                 (let ((element-array (gl:alloc-gl-array :unsigned-short 4)))
                                                 (dotimes (i 4)
                                                   (setf (gl:glaref element-array i) i))
                                                 element-array)))
          (let* ((shader (kit.gl.shader:compile-shader-dictionary 'my-shader))
                 (projection-matrix (kit.math:ortho-matrix 0 800 0 600 -1 1))
                 
                 (square-vao (make-instance 'kit.gl.vao:vao :type 'flat-vao))
                 (square-vertex-color-data (make-array 30
                                                       :element-type 'single-float
                                                       :initial-contents '(-0.5 -0.5 1.0 0.0 0.0
                                                                           0.5 -0.5  0.0 1.0 0.0
                                                                           0.5  0.5 0.0 0.0 1.0
                                                                           0.5  0.5 0.0 0.0 1.0
                                                                           -0.5 0.5 1.0 1.0 0.0
                                                                           -0.5 -0.5 1.0 0.0 0.0)))
                 (square-translate-scale-matrix (sb-cga:matrix* (sb-cga:translate* 200.0 300.0 0.0)
                                                                (sb-cga:scale* 50.0 50.0 0.0)))
                 (triangle-vao (make-instance 'kit.gl.vao:vao :type 'flat-vao))
                 (triangle-vertex-color-data (make-array 15
                                                         :element-type 'single-float
                                                         :initial-contents '(-0.5 -0.5 1.0 0.0 0.0
                                                                             0.5 -0.5 0.0 1.0 0.0
                                                                             0.0 0.5 0.0 0.0 1.0)))
                 (triangle-translate-scale-matrix (sb-cga:matrix* (sb-cga:translate* 600.0 300.0 0.0)
                                                                  (sb-cga:scale* 50.0 50.0 0.0))))
            (kit.gl.shader:use-program shader :basic-shader)

            (kit.gl.vao:vao-buffer-vector square-vao
                                          0
                                          (* 4 (length square-vertex-color-data))
                                          square-vertex-color-data)

            (kit.gl.vao:vao-buffer-vector triangle-vao
                                          0
                                          (* 4 (length triangle-vertex-color-data))
                                          triangle-vertex-color-data)

            
            (with-event-loop (:method :poll)
              (:idle ()
                     (gl:clear-color 0.0 0.0 0.0 0.0)
                     (gl:clear :color-buffer)
                     (kit.gl.shader:uniform-matrix shader
                                                   :model-view-projection-matrix
                                                   4
                                                   (vector (sb-cga:matrix* projection-matrix square-translate-scale-matrix)))
                     (kit.gl.vao:vao-draw square-vao
                                          :primitive :triangles
                                          :count 6)
                     (kit.gl.shader:uniform-matrix shader
                                                   :model-view-projection-matrix
                                                   4
                                                   (vector (sb-cga:matrix* projection-matrix triangle-translate-scale-matrix)))
                     (kit.gl.vao:vao-draw triangle-vao
                                          :primitive :triangles
                                          :count 3)
                     (gl-swap-window window))
              (:quit (kit.gl.vao:vao-unbind square-vao)
                     t))))))))
