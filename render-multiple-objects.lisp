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

        (let* ((shader (kit.gl.shader:compile-shader-dictionary 'my-shader))
               (projection-matrix (kit.math:ortho-matrix 0 800 0 600 -1 1))
               (square-vao (let ((new-vao (make-instance 'kit.gl.vao:vao :type 'flat-vao))
                                 (vertex-attribs (make-array 30
                                                             :element-type 'single-float
                                                             :initial-contents '(-0.5 -0.5 1.0 0.0 0.0
                                                                                 0.5 -0.5  0.0 1.0 0.0
                                                                                 0.5  0.5 0.0 0.0 1.0
                                                                                 0.5  0.5 0.0 0.0 1.0
                                                                                 -0.5 0.5 1.0 1.0 0.0
                                                                                 -0.5 -0.5 1.0 0.0 0.0))))
                             (kit.gl.vao:vao-buffer-vector new-vao
                                                           0
                                                           (* 4 (length vertex-attribs))
                                                           vertex-attribs)
                             new-vao))

               (square-translate-scale-matrix (sb-cga:matrix* (sb-cga:translate* 200.0 300.0 0.0)
                                                              (sb-cga:scale* 100.0 100.0 0.0)))
               (triangle-vao (let ((new-vao (make-instance 'kit.gl.vao:vao :type 'flat-vao))
                                   (vertex-attribs (make-array 15
                                                       :element-type 'single-float
                                                       :initial-contents '(-0.5 -0.5 1.0 0.0 0.0
                                                                           0.5 -0.5 0.0 1.0 0.0
                                                                           0.0 0.5 0.0 0.0 1.0))))
                               (kit.gl.vao:vao-buffer-vector new-vao
                                                             0
                                                             (* 4 (length vertex-attribs))
                                                             vertex-attribs)
                               new-vao))
               (triangle-translate-scale-matrix (sb-cga:matrix* (sb-cga:translate* 600.0 300.0 0.0)
                                                                (sb-cga:scale* 50.0 50.0 0.0)))
               (angle-of-rotation 0.0))
          
          (kit.gl.shader:use-program shader :basic-shader)
          
          (with-event-loop (:method :poll)
            (:idle ()
                   (gl:clear-color 0.0 0.0 0.0 0.0)
                   (gl:clear :color-buffer)
                   (kit.gl.shader:uniform-matrix shader
                                                 :model-view-projection-matrix
                                                 4
                                                 (vector (sb-cga:matrix* projection-matrix
                                                                         square-translate-scale-matrix
                                                                         (sb-cga:rotate-around (make-array 3
                                                                                                           :element-type 'single-float
                                                                                                           :adjustable nil
                                                                                                           :fill-pointer nil
                                                                                                           :displaced-to nil
                                                                                                           :initial-contents '(0.0 0.0 1.0))
                                                                                               (kit.math:deg-to-rad angle-of-rotation)))))
                   (kit.gl.vao:vao-draw square-vao
                                        :primitive :triangles
                                        :count 6)
                   (kit.gl.shader:uniform-matrix shader
                                                 :model-view-projection-matrix
                                                 4
                                                 (vector (sb-cga:matrix* projection-matrix
                                                                         triangle-translate-scale-matrix
                                                                         (sb-cga:rotate-around (make-array 3
                                                                                                           :element-type 'single-float
                                                                                                           :adjustable nil
                                                                                                           :fill-pointer nil
                                                                                                           :displaced-to nil
                                                                                                           :initial-contents '(0.0 0.0 1.0))
                                                                                               (kit.math:deg-to-rad (- angle-of-rotation))))))
                   (kit.gl.vao:vao-draw triangle-vao
                                        :primitive :triangles
                                        :count 3)
                   (incf angle-of-rotation)
                   (gl-swap-window window))
            (:quit ()
                   (kit.gl.vao:vao-unbind square-vao)
                   t)))))))
