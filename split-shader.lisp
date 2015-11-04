(in-package :cl-user)
(defpackage :split-shader
  (:use :cl
        :kit.gl.shader)
  (:export :my-shader))

(in-package :split-shader)


(defdict my-shader (:shader-path (asdf:system-source-directory :opengl-shader-test))
  (program (:basic-shader
            :attrs ((:position 0)
                    (:color 1))
            :uniforms ((:model-view-projection-matrix "model_view_projection_matrix")))
           (:vertex-shader (:file "multi-vs.glsl"))
           (:fragment-shader (:file "multi-fs.glsl"))))
