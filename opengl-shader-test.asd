(in-package :asdf-user)
(defsystem "opengl-shader-test"
  :depends-on (:sdl2
               :alexandria
               :cl-opengl
               :sdl2-image
               :glkit
               :mathkit
               :sb-cga)
  :components ((:file "texture")
               (:file "vertex-array-objects")
               (:file "simple-rectangle-test")
               (:file "glkittest")
               (:file "render-multiple-objects")
               (:file "split-shader")
               (:file "split-vao")
               (:file "test-entity" :depends-on ("split-vao"))
               (:file "split-render-test" :depends-on ("split-shader"
                                                       "test-entity"))))
