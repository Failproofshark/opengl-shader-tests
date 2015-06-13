(in-package :asdf-user)
(defsystem "opengl-shader-test"
  :depends-on (:sdl2
               :alexandria
               :cl-opengl
               :sdl2-image)
  :components ((:file "texture")
               (:file "vertex-array-objects")
               (:file "simple-triangle")))
