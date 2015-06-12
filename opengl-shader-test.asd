(in-package :asdf-user)
(defsystem "opengl-shader-test"
  :depends-on (:sdl2
               :alexandria
               :cl-opengl)
  :components ((:file "vertex-array-objects")
               (:file "simple-triangle")))
