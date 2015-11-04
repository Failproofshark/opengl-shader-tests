(in-package :cl-user)
(defpackage :split-vao
  (:use :cl
        :kit.gl.vao)
  (:export :my-vao))
(in-package :split-vao)
(defvao my-vao ()
  (:separate ()
             (vertex :float 2)
             (color :float 3)))
